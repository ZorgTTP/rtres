#' @keywords internal
separator <- "#"

#' @keywords internal
parameter_mapping <- list(
  password = "pw",
  domain = "dn",
  project = "pn",
  username = "un",
  password = "pw",
  search_image = "si",
  salted_encryption = "se",
  on_behalf_of_username = "oboun",
  on_behalf_of_userguid = "obogu"
)

# The following response codes cause an immediate abort
# Other response codes are displayed as warnings
#' @keywords internal
error_codes <- c(100, 200, 313, 314, 999)

#' Encrypt and Decrypt Values using TRES Service
#'
#' These functions encrypt and decrypt the provided values using the TRES service.
#'
#' @param values Character vector. Values to be encrypted or decrypted.
#' @param connection An [rtres_connection].
#' @param chunk_size Integer. The number of values to process in each chunk (default: 1000). Disable chunking with 0.
#' @param keep_invalid Boolean or NULL. Whether to keep values that cannot be encrypted or decrypted or return NA (default: NULL, meaning the setting in the connection object is used).
#' @param ... Additional parameters to temporarily override any settings in the connection object, such as \code{search_image}.
#'
#' @return A character vector containing the encrypted or decrypted values.
#' @examples
#' con <- rtres:::local_connect()
#'
#' # Normal input
#' input <- c("One", "Two")
#' (encrypted_input <- tres_encrypt(input, con))
#'
#' tres_decrypt(encrypted_input, con)
#'
#' # Some invalid input with keep_invalid = FALSE (default)
#' input <- c("One", " ", NA)
#' (encrypted_input <- tres_encrypt(input, con))
#'
#' tres_decrypt(encrypted_input, con)
#'
#' # Same invalid input with keep_invalid = TRUE
#' (encrypted_input <- tres_encrypt(input, con, keep_invalid = TRUE))
#'
#' tres_decrypt(encrypted_input, con, keep_invalid = TRUE)
#'
#' @rdname tres_request
#' @seealso \code{\link{tres_connect}}
#' @export
tres_encrypt <- function(values, connection, chunk_size = 1000,
                         keep_invalid = NULL, ...) {
  rtres:::tres_request_chunked(
    values = values,
    connection = connection,
    end_point = "encryptpost",
    chunk_size = chunk_size,
    keep_invalid = keep_invalid,
    ...
  )
}

#' @rdname tres_request
#' @export
tres_decrypt <- function(values, connection, chunk_size = 1000,
                         keep_invalid = NULL, ...) {
  rtres:::tres_request_chunked(
    values = values,
    connection = connection,
    end_point = "decryptpost",
    chunk_size = chunk_size,
    keep_invalid = keep_invalid,
    ...
  )
}

tres_request <- function(values, connection, end_point, ...) {
  text <- rtres:::str_join(values)
  body <- rtres:::build_request_body(connection, text, ...)

  req <- httr2::request(connection$base_url)
  req <- httr2::req_url_path_append(req, "Service")
  req <- httr2::req_url_path_append(req, end_point)
  req <- httr2::req_method(req, "POST")
  req <- httr2::req_headers(req, Accept = "application/json")
  req <- httr2::req_body_form(req, !!!body)

  httr2::resp_body_json(httr2::req_perform(req))
}

tres_request_chunked <- function(values, connection, end_point,
                                 chunk_size, keep_invalid = NULL, ...) {
  rtres:::check_tres_connection(connection)

  if (chunk_size > 0) {
    chunk_ids <- ceiling(seq_along(values) / chunk_size)
    chunk_starts <- seq(0, length(values) - 1, chunk_size)

    chunked_input <- split(values, chunk_ids)

    withCallingHandlers(
      chunked_output <- purrr::map2(
        chunked_input, chunk_starts,
        function(values, start) {
          response <- rtres:::tres_request(
            values = values,
            connection = connection,
            end_point = end_point,
            ...
          )
          rtres:::extract_values_from_response(
            response = response,
            start = start,
            end_point = end_point
          )
        }
      ),
      purrr_error_indexed = function(err) {
        rlang::cnd_signal(err$parent)
      }
    )

    output <- unsplit(chunked_output, chunk_ids)
  } else {
    response <- rtres:::tres_request(values, connection, end_point, ...)
    output <- rtres:::extract_values_from_response(response, end_point = end_point)
  }

  keep_invalid <- ifelse(is.null(keep_invalid), connection$keep_invalid, keep_invalid)
  if (keep_invalid) {
    invalid <- is.na(output)
    output[invalid] <- values[invalid]
  }

  output
}

build_request_body <- function(connection, text, ...) {
  body <- list(
    text = text,
    domain = connection$domain,
    project = connection$project,
    search_image = connection$search_image,
    salted_encryption = connection$salted_encryption,
    username = connection$username,
    password = connection$password,
    separator = rtres:::separator
  )

  extra_body <- list(...)
  body <- utils::modifyList(body, extra_body)

  rtres:::tres_validate(body)
  rtres:::rename_named_list(body, rtres:::parameter_mapping)
}

rename_named_list <- function(x, y) {
  # Loop through each element in list x
  for (i in seq_along(x)) {
    old_name <- names(x)[i]
    # Check if the old name exists in list y
    if (old_name %in% names(y)) {
      # Update the name in list x
      names(x)[i] <- y[[old_name]]
    }
  }
  x
}

str_join <- function(x) {
  stringr::str_c(stringr::str_replace_na(x, ""), collapse = rtres:::separator)
}

str_split <- function(x) {
  stringr:::str_split_1(x, rtres:::separator)
}

extract_values_from_response <- function(response, start = 0, end_point = "encryptpost") {
  codes <- as.numeric(rtres:::str_split(response$ResponseCode))
  values <- rtres:::str_split(response$ResponseMessage)

  if (all(codes == 0)) {
    return(values)
  }

  # The following check is necessary to avoid an edge case where TRES errors about a missing parameter.
  # This would break, for example, for values = c("a", "b", NA) and chunk_size = 2.
  if ((length(codes) == 1) && (codes[1] == 999) && (values[1] == "Value cannot be null.\r\nParameter name: text")) {
    if (end_point == "encryptpost") {
      values[1] <- "The plain text that should be encrypted was not specified."
      codes[1] <- 310
    } else {
      values[1] <- "Encrypted value is not valid."
      codes[1] <- 316
    }
  }

  # Print warnings and errors for every non-zero code
  for (i in seq_along(codes)[codes > 0]) {
    msg <- paste0("[", i + start, "] ", values[i], " (E", codes[i], ")")

    if (codes[i] %in% rtres:::error_codes) {
      cli::cli_abort(message = c("x" = msg), call = NULL)
    } else {
      cli::cli_warn(message = c("i" = msg), call = NULL)
    }
  }

  # Replace invalid values with NA
  values[codes > 0] <- NA
  values
}
