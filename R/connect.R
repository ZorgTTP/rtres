#' Connect to the TRES service
#'
#' This function establishes a connection to the TRES service using the provided credentials and configuration parameters.
#'
#' @param base_url String. The base URL of the TRES service.
#' @param domain String. The domain to connect to within the TRES service.
#' @param project String. The project to connect to within the TRES service.
#' @param username String. The username to use for authentication.
#' @param password String. The password to use for authentication.
#' @param search_image Boolean. Whether to include the search image when encrypting (default: FALSE).
#' @param salted_encryption Boolean. Whether to use salted encryption (default: FALSE).
#' @param on_behalf_of_username String or NULL. The username to act on behalf of (default: NULL).
#' @param on_behalf_of_userguid String or NULL. The user GUID to act on behalf of (default: NULL).
#' @param channel String or NULL. The communication channel to use (default: NULL).
#' @param keep_invalid Boolean. Whether to keep values that cannot be encrypted or decrypted or return NA (default: FALSE).
#'
#' @return An object of class \code{"rtres_connection"} representing the connection.
#' @examples
#' \dontrun{
#' con <- tres_connect(
#'   base_url = "https://example.com/tres",
#'   domain = "example_tres_domain",
#'   project = "example_tres_project",
#'   username = "user",
#'   password = "pass"
#' )
#' }
#' @rdname rtres_connection
#' @export
tres_connect <- function(base_url,
                         domain,
                         project,
                         username,
                         password,
                         search_image = FALSE,
                         salted_encryption = FALSE,
                         on_behalf_of_username = NULL,
                         on_behalf_of_userguid = NULL,
                         channel = NULL,
                         keep_invalid = FALSE) {
  connection <- structure(
    list(
      base_url = base_url,
      domain = domain,
      project = project,
      username = username,
      password = password,
      search_image = search_image,
      salted_encryption = salted_encryption,
      on_behalf_of_username = on_behalf_of_username,
      on_behalf_of_userguid = on_behalf_of_userguid,
      channel = channel,
      keep_invalid = keep_invalid
    ),
    class = c("rtres_connection")
  )

  rtres:::tres_validate(connection)
}

tres_validate <- function(x) {
  if (x$search_image && x$salted_encryption) {
    cli::cli_abort(
      message = c("x" = "{.field search_image} and {.field salted_encryption} cannot both be {.val TRUE}."),
      call = NULL
    )
  }

  invisible(x)
}

#' @export
print.rtres_connection <- function(x, ...) {
  cli::cli_text("{.cls {class(x)}}")
  cli::cli_ul()
  cli::cli_li("{.field base_url}: {.url {x$base_url}}")
  cli::cli_li("{.field domain}: {x$domain}")
  cli::cli_li("{.field project}: {x$project}")
  cli::cli_li("{.field username}: {x$username}")
  cli::cli_li("{.field password}: *****")
  cli::cli_li("{.field search_image}: {x$search_image}")
  cli::cli_li("{.field salted_encryption}: {x$salted_encryption}")
  cli::cli_li("{.field on_behalf_of_username}: {rtres:::lit_null(x$on_behalf_of_username)}")
  cli::cli_li("{.field on_behalf_of_userguid}: {rtres:::lit_null(x$on_behalf_of_userguid)}")
  cli::cli_li("{.field channel}: {rtres:::lit_null(x$channel)}")
  cli::cli_li("{.field keep_invalid}: {rtres:::lit_null(x$keep_invalid)}")
  cli::cli_end()
  invisible(x)
}

lit_null <- function(x) {
  ifelse(is.null(x), "NULL", x)
}

is_tres_connection <- function(x) {
  inherits(x, "rtres_connection")
}

check_tres_connection <- function(x, arg = caller_arg(connection), call = caller_env()) {
  if (!missing(x) && rtres:::is_tres_connection(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    cli::format_inline("a {.cls rtres_connection} object"),
    allow_null = FALSE,
    arg = arg,
    call = call
  )
}
