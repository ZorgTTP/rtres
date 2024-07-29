#' Preprocess Input and Postprocess Output
#'
#' These utility functions are used to preprocess input to the `tres_encrypt()` function and postprocess the output from the `tres_decrypt()` function.
#'
#' * `vec_wrap()` surrounds each value with a character (and `vec_unwrap()` reverses that)
#' * `vec_jsonify()` serializes each value to JSON (and `vec_unjsonify()` reverses that)
#' * `vec_serialize()` serializes each value along with the R object metadata to JSON (and `vec_unserialize()` reverses that)
#' * `vec_extract_search_image()` extracts from each encrypted value the search image
#'
#' @param values A vector of values to be processed.
#' @param with String. The string to wrap or unwrap the values with (default: "_").
#'
#' @return A vector of processed values.
#'
#' @examples
#' strings <- c("One", "", " ", NA)
#'
#' vec_wrap(strings)
#'
#' vec_wrap(strings) |> vec_unwrap()
#'
#' vec_jsonify(strings)
#'
#' vec_jsonify(strings) |> vec_unjsonify()
#'
#' vec_serialize(strings)
#'
#' vec_serialize(strings) |> vec_unserialize()
#'
#' numbers <- c(3.1415, 42, 1337)
#'
#' vec_jsonify(numbers)
#'
#' vec_serialize(numbers)
#'
#' datetimes <- c(lubridate::now(), lubridate::now() + lubridate::hours(1))
#'
#' vec_jsonify(datetimes)
#'
#' vec_jsonify(datetimes) |> vec_unjsonify()
#'
#' vec_serialize(datetimes)
#'
#' vec_serialize(datetimes) |> vec_unserialize()
#'
#' input <- c(
#'   "3::f0a2b963-db9e-4f0b-ba61-577a12500450::1:mGs24EZuJbLmd7wpTivPMg==::1:yggT9t/vw2GfU/hAw4IzyUNWv81lm3NRAPdOrM+BZc3Kk3OYpZvjcTJcsI0oGC1z::K1Z+I9Lp86Njwa1jy7eQ7prYCsZz/8+pLR60wfrNVCI=",
#'   "3::f0a2b963-db9e-4f0b-ba61-577a12500450::1:mGs24EZuJbLmd7wpTivPMg==::::qqIEyAAdYeARM0RWPRNaDFJY7RP0OD/cgoWSiXLW6NA=",
#'   NA
#' )
#'
#' vec_extract_search_image(input)
#' @name utils
NULL


#' @rdname utils
#' @export
vec_wrap <- function(values, with = "_") {
  stringr::str_c(with, values, with)
}

#' @rdname utils
#' @export
vec_unwrap <- function(values, with = "_") {
  stringr::str_extract(values, paste0("^", with, "(.*)", with, "$"), group = 1)
}

#' @rdname utils
#' @export
vec_jsonify <- function(values) {
  purrr::map_chr(values, jsonlite::toJSON)
}

#' @rdname utils
#' @export
vec_unjsonify <- function(values) {
  purrr::map_vec(values, jsonlite::fromJSON)
}

#' @rdname utils
#' @export
vec_serialize <- function(values) {
  purrr::map_chr(values, jsonlite::serializeJSON)
}

#' @rdname utils
#' @export
vec_unserialize <- function(values) {
  purrr::map_vec(values, jsonlite::unserializeJSON)
}

#' @rdname utils
#' @export
vec_extract_search_image <- function(values) {
  stringr::str_split_i(values, "::", 4)
}
