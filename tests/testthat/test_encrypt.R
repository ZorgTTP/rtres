test_that("tres_encrypt() doesn't work without an <rtres_connection> object", {
  expect_error(tres_encrypt("Geheim"))
  expect_error(tres_encrypt("Geheim", FALSE))
})

test_that("tres_encrypt() works with normal character vectors", {
  con <- local_connect()
  input <- c("Strict", "Geheim", "Toch")
  expected_output <- c(
    "3::f752737e-57f2-4ed0-93e9-600982607393::1:CmKe4Oo9f9JIjnAFMTg/WQ==::::eSA4KQdYIGk4R9Jmur+3jiGylpUTSi6U4Jk7pSFPjBY=",
    "3::f752737e-57f2-4ed0-93e9-600982607393::1:JbCORkxHBVKA10sfTJOWfQ==::::36g60qAFUHOwLzmlsBBvz7ZvH6pXtngJZkHLUMzHmCI=",
    "3::f752737e-57f2-4ed0-93e9-600982607393::1:fMIadbRobicXUZdkvKpR0w==::::U8Hyf2dvqw9gDwzna/Dcc/P627m/I/dIEWFX6MzE2Yc="
  )

  expect_equal(tres_encrypt(input, con), expected_output)
  expect_equal(tres_encrypt(input, con, chunk_size = 3), expected_output)
  expect_equal(tres_encrypt(input, con, chunk_size = 2), expected_output)
  expect_equal(tres_encrypt(input, con, chunk_size = 1), expected_output)
  expect_equal(tres_encrypt(input, con, chunk_size = 0), expected_output)
})

test_that("tres_encrypt() works with search_image = TRUE", {
  con <- local_connect()
  input <- c("Strict", "Geheim", "Toch")
  expected_output <- c(
    "3::f752737e-57f2-4ed0-93e9-600982607393::1:CmKe4Oo9f9JIjnAFMTg/WQ==::1:Sxd/hU/tF270+0iteyH8UkO0CotkFAWKbA1qDcDZ2j3J52CLyyL48Hsp/puZUtfl::3oppa6wX57Q6Ls+JrWiXoByWRfickBLYfGKIHouMuo8=",
    "3::f752737e-57f2-4ed0-93e9-600982607393::1:JbCORkxHBVKA10sfTJOWfQ==::1:XIdxNifNkRI5qoqFiBwBeBEvBSTXTqjeT+FXM8lmNGB2PDONxTpHCL4hHF3ozWBh::9q/e3oSvR685vBQOAYybprdIZI9gXQ9CQG7+xgVpU3Y=",
    "3::f752737e-57f2-4ed0-93e9-600982607393::1:fMIadbRobicXUZdkvKpR0w==::1:sAwUJJuQORrBoZDHSqqBE3STeBlOcqLhJxFOXe4oTYodd2kQ6RO5xcbTiYyB0E8x::ZLoCmF0H0eyptLDlSxneL8TYEmaZGkQGTpC+o/H0cEM="
  )

  expect_equal(tres_encrypt(input, con, search_image = TRUE), expected_output)
})

test_that("tres_encrypt() works with salted_encryption = TRUE", {
  con <- local_connect()
  input <- c("Strict", "Geheim", "Toch")

  output <- tres_encrypt(input, con, salted_encryption = TRUE)
  expect_true(all(stringr::str_starts(output, "3::f752737e-57f2-4ed0-93e9-600982607393::2:")))
})

test_that("tres_encrypt() doesn't work with search_image = TRUE and salted_encryption = TRUE", {
  con <- local_connect()
  expect_error(tres_encrypt(input, con, salted_encryption = TRUE, search_image = TRUE))
})

test_that("tres_encrypt() works with keep_invalid = FALSE", {
  con <- local_connect()
  input <- c("Strict", " ", NA)
  expected_output <- c("3::f752737e-57f2-4ed0-93e9-600982607393::1:CmKe4Oo9f9JIjnAFMTg/WQ==::::eSA4KQdYIGk4R9Jmur+3jiGylpUTSi6U4Jk7pSFPjBY=", NA, NA)
  suppressWarnings(output <- tres_encrypt(input, con, keep_invalid = FALSE))
  expect_equal(output, expected_output)
})

test_that("tres_encrypt() works with keep_invalid = TRUE", {
  con <- local_connect()
  input <- c("Strict", " ", NA)
  expected_output <- c("3::f752737e-57f2-4ed0-93e9-600982607393::1:CmKe4Oo9f9JIjnAFMTg/WQ==::::eSA4KQdYIGk4R9Jmur+3jiGylpUTSi6U4Jk7pSFPjBY=", " ", NA)
  suppressWarnings(output <- tres_encrypt(input, con, keep_invalid = TRUE))
  expect_equal(output, expected_output)
})

test_that("tres_encrypt() works with one empty value", {
  con <- local_connect()
  output <- suppressWarnings(tres_encrypt(c(""), con))
  expect_equal(output, as.character(c(NA)))
})
