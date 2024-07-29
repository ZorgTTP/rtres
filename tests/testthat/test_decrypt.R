test_that("tres_decrypt() works with one empty value", {
  con <- local_connect()
  output <- suppressWarnings(tres_decrypt(NA, con))
  expect_equal(output, as.character(NA))
})

test_that("tres_decrypt() and tres_encrypt() work together", {
  con <- local_connect()
  input <- c("A", " ")

  # With keep_invalid = FALSE (which is the default)
  suppressWarnings(expect_equal(tres_decrypt(tres_encrypt(input, con), con), c("A", NA)))

  # With keep_invalid = TRUE (as argument)
  suppressWarnings(expect_equal(tres_decrypt(tres_encrypt(input, con, keep_invalid = TRUE), con, keep_invalid = TRUE), c("A", " ")))

  # With keep_invalid = TRUE (as connection setting)
  con$keep_invalid <- TRUE
  suppressWarnings(expect_equal(tres_decrypt(tres_encrypt(input, con), con), c("A", " ")))
  con$keep_invalid <- FALSE

  # Using vec_wrap() and vec_unwrap()
  suppressWarnings(expect_equal(vec_unwrap(tres_decrypt(tres_encrypt(vec_wrap(input), con, keep_invalid = TRUE), con, keep_invalid = TRUE)), c("A", " ")))

  # Using vec_jsonify() and vec_unjsonify()
  suppressWarnings(expect_equal(vec_unjsonify(tres_decrypt(tres_encrypt(vec_jsonify(input), con, keep_invalid = TRUE), con, keep_invalid = TRUE)), c("A", " ")))

  # Using vec_serialize() and vec_unserialize()
  suppressWarnings(expect_equal(vec_unserialize(tres_decrypt(tres_encrypt(vec_serialize(input), con, keep_invalid = TRUE), con, keep_invalid = TRUE)), c("A", " ")))
})
