test_that("vec_wrap() works", {
  input <- c("Geheim", "", NA)
  expect_equal(input, vec_unwrap(vec_wrap(input)))

  input <- c(" ", "", 42L, 3.1415, TRUE)
  expected_output <- c("_ _", "__", "_42_", "_3.1415_", "_TRUE_")
  expect_equal(vec_wrap(input), expected_output)
})

test_that("vec_jsonify() works", {
  input <- c("Geheim", "", NA)
  expect_equal(input, vec_unjsonify(vec_jsonify(input)))
})

test_that("vec_serialize() works", {
  input <- c("Geheim", "", NA)
  expect_equal(input, vec_unserialize(vec_serialize(input)))
  expect_equal(NULL, vec_unserialize(vec_serialize(NULL)))
})

test_that("extract_search_image() works", {
  input <- c(
    "3::f0a2b963-db9e-4f0b-ba61-577a12500450::1:mGs24EZuJbLmd7wpTivPMg==::1:yggT9t/vw2GfU/hAw4IzyUNWv81lm3NRAPdOrM+BZc3Kk3OYpZvjcTJcsI0oGC1z::K1Z+I9Lp86Njwa1jy7eQ7prYCsZz/8+pLR60wfrNVCI=",
    "3::f0a2b963-db9e-4f0b-ba61-577a12500450::1:mGs24EZuJbLmd7wpTivPMg==::::qqIEyAAdYeARM0RWPRNaDFJY7RP0OD/cgoWSiXLW6NA=",
    NA
  )
  expected_ouput <- c(
    "1:yggT9t/vw2GfU/hAw4IzyUNWv81lm3NRAPdOrM+BZc3Kk3OYpZvjcTJcsI0oGC1z",
    "",
    NA
  )

  expect_equal(vec_extract_search_image(input), expected_ouput)
})
