con1 <- local_connect(profile = "1")
con2 <- local_connect(profile = "2")
con3 <- local_connect(profile = "3")
con4 <- local_connect(profile = "4")
con5 <- local_connect(profile = "5")

test_that("User 1 can encrypt; User 1 can decrypt", {
  input <- c("A", "B")
  output <- tres_decrypt(tres_encrypt(input, con1), con1)
  expect_equal(input, output)
})

test_that("User 1 can encrypt; User 2 can decrypt", {
  input <- c("A", "B")
  output <- tres_decrypt(tres_encrypt(input, con1), con2)
  expect_equal(input, output)
})

test_that("User 1 can encrypt; User 3 cannot decrypt", {
  input <- c("A")
  expect_warning(
    output <- tres_decrypt(tres_encrypt(input, con1), con3),
    regexp = "E303"
  )
  expect_equal(output, as.character(c(NA)))

  input <- c("A")
  expect_warning(
    output <- tres_decrypt(
      c(
        tres_encrypt(input, con1),
        tres_encrypt(input, con3)
      ),
      con3
    ),
    regexp = "E303"
  )
  expect_equal(output, as.character(c(NA, "A")))
})

test_that("User 4 is not approved", {
  input <- c("A")
  expect_error(output <- tres_encrypt(input, con4), regexp = "not enabled.*E200")
})

test_that("User 5 has expired password", {
  input <- c("A")
  expect_error(output <- tres_encrypt(input, con5), regexp = "expired.*E999")
})
