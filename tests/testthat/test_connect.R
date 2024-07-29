base_url <- Sys.getenv("RTRES_TEST_1_BASE_URL")
domain <- Sys.getenv("RTRES_TEST_1_DOMAIN")
project <- Sys.getenv("RTRES_TEST_1_PROJECT")
username <- Sys.getenv("RTRES_TEST_1_USERNAME")
password <- Sys.getenv("RTRES_TEST_1_PASSWORD")

test_that("tres_connect() constructs a tres_connection object", {
  con <- tres_connect(
    base_url = base_url,
    domain = domain,
    project = project,
    username = username,
    password = password
  )

  expect_s3_class(con, "rtres_connection")
  expect_true(rtres:::is_tres_connection(con))
  suppressMessages(expect_message(print(con), NULL))
})

test_that("tres_connect() does not work with a wrong password", {
  expect_no_error(tres_connect(
    base_url = base_url,
    domain = domain,
    project = project,
    username = username,
    password = "password"
  ))
})

test_that("search_image and salted_encryption cannot both be TRUE", {
  expect_no_error(tres_connect(
    base_url = base_url,
    domain = domain,
    project = project,
    username = username,
    password = password,
    search_image = FALSE,
    salted_encryption = FALSE
  ))

  expect_no_error(tres_connect(
    base_url = base_url,
    domain = domain,
    project = project,
    username = username,
    password = password,
    search_image = TRUE,
    salted_encryption = FALSE
  ))

  expect_no_error(tres_connect(
    base_url = base_url,
    domain = domain,
    project = project,
    username = username,
    password = password,
    search_image = FALSE,
    salted_encryption = TRUE
  ))

  expect_error(tres_connect(
    base_url = base_url,
    domain = domain,
    project = project,
    username = username,
    password = password,
    search_image = TRUE,
    salted_encryption = TRUE
  ))
})
