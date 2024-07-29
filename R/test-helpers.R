local_connect <- function(profile = "1") {
  tres_connect(
    domain = Sys.getenv(paste0("RTRES_TEST_", profile, "_DOMAIN")),
    project = Sys.getenv(paste0("RTRES_TEST_", profile, "_PROJECT")),
    base_url = Sys.getenv(paste0("RTRES_TEST_", profile, "_BASE_URL")),
    username = Sys.getenv(paste0("RTRES_TEST_", profile, "_USERNAME")),
    password = Sys.getenv(paste0("RTRES_TEST_", profile, "_PASSWORD")),
    salted_encryption = FALSE,
    search_image = FALSE
  )
}
