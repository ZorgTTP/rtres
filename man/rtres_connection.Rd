% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connect.R
\name{tres_connect}
\alias{tres_connect}
\title{Connect to the TRES service}
\usage{
tres_connect(
  base_url,
  domain,
  project,
  username,
  password,
  search_image = FALSE,
  salted_encryption = FALSE,
  on_behalf_of_username = NULL,
  on_behalf_of_userguid = NULL,
  channel = NULL,
  keep_invalid = FALSE
)
}
\arguments{
\item{base_url}{String. The base URL of the TRES service.}

\item{domain}{String. The domain to connect to within the TRES service.}

\item{project}{String. The project to connect to within the TRES service.}

\item{username}{String. The username to use for authentication.}

\item{password}{String. The password to use for authentication.}

\item{search_image}{Boolean. Whether to include the search image when encrypting (default: FALSE).}

\item{salted_encryption}{Boolean. Whether to use salted encryption (default: FALSE).}

\item{on_behalf_of_username}{String or NULL. The username to act on behalf of (default: NULL).}

\item{on_behalf_of_userguid}{String or NULL. The user GUID to act on behalf of (default: NULL).}

\item{channel}{String or NULL. The communication channel to use (default: NULL).}

\item{keep_invalid}{Boolean. Whether to keep values that cannot be encrypted or decrypted or return NA (default: FALSE).}
}
\value{
An object of class \code{"rtres_connection"} representing the connection.
}
\description{
This function establishes a connection to the TRES service using the provided credentials and configuration parameters.
}
\examples{
\dontrun{
con <- tres_connect(
  base_url = "https://example.com/tres",
  domain = "example_tres_domain",
  project = "example_tres_project",
  username = "user",
  password = "pass"
)
}
}
