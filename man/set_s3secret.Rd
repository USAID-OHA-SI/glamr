% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_secrets.R
\name{set_s3secret}
\alias{set_s3secret}
\title{Store S3 Credentials - Secret Access Key}
\usage{
set_s3secret(secret)
}
\arguments{
\item{secret}{S3 account secret key}
}
\value{
stored secret key
}
\description{
`set_s3secret` stores your s3 secret key using the `keyring` package.
This will only need to done once. After running `set_s3secret(secret)`,
RStudio API which will then store the key in your OS credential store using `keyring`.
}
\examples{
\dontrun{
set_s3secret("fsfs8sf0fds9f6s5") }

}
