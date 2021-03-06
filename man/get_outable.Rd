% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_outable.R
\name{get_outable}
\alias{get_outable}
\title{Pull OU UIDS}
\usage{
get_outable(username, password, baseurl = "https://final.datim.org/")
}
\arguments{
\item{username}{DATIM Username}

\item{password}{DATIM password, recommend using `mypwd()`}

\item{baseurl}{base url for the API, default = https://final.datim.org/}
}
\description{
Pull OU UIDS
}
\examples{
\dontrun{
 ou_table <- datim_outable("userx", mypwd("userx")) }
}
