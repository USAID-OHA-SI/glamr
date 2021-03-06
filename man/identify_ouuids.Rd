% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_outable.R
\name{identify_ouuids}
\alias{identify_ouuids}
\title{Pull OU UIDS}
\usage{
identify_ouuids(username, password, baseurl = "https://final.datim.org/")
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
 ous <- identify_ouuids("userx", mypwd("userx")) }
}
