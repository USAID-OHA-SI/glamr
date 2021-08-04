#' Current PEPFAR Data Calendar
#'
#' A dataset the dates for the release of the MER Structured Dataset
#'
#' @format A data frame with 8 rows and 5 variables:
#' \describe{
#'   \item{fiscal_year}{fiscal year, start = October}
#'   \item{quarter}{fiscal quarter, integer 1-4}
#'   \item{type}{data release type, initial or clean}
#'   \item{entry_open}{date entry begins into DATIM}
#'   \item{entry_close}{date DATIM is closed and data are frozen}
#' }
#' @source \url{https://datim.zendesk.com/hc/en-us/articles/115001940503-PEPFAR-Data-Calendar}
"pepfar_data_calendar"


#' Current List of PEPFAR OUs/Countries and their ISO codes
#'
#' A dataset PEPFAR Operating Units and Countries along with their ISO codes.
#' This is a useful dataset for having a full set of PEPFAR countries or to
#' align ISO codes with external datasources. Pulled from DATIM and the MSD.
#'
#' @format A data frame with 51 rows and 4 variables:
#' \describe{
#'   \item{operatingunit}{PEPFAR Operating Unit (countries + 3 regional programs)}
#'   \item{operatingunit_iso}{PEPFAR Operating Unit ISO-3}
#'   \item{countryname}{PEPFAR Country Name}
#'   \item{countryname_iso}{PEPFAR Country Name ISO-3}
#' }
#' @source \url{https://final.datim.org/}
"pepfar_country_list"
