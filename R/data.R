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
