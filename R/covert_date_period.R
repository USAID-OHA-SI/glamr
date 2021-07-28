#' Convert Date to FY Quarter/Period
#'
#' @param date date formatted like 2021-10-01
#'
#' @return vector of FY period, eg FY22Q1
#' @export
#'
#' @examples
#' \dontrun{
#' dates <- c("2021-10-01", "2021-11-15")
#' convert_date_to_qtr(dates) }
#'
convert_date_to_qtr <- function(date){
  pd <- lubridate::quarter(date,
                           with_year = TRUE,
                           fiscal_start = 10)

  pd <- stringr::str_replace(pd, "20", "FY")

  pd <- stringr::str_replace(pd, "\\.", "Q")

  return(pd)
}


#' Convert FY Quarter/Period to Date
#'
#' @description
#' Convert a period (from reshape_msd()) in the format of FY22Q1 or FY22 (for
#' targets/cumulative).
#'
#' @param period period formated like FY22Q1 or FY22
#' @param type start or end date of quarter/period, default = "start"
#'
#' @return date vector
#' @export
#'
#' @examples
#' \dontrun{
#' df <- read_msd(path)
#' df <- df %>%
#'   filter(df,
#'          operatingunit == "Jupiter",
#'          indicator == "TX_NEW",
#'          standarddisaggregate == "Total Numerator") %>%
#'   group_by(fiscal_year, primepartner) %>%
#'   summarize(across(start_with("qtr"), sum, na.rm = TRUE)) %>%
#'   ungroup()
#' df <- df %>%
#'   reshape_msd() %>%
#'   mutate(date = convert_qtr_to_date(period), .after = period) }
#'
convert_qtr_to_date <- function(period, type = "start"){

  if(stringr::str_detect(period, "(FY[:digit:]{2}Q[:digit:]{1}|FY[:digit:]{2})", negate = TRUE))
    stop("Expecting a period to be formatted like FY22Q1 or FY22")

  if(stringr::str_detect(period, "Q", negate = TRUE)){
    q <- ifelse(type == "start", 1, 4)
    period <- glue::glue("{period}Q{q}")
  }

  period <- stringr::str_remove(period, "FY")

  cy_date <- lubridate::yq(period)

  if(type == "start"){
    date <- cy_date - months(3)
  } else {
    date <- cy_date - 1
  }

  return(date)

}
