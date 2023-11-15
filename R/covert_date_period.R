#' Convert Date to FY Quarter/Period
#'
#' @param date date formatted like 2021-10-01
#'
#' @return vector of FY period, eg FY22Q1
#' @export
#'
#' @family period
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
#' @family period
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


#' Convert DATIM CY Quarter/Period to FY Quarter
#'
#' Convert a period a DATIM API in the format of FY22Q1 or FY22 (for
#' targets/cumulative). This function is built into `extract_datim`.
#'
#' @param df dataframe from \code{extract_datim()}
#'
#' @return Convert periods from long CY dates to PEPFAR standard FY
#' @export
#' @seealso [set_datim()] to store DATIM authentication;
#' [load_secrets()] to store DATIM authentication;
#' [extract_datim()] to run API (which incorporates convert_datim_pd_to_qtr)
#'
#' @family period
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(Periods = c("Jan to Mar 2019", "Oct 2018 to Sep 2019"))
#' df <- convert_datim_pd_to_qtr(df) }

convert_datim_pd_to_qtr <- function(df){

  if("Period" %in% names(df)){
    suppressWarnings(
      df <- df %>%
        dplyr::mutate(Period = dplyr::case_when(
          stringr::str_detect(Period, "^[:alpha:]{3} to")
          ~ Period %>%
            stringr::str_replace(" to [:alpha:]{3}", "1,") %>%
            lubridate::mdy() %>%
            lubridate::quarter(with_year = TRUE, fiscal_start = 10) %>%
            as.character() %>%
            stringr::str_replace("20", "FY") %>%
            stringr::str_replace("\\.", "Q"),
          stringr::str_detect(Period, "^Oct [:digit:]{4}")
          ~ Period %>%
            stringr::str_extract(("(?<=20)[:digit:]{2}$"))%>%
            paste0("FY", .),
          TRUE ~ Period))
    )
  }

  return(df)

}


#' Convert Fiscal Year and Quarter into Period
#'
#' Using `gophr::reshape_msd()` often creates the a perfable long dataset when
#' working with the MSD, but may restrict the user to certain default during the
#' process. Creating a clean period  (eg FY22Q1) requires a number of lines of
#' code to get right, so this function provides stopgap when you are working
#' with a long dataset that has a fiscal year and quarter column and desire a
#' period variable.
#'
#' @param df MSD data frame reshaped long, eg `pivot_longer`
#' @param fy_ind indicator name in df for the fiscal year, default = "fiscal_year"
#' @param qtr_ind indicator name in the df for quarters, default = "qtrs"
#'
#' @return united period column combining and cleaning fiscal year and quarter
#' @export
#'
#' @family period
#'
#' @examplesIf FALSE
#'
#' df_summary <- df_msd %>%
#'        filter(indicator == "TX_CURR",
#'               standardizeddisaggregate == "Total Numerator",
#'               operatingunit == "Jupiter") %>%
#'        group_by(mech_code, fiscal_year) %>%
#'        summarise(across(starts_with("qtr"), sum, na.rm = TRUE),
#'                  .groups = "drop")
#'
#' df_summary <- df_summary %>%
#'        pivot_longer(-c(mech_code, fiscal_year), names_to = "qtrs")
#'
#' df_summary <- convert_fy_qtr_to_pd(df_summary)

convert_fy_qtr_to_pd <- function(df, fy_ind = "fiscal_year", qtr_ind = "qtr"){

  if(!fy_ind %in% names(df))
    usethis::ui_stop("Cannot find '{fy_ind}' in data frame provided.")

  if(!qtr_ind %in% names(df))
    usethis::ui_stop("Cannot find '{qtr_ind}' in data frame provided.")

  #clean up period
  df <- df %>%
    tidyr::unite(period, c(!!fy_ind, !!qtr_ind), sep = "") %>%
    dplyr::mutate(period = period %>%
                    stringr::str_remove("^20(?=[:digit:{2}])") %>%
                    stringr::str_remove("(targets|cumulative|tr)") %>%
                      toupper %>% paste0("FY", .))

  return(df)

}
