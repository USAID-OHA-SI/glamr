#' @title Clean column data
#'
#' @param .data   MSD Datasets
#' @param colname Name of the column(s)
#' @return  Cleaned MSD DataFrame
#' @export
#' @examples
#' \dontrun{
#'  df_msd %>% clean_column(colname = "psnu") }

clean_column <- function(.data, colname = "psnu") {

    # Check params
    name <- {{colname}}

    # Check for valid column name
    if (length(dplyr::setdiff(name,  names(.data))) > 0) {
      cat("\nERROR - One of the column names is unknown: ",
          Wavelength::paint_red({{colname}}), "\n")

      return(NULL)
    }

    # Remove characters
    rmv_tail <- c(
        "District",
        "County",
        "District Municipality",
        "Metropolitan Municipality",
        "Municipality"
      ) %>%
      paste0(" ", ., "$", collapse = "|")

    # Remove characters at the end
    .data <- .data %>%
      dplyr::mutate_at(.vars = tidyselect::all_of(name),
                       stringr::str_remove,
                       pattern = rmv_tail)

    # Remove first 2 leading chars
    rmv_lead <- "^[A-Za-z]{2}[[:space:]]"

    # Remove characters
    .data <- .data %>%
      dplyr::mutate_at(.vars = tidyselect::all_of(name),
                       stringr::str_remove,
                       pattern = rmv_lead)
  }


#' @title Clean PSNU column data
#'
#' @param .data   MSD Datasets
#' @return  Cleaned MSD DataFrame
#' @export
#' @examples
#' \dontrun{
#'  df_msd %>% clean_psnu() }

clean_psnu <- function(.data) {

    # Check for valid column name
    if (!"psnu" %in% names(.data)) {
      cat("\nERROR - psnu column is not available as a column.\n")
      return(NULL)
    }

    # Remove extract characters
    .data <- .data %>%
      clean_column(colname = "psnu")
  }


#' @title Clean data from fundingagency column
#'
#' @param .data MSD Datasets
#' @return  Cleaned MSD DataFrame
#' @export
#' @examples
#' \dontrun{
#'  df_msd %>% clean_agency() }

clean_agency <- function(.data) {

    # Check for valid column name
    if (!"fundingagency" %in% names(.data)) {
      cat("\nERROR - fundingagency column is not available as a column.\n")
      return(NULL)
    }

    # clean column data
    .data <- .data %>%
      dplyr::mutate(
        # Convert everyting to upper case
        fundingagency = stringr::str_to_upper(fundingagency),
        # Remove any leading char up to /
        fundingagency = stringr::str_remove(fundingagency, pattern = "^.*\\/")
      )
  }
