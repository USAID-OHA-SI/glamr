#' @title Clean column data
#'
#' @param .data   MSD Datasets
#' @param colname Name of the column(s)
#' @return  Cleaned MSD DataFrame
#' @family column munging
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
          crayon::red({{colname}}), "\n")

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
      dplyr::mutate_at(.vars = dplyr::all_of(name),
                       stringr::str_remove,
                       pattern = rmv_tail)

    # Remove first 2 leading chars
    rmv_lead <- "^[A-Za-z]{2}[[:space:]]"

    # Remove characters
    .data <- .data %>%
      dplyr::mutate_at(.vars = dplyr::all_of(name),
                       stringr::str_remove,
                       pattern = rmv_lead)
  }


#' @title Clean PSNU column data
#'
#' @param .data   MSD Datasets
#' @return  Cleaned MSD DataFrame
#' @family column munging
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
#' `clean_agency` converts all funding agency names to upper case and removes
#' the HHS prefix for those agencies.
#'
#' @param .data MSD Datasets
#' @return  Cleaned MSD DataFrame
#' @family column munging
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
        fundingagency = stringr::str_to_upper(fundingagency),
        fundingagency = stringr::str_remove(fundingagency, pattern = "^.*\\/")
      )
  }


#' @title Lookup Official Country name
#'
#' @param country country name
#' @return cleaned country name
#' @keywords internal

lookup_country <- function(country) {

  # Symbol [Just for dplyr to deal]
  country <- {{country}}

  # Retrieve the correct name
  country = dplyr::case_when(
    country == "Myanmar" ~ "Burma",
    country == "Czech Republic" ~ "Czechia",
    country == "Ivory Coast" ~ "Cote d'Ivoire",
    country == "Czechia" ~ "Czech Republic",
    country == "Myanmar" ~ "Burma",
    country == "Northern Cyprus" ~ "Cyprus",
    country == "Republic of Serbia" ~ "Serbia",
    country == "Taiwan*" ~ "Taiwan",
    country == "Korea, South" ~ "South Korea",
    country == "Guinea-Bissau" ~ "Guinea Bissau",
    country == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
    country == "DRC" ~ "Democratic Republic of the Congo",
    country == "Congo Democratic Republic" ~ "Democratic Republic of the Congo",
    country == "Congo (Brazzaville)" ~ "Republic of the Congo",
    country == "Republic of Congo" ~ "Republic of the Congo",
    country == "East Timor" ~ "Timor-Leste",
    country == "The Bahamas" ~ "Bahamas",
    country == "United Republic of Tanzania" ~ "Tanzania",
    country == "United States of America" ~ "United States",
    country == "Swaziland" ~ "Eswatini",
    country == "S. Sudan" ~ "South Sudan",
    country == "Saint Kitts and Nevis" ~ "Saint Kitts & Nevis",
    country == "Antigua and Barbuda" ~ "Antigua & Barbuda",
    country == "Trinidad and Tobago" ~ "Trinidad & Tobago",
    country == "Saint Vincent and The Grenadines" ~ "Saint Vincent & the Grenadines",
    TRUE ~ country
  )

  return(country)
}


#' @title Clean OU/Country names to match PEPFAR Data
#'
#' `clean_countries` is used to adjust Natural Earth country names to match
#' PEPFAR ones.
#'
#' @param .data MSD Datasets
#' @param colname Column name to be updated
#' @return  Cleaned DataFrame
#' @family column munging
#' @export
#'
#' @examples
#' \dontrun{
#'  spdf <- ne_countries(type = "sovereignty", scale = 110, returnclass = "sf") %>%
#'    dplyr::select(sovereignt, admin, name, adm0_a3) %>%
#'    glamr::clean_countries(colname = "admin") }

clean_countries <-
  function(.data, colname = "admin") {

    # Check for valid column name
    if (!colname %in% names(.data)) {
      cat("\nERROR - ",
          crayon::red(colname),
          " is not available as a column.\n")

      return(NULL)
    }

    # Notification
    cat("\nUpdating", crayon::green(colname), "column ...\n")

    # Params symbols
    name <- {{colname}}

    #clean column data
    .data %>%
      dplyr::mutate(dplyr::across(.cols = dplyr::all_of(name),
                           .fns = lookup_country))
  }


#' Clean indicators (apply _D suffix)
#'
#' `clean_indicator` applies a '_D' suffix to any indicators that are a
#' denominator. This is particularly useful when aggregating data or
#' reshaping.
#'
#' @param df MSD data frame
#'
#' @return indicators with denominator have _D suffix
#' @family column munging
#' @export
#'
#' @examples
#' \dontrun{
#' df <- df %>%
#' filter(indicator == "TX_PVLS",
#'        standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
#' clean_indicator() }
clean_indicator <- function(df){
  # Check for valid column name
  if (!all(c("indicator", "numeratordenom") %in% names(df))) {
    usethis::ui_warn("ERROR - {usethis::ui_field('indicator')} and/or {usethis::ui_field('numeratordenom')} are not columns in the dataframe. No adjustments to indicator made.")
  } else {
    #add _D to indicators that are denominators
    df <- df %>%
      dplyr::mutate(indicator =
                      dplyr::case_when(indicator %in% c("TX_TB_D_NEG", "TX_TB_D_POS") ~ glue::glue("{indicator}"),
                                       numeratordenom == "D" ~ glue::glue("{indicator}_D"),
                                       TRUE ~ glue::glue("{indicator}")) %>% paste(.))

  }
  return(df)

}

#' Clean Filename
#'
#' This function is primarily useful for removing any apostrophe from the filename
#' since this will get rejected by Google Drive, but also includes features like
#' replacing spaces with an underscore, converting to all lowercase, and adding
#' a date prefix or suffix.
#'
#' @param x filepath or file name
#' @param rm_apostrophe remove all apostrophes, default = TRUE
#' @param rp_space replace spaces with underscore, default = FALSE
#' @param mk_lower make lowercase, default = FALSE
#' @param add_date add date "prefix" or "suffix"
#'
#' @return clean filename
#' @export
#'
#' @examples
#' \dontrun{
#' file <- "Submission_Coted'Ivoire_data.csv"
#' new_file <- clean_filename(file, rm_apostrophe = TRUE, add_date = 'prefix') }
clean_filename <- function(x,
                           rm_apostrophe = TRUE,
                           rp_space = FALSE,
                           mk_lower = FALSE,
                           add_date = NULL){
  if(rm_apostrophe == TRUE)
    x <- stringr::str_remove_all(x, "'")

  if(rp_space == TRUE)
    x <- stringr::str_replace_all(x, " ", "_")

  if(mk_lower == TRUE)
    x <- tolower(x)

  if(add_date == "prefix"){
    x <- stringr::str_replace(x,
                              basename(x),
                              glue::glue('{format(Sys.Date(),"%Y%m%d")}_{basename(x)}'))
  }

  if(add_date == "suffix"){
    x <- stringr::str_replace(x,
                              glue::glue("{tools::file_ext(x)}$"),
                              glue::glue('{format(Sys.Date(),"%Y%m%d")}_{tools::file_ext(x)}'))
  }
}
