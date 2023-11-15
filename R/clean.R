#' @title Lookup official Country name
#'
#' @param country country name
#' @param language language to use for lookup
#'
#' @return cleaned country name
#' @export
#'
#' @examples{
#'  cntry <- "Cote d'Ivoire"
#'
#'  name <- lookup_country(cntry)
#'
#'  name
#' }
#'
lookup_country <- function(country, language = "en") {

  # Symbol [Just for dplyr]
  curr_name <- {{country}}

  # Vectorise the lookup process
  curr_name %>%
    purrr::map_chr(function(.x) {

      # Hold targeted country
      curr_name <- .x
      new_name <- curr_name

      # lookup country
      if (language == "en") {
        # Include NE Columns
        df_cntries <- pepfar_country_xwalk %>%
          dplyr::filter(
            base::rowSums(dplyr::across(
              .cols = c(dplyr::ends_with(language),
                        dplyr::all_of(c("sovereignt", "admin", "name"))),
              .fns = ~(. == curr_name))) > 0)
      } else {
        df_cntries <- pepfar_country_xwalk %>%
          dplyr::filter(
            base::rowSums(dplyr::across(
              .cols = dplyr::ends_with(language),
              .fns = ~(. == curr_name))) > 0)
      }

      # make sure result if valid
      if (!base::is.null(df_cntries) & base::nrow(df_cntries) == 1) {

        new_name <- df_cntries %>%
          dplyr::pull(country)

        # notification for new match
        if (curr_name != new_name) {
          base::print(glue::glue("{curr_name} => {new_name}"))
        }
      }

      # Result
      return(new_name)
    })

  # Retrieve the correct name
  # country = dplyr::case_when(
  #   country == "Myanmar" ~ "Burma",
  #   country == "Czech Republic" ~ "Czechia",
  #   country == "Ivory Coast" ~ "Cote d'Ivoire",
  #   country == "Czechia" ~ "Czech Republic",
  #   country == "Myanmar" ~ "Burma",
  #   country == "Northern Cyprus" ~ "Cyprus",
  #   country == "Republic of Serbia" ~ "Serbia",
  #   country == "Taiwan*" ~ "Taiwan",
  #   country == "Korea, South" ~ "South Korea",
  #   country == "Guinea-Bissau" ~ "Guinea Bissau",
  #   country == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
  #   country == "DRC" ~ "Democratic Republic of the Congo",
  #   country == "Congo Democratic Republic" ~ "Democratic Republic of the Congo",
  #   country == "Congo (Brazzaville)" ~ "Republic of the Congo",
  #   country == "Republic of Congo" ~ "Republic of the Congo",
  #   country == "East Timor" ~ "Timor-Leste",
  #   country == "The Bahamas" ~ "Bahamas",
  #   country == "United Republic of Tanzania" ~ "Tanzania",
  #   country == "United States of America" ~ "United States",
  #   country == "Swaziland" ~ "Eswatini",
  #   country == "S. Sudan" ~ "South Sudan",
  #   country == "Saint Kitts and Nevis" ~ "Saint Kitts & Nevis",
  #   country == "Antigua and Barbuda" ~ "Antigua & Barbuda",
  #   country == "Trinidad and Tobago" ~ "Trinidad & Tobago",
  #   country == "Saint Vincent and The Grenadines" ~ "Saint Vincent & the Grenadines",
  #   TRUE ~ country
  # )
}


#' @title Clean Natural Earth Country names to match PEPFAR Data
#'
#' @description `clean_countries` is used to adjust Natural Earth country names to match PEPFAR's Operatingunit / country. This function can also be used to shorten OU/Country names by setting the parameter short to TRUE.
#'
#' @param .data     Reference Datasets
#' @param colname   Column name to be updated
#' @param language  language of reference, default is set to `en`. Options are: `fr`, `de`, `es`, `ar`
#' @param short     If TRUE, shorten OU/Country names instead, default is TRUE
#' @return  Cleaned DataFrame
#' @family  Column clean up
#' @export
#'
#' @examples
#' \dontrun{
#'  library(sf)
#'  library(rnaturalearth)
#'  library(glamr)
#'
#'  spdf <- ne_countries(type = "sovereignty", scale = 110, returnclass = "sf") %>%
#'    sf::st_drop_geometry() %>%
#'    dplyr::select(sovereignt, admin, name, adm0_a3) %>%
#'    glamr::clean_countries(colname = "admin") }
#'
clean_countries <- function(.data,
                            colname = "admin",
                            language = "en",
                            short = TRUE) {

    # Check for valid column name
    if (!colname %in% names(.data)) {
      cat("\nERROR - ",
          crayon::red(base::paste(colname, collapse = ", ")),
          " not available as a column(s).\n")

      return(NULL)
    }

    # Exception - Shorten PEPFAR OU/Country names
    if (base::all(colname %in% c("operatingunit", "country")) && short == TRUE) {
      .data <- .data %>%
        clean_column(colname = colname)

      return(.data)
    }

    # Params symbols
    name <- {{colname}}

    #clean column data
    .data %>%
      dplyr::mutate(dplyr::across(.cols = dplyr::all_of(name),
                           .fns = lookup_country,
                           language = language))
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
