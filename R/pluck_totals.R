#' Filter for Only Totals in MSD
#'
#' Often times we want just want to work with totals when performing an analysis
#' or visualization. This function is a simple filter for Total Numerator and
#' Denominator in a MSD dataframe
#'
#' @param df MER Structured Dataset (MSD) dataframe
#'
#' @return MSD with only total numerator and denominator
#' @export
#'
#' @examplesIf FALSE
#' df_msd <- read_msd(path)
#' df_totals <- df_msd %>%
#'   pluck_totals() %>%
#'   clean_indicator() %>%
#'   filter(indicator %in% cascade_ind)
#'
pluck_totals <- function(df){

  if(!"standardizeddisaggregate" %in% names(df))
    stop("The provided dataframe is missing `standardizeddisaggregate`.")

  df_totals <- df %>%
    dplyr::filter(standardizeddisaggregate %in%
                    c("Total Numerator", "Total Denominator"))

  if(nrow(df_totals) == 0)
    usethis::ui_warn("No total numerator or denominators in data frame.")

  return(df_totals)
}
