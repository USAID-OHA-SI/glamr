#' Resolve Known Issues
#'
#' @description This function handles known issues in the MSD. For
#' example, a mechanism starting mid-year and reporting on TX_CURR
#' will duplicate TX_CURR targets. This function resolves that by
#' removing those known cases and
#'
#' @param df standard MSD data frame, typically after its been filtered
#' @param store_excl should the known exclusions be store in the Global Envir?
#'
#' @return df excluding know targets/results issues
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(tidyverse)
#' library(glamr)
#'
#' load_secrets() # or googlesheets4::gs4_auth()
#'
#' df_msd <- si_path() %>%
#'   return_latest("OU_IM") %>%
#'   read_rds()
#'
#' df_mwi <- df_msd %>%
#'   filter(operatingunit == "Malawi",
#'          indicator == "TX_CURR")
#'
#' df_mwi_resolved <- df_mwi %>%
#'   resolve_knownissues() }

resolve_knownissues <- function(df, store_excl = FALSE){

  #stop if the data aren't long
  if(!all(c("targets", "qtr1", "qtr2", "qtr3", "qtr4", "cumulative") %in% names(df)))
    stop("Need to provide a data frame in the normal MSD structure.")

  #stop if don't have google authentication established
  if(!googlesheets4::gs4_has_token())
    stop("resolve_knownissues() requires OAuth to be set prior to running. Establish authenication for the session using glamr::load_secrets() or googlesheets4::gs4_auth().")

  #pull down known issues from Google Sheets & tidy
  df_issues <- get_knownissues() %>%
    clean_knownissues()

  #set list to exclude from MSD
  df_flag <- flag_knownissues(df, df_issues)

  #replace known issue data to exclude with NAs
  df_adj <- df_flag %>%
    squish_knownissues("targets") %>%
    squish_knownissues("results") %>%
    dplyr::select(-period_type)

  #print out known issues
  df %>%
    note_knownissues(df_issues) %>%
    print_knownissues(store_excl)

  invisible(df_adj)

}


#' Known Data Issues Tracker Google Sheet ID
#'
#' @return google sheet id
#'
gs_id_knownissues <- '1CMPY-GCWP3NSNWvLGLwMwBaPZsobgvjtobhjkZkDfow'

#' Browse Known Issues
#'
#' @description Launch Known Data Issues Tracker
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#'  browse_knowissues() }
browse_knowissues <- function(){
  googlesheets4::gs4_browse(googlesheets4::as_sheets_id(gs_id_knownissues))
}

#' Get Known Issues locally
#'
#' @description load Google Sheet
#'
get_knownissues <- function(){

  #load data from sheet
  suppressMessages(
  df_issues <- googlesheets4::read_sheet(googlesheets4::as_sheets_id(gs_id_knownissues),
                                         sheet = "Known Issues Responses"))

  #rename columns
  df_issues <- df_issues %>%
    dplyr::rename(timestamp = Timestamp,
                  countryname = `What PEPFAR country is affected?`,
                  fiscal_year = `What fiscal year has this issue?`,
                  period_type = `Is this issue related to targets or results?`,
                  period = `Is there a specific quarter affected?`,
                  mech_code = `What is the specific mechanism id?`,
                  mech_pname = `What is the preferred mechanism name?`,
                  indicator = `What indicators are affected?`,
                  action = `How should the data issue be handled?`,
                  description = `Description of the issue`,
                  opu_status = `Is there an OPU open/pending at this time?`,
                  email = `Email Address`)

  return(df_issues)

}


#' Clean Known Issues Google Sheet
#'
#' @param df data from get_knownissues()
#'
#' @return adjust mech_code and tidy by expanding by indicator

clean_knownissues <- function(df){
  #convert mech_code to string to match MSD
  df <- df %>%
    dplyr::mutate(mech_code = as.character(mech_code))

  #set resolved = FALSE if its NA
  df <- df %>%
    dplyr::mutate(resolved = ifelse(is.na(resolved), FALSE, resolved))

  #expand out non-tidy columns
  df_expnd <- df %>%
    tidyr::separate_rows(indicator, sep = ",") %>%
    dplyr::mutate(indicator = stringr::str_trim(indicator))

  return(df_expnd)
}


#' Flag Known Issues
#'
#' @param df original MSD input
#' @param df_issues df output from clean_knownissues()
#'
#' @return df of MSD with extra column of period type to know what to excluded

flag_knownissues <- function(df, df_issues){

  #create df for just exclusion and hasn't been resolved
  df_excl <- df_issues %>%
    dplyr::filter(action == "exclude",
                  resolved == FALSE) %>%
    dplyr::select(mech_code, fiscal_year, indicator, period_type)

  #join to main df
  df_join <- df %>%
    dplyr::left_join(df_excl, by = c("mech_code", "fiscal_year", "indicator"))

  return(df_join)
}


#' Squish Known Issues
#'
#' @param df df output from flag_knownissues()
#' @param type period type, either targets (default) or results
#'
#' @return dataframe with known issues data removed

squish_knownissues <- function(df, type = "targets"){

  if(!"period_type" %in% names(df))
    df <- dplyr::mutate(df, period_type = "none")

  #convert NAs to none for use of negative filter
  df <- dplyr::mutate(df, period_type = ifelse(is.na(period_type), "none", period_type))

  if({type} %in% unique(df$period_type)){
    #columns to adjust
    cols <- dplyr::case_when(type == "targets" ~ "targets",
                             type == "results" ~ c("qtr1", "qtr2", "qtr3", "qtr4", "cumulative"))

    #replace known results issues with NA
    df_adj <- df %>%
      dplyr::filter(period_type == {type}) %>%
      dplyr::mutate(dplyr::across(all_of(cols), ~ NA))

    #remove issue rows and bind back on the NA values
    df <- df %>%
      dplyr::filter(period_type != {type}) %>%
      dplyr::bind_rows(df_adj) %>%
      dplyr::mutate(period_type = dplyr::na_if(period_type, "none"))
  }

  return(df)
}


#' Note Known Issues
#'
#' @param df_orig MSD dataset
#' @param df_allissues df from clean_knownissues()
#'
#' @return df that has a description of what was excluded in orig df

note_knownissues <- function(df_orig, df_allissues){

  #limit to relevent field for messaging
  df_issues_type <- df_allissues %>%
    dplyr::select(action, mech_code, fiscal_year, indicator, period_type, description)

  #note all the changes being made
  df_issues_matches <- df_issues_type %>%
    dplyr::inner_join(df_orig, by = c("mech_code", "indicator", "fiscal_year")) %>%
    dplyr::distinct(action, countryname, mech_code, fiscal_year, period_type, description, indicator) %>%
    dplyr::group_by(action, countryname, mech_code, fiscal_year, period_type, description) %>%
    dplyr::summarise(indicator = paste(indicator, collapse=", "), .groups = "drop") %>%
    dplyr::mutate(msg = ifelse(action == "exclude",
                               glue::glue("Excluded {countryname} {mech_code} FY{stringr::str_sub(fiscal_year, -2)} {period_type} for {indicator}"),
                               glue::glue("Extra info provided for {countryname} {mech_code} FY{stringr::str_sub(fiscal_year, -2)} {period_type} for {indicator}: {description}")))

  return(df_issues_matches)
}

#' Print Known Issues to console
#'
#' @param df data frame from note_knownissues()
#' @param store_excl whether to store exclusions to Global Envir
#'
#' @return console print of known issues

print_knownissues <- function(df, store_excl = FALSE){

  #list of excluded data
  lst_excl_matches <- df %>%
    dplyr::filter(action == "exclude") %>%
    dplyr::pull()

  #list of info only
  lst_info_matches <- df %>%
    dplyr::filter(action == "info only") %>%
    dplyr::pull()

  #print what is excluded
  purrr::walk(lst_excl_matches, usethis::ui_done)

  #if nothing is excluded, print nothing is excluded
  if(length(lst_excl_matches) == 0)
    lst_excl_matches <- "Given your dataset, no documented issues to exlcude"

  #print info
  purrr::walk(lst_info_matches, usethis::ui_info)

  #store in global envir if desired
  if(store_excl == TRUE)
    lst_excl_matches <<- lst_excl_matches

}
