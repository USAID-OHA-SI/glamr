#' Resolve Known Issues
#'
#' @description This function handles known issues in the MSD. For
#' example, a mechanism starting mid-year and reporting on TX_CURR
#' will duplicate TX_CURR targets. This function resolves that by
#' removing those known cases and stores cases relevant to your data frame.
#'
#' The best workflow is to filter your dataset down to a country and/or
#' technical area of interest before running `resolve_knownissues()`. When you
#' run the function, it will print out any known issues to the console (and can)
#' even store them to your Global Environment, so it makes sense to limit the
#' data first to what you are using/care about.
#'
#' The list of known issues is maintained by USAID/SIEI division. To access the
#' table requires having a USAID email account and can be accessed via
#' `browse_knownissues()`.
#'
#' @param df standard MSD data frame, typically after its been filtered
#' @param remove_cs remove data flagged as central support (CS), default = TRUE
#' @param store_excl should the known exclusions be store in the Global Envir?
#'
#' @return df excluding known targets/results issues
#' @seealso [browse_knownissues()] to view table in Google Sheets;
#'  [set_email()] to store USAID email;
#'  [load_secrets()] to load credentials into session.
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

resolve_knownissues <- function(df, remove_cs = TRUE, store_excl = FALSE){

  #stop if the data aren't in original structure (semi-wide)
  validate_structure(df)

  #stop if don't have google authentication established
  if(!googlesheets4::gs4_has_token() && is.null(getOption("gargle_oauth_email")))
    stop("resolve_knownissues() requires OAuth to be set prior to running. Establish authenication for the session using glamr::load_secrets() or googlesheets4::gs4_auth().")

  #pull down known issues from Google Sheets & tidy
  df_issues <- get_knownissues() %>%
    clean_knownissues()

  #set list to exclude from MSD
  df_flag <- flag_knownissues(df, df_issues)

  #replace known issue data to exclude with NAs
  df_adj <- squish_knownissues(df_flag)

  #remove central reporting
  if(remove_cs == TRUE)
    df_adj <- remove_centralsupport(df_adj)

  #print out known issues
  df %>%
    note_knownissues(df_issues) %>%
    print_knownissues(store_excl)

  invisible(df_adj)

}


#' Validate supplied dataframe to ensure valid structure
#'
#' @param df data from get_knownissues()
#'
#' @keywords internal
#'
validate_structure <- function(df){

  cols <- names(df)

  msd_req <- c("targets", "cumulative",
               "qtr1", "qtr2", "qtr3", "qtr4")

  fsd_req <- c("cop_budget_total", "workplan_budget_amt",
               "expenditure_amt")

  is_msd <- all(msd_req %in% cols)

  is_fsd <- all(fsd_req %in% cols)

  if(is_msd == FALSE && is_fsd == FALSE)
    stop("Need to provide a data frame in the normal MSD/FSD structure.")

}

#' Known Data Issues Tracker Google Sheet ID
#'
#' @return google sheet id
#' @keywords internal
#'
gs_id_knownissues <- '1CMPY-GCWP3NSNWvLGLwMwBaPZsobgvjtobhjkZkDfow'

#' Browse Known Issues
#'
#' @description Launches the USAID managed, Known Data Issues Tracker.
#'
#' The Known Data Issues Tracker is a table that summarizes different issues
#' that are known in DATIM but cannot be resolved. For example, when a treatment
#' partner start/ends mid-year, there will be duplicated targets that cannot be
#' adjusted in DATIM.
#'
#' The list of known issues is maintained by USAID/SIEI division. To access the
#' table requires having a USAID email account.
#'
#' @return Launches The Known Data Issues Tracker Google Sheet.
#' @seealso [resolve_knownissues()] to exclude Known Issues from data set;
#'  [set_email()] to store USAID email;
#'  [load_secrets()] to load credentials into session.
#' @export
#'
#' @examples
#'  \dontrun{
#'  load_secrets()
#'  browse_knownissues() }
browse_knownissues <- function(){
  googlesheets4::gs4_browse(googlesheets4::as_sheets_id(gs_id_knownissues))
}

#' Get Known Issues locally
#'
#' @description load Google Sheet
#' @keywords internal
#'
get_knownissues <- function(){

  #load data from sheet
  suppressMessages(
  df_issues <- googlesheets4::read_sheet(googlesheets4::as_sheets_id(gs_id_knownissues),
                                         sheet = "Known Issues Responses"))

  #rename columns
  df_issues <- df_issues %>%
    dplyr::rename(timestamp = Timestamp,
                  country = `What PEPFAR country is affected?`,
                  fiscal_year = `What fiscal year has this issue?`,
                  period_type = `Where does the known issue reside?`,
                  period = `Is there a specific quarter affected?`,
                  mech_code = `What is the specific mechanism id?`,
                  mech_pname = `What is the preferred mechanism name?`,
                  indicator = `What indicators are affected?`,
                  action = `How should the data issue be handled?`,
                  description = `Description of the issue`,
                  opu_status = `Is there an OPU open/pending at this time?`,
                  email = `Email Address`,
                  resolved = `resolved?`)

  return(df_issues)

}


#' Clean Known Issues Google Sheet
#'
#' @param df data from get_knownissues()
#'
#' @return adjust mech_code and tidy by expanding by indicator
#' @keywords internal

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

  #adjust period_type
  df_expnd <- df_expnd %>%
    dplyr::mutate(period_type = period_type %>%
                    stringr::str_remove("MER ") %>%
                    stringr::str_replace("Amount", "amt") %>%
                    stringr::str_replace_all(" ", "_") %>%
                    tolower())

  return(df_expnd)
}


#' Flag Known Issues
#'
#' @param df original MSD input
#' @param df_issues df output from clean_knownissues()
#'
#' @return df of MSD with extra column of period type to know what to excluded
#' @keywords internal

flag_knownissues <- function(df, df_issues){

  #variables used in distinct, excluding indicator for FSD
  var_distinct <- c("mech_code", "fiscal_year", "indicator", "period_type")
  if("cop_budget_total" %in% names(df))
    var_distinct[var_distinct != "indicator"]

  #create df for just exclusion and hasn't been resolved
  df_excl <- df_issues %>%
    dplyr::filter(action == "exclude",
                  resolved == FALSE) %>%
    dplyr::distinct(dplyr::across(var_distinct)) %>%
    dplyr::mutate(value = TRUE) %>%
    tidyr::pivot_wider(names_from = period_type,
                       names_prefix = "exclude_",
                       values_from = value)

  #variables used in join, exclude indicator for FSD
  var_join <- c("mech_code", "fiscal_year", "indicator")
  if("cop_budget_total" %in% names(df))
    var_join[var_join != "indicator"]

  #join to main df
  df_join <- dplyr::left_join(df, df_excl, by = var_join)

  return(df_join)
}


#' Squish Known Issues
#'
#' @param df df output from flag_knownissues()
#'
#' @return dataframe with known issues data removed
#' @keywords internal

squish_knownissues <- function(df){

  #ensure that exclude_results vars exist
   df <- complete_exclude_vars(df)

  #fill exclude_* with FALSE for mechs not not in flagged list
    df <- df %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("exclude"),
                                  ~ ifelse(is.na(.), FALSE, .)))

  #remove known issues = replace with NA (for MER)
  if("cumulative" %in% names(df)){
    df <- df %>%
      dplyr::mutate(dplyr::across(c(dplyr::starts_with("qtr"), cumulative),
                                  ~ ifelse(exclude_results == TRUE, NA_real_, .)),
                    targets = ifelse(exclude_targets == TRUE, NA_real_, targets))
  }

  #remove known issues = replace with NA (for MER)
    if("cop_budget_total" %in% names(df)){
      df <- df %>%
        dplyr::mutate(cop_budget_total = ifelse(exclude_cop_budget_total == TRUE, NA_real_, targets),
                      workplan_budget_amt = ifelse(exclude_cop_budget_total == TRUE, NA_real_, targets),
                      expenditure_amt = ifelse(exclude_cop_budget_total == TRUE, NA_real_, targets))
    }

  #remove exclude_*
    df <- dplyr::select(df, -dplyr::starts_with("exclude"))

  return(df)
}

#' Add full set of exclusion variables
#'
#' @param df df output from flag_knownissues()
#'
#' @return data frame with new variables
#' @keywords internal

complete_exclude_vars <- function(df){

  lst_exclude <- c("results", "targets", "cop_budget_total",
                   "workplan_budget_amt", "expenditure_amt") %>%
    paste("exclude", ., sep = "_")

  # purrr::map(lst_exclude, ~ add_var(df, .x))
  df <- df %>%
    add_var(lst_exclude[1]) %>%
    add_var(lst_exclude[2]) %>%
    add_var(lst_exclude[3]) %>%
    add_var(lst_exclude[4]) %>%
    add_var(lst_exclude[5])

  return(df)

}

#' Add variable if it doesn't exist
#'
#' @param df dataframe
#' @param x character of new variable name
#'
#' @return data frame with new variable
#' @keywords internal

add_var <- function(df, x){
  if(!{{x}} %in% names(df))
    df <- dplyr::mutate(df, {{x}} := NA)

  return(df)
}


#' Remove Central Support Reporting
#'
#' Central Support is often desired to be removed from MER analysis. As of
#' FY21Q4, "CS" is a field under `indicatortype`, which can be used to
#' identify and exclude this sort of reporting. This function is run by
#' default in `resolve_knownissues`, but can be run separately if desired.
#'
#' @param df MSD dataframe (must include indicatortype)
#'
#' @return df with central support reporting removed
#' @export
#'
remove_centralsupport <- function(df){

  if("indicatortype" %in% names(df)){
    df <- dplyr::filter(df, indicatortype != "CS")
  } else {
    usethis::ui_warn("Cannot remove Central Support reporting due to `indicatortype` missing from provided data frame.")
  }

  return(df)
}

#' Note Known Issues
#'
#' @param df_orig MSD dataset
#' @param df_allissues df from clean_knownissues()
#'
#' @return df that has a description of what was excluded in orig df
#' @keywords internal

note_knownissues <- function(df_orig, df_allissues){

  #limit to relevent field for messaging
  df_issues_type <- df_allissues %>%
    dplyr::select(action, mech_code, fiscal_year, indicator, period_type, description)

  #note all the changes being made
  df_issues_matches <- df_issues_type %>%
    dplyr::inner_join(df_orig, by = c("mech_code", "indicator", "fiscal_year")) %>%
    dplyr::distinct(action, country, mech_code, fiscal_year, period_type, description, indicator) %>%
    dplyr::group_by(action, country, mech_code, fiscal_year, period_type, description) %>%
    dplyr::summarise(indicator = paste(indicator, collapse=", "), .groups = "drop") %>%
    dplyr::mutate(msg = dplyr::case_when(action == "exclude" & period_type %in% c("results", "targets") ~ glue::glue("Excluded {country} {mech_code} FY{stringr::str_sub(fiscal_year, -2)} {period_type} for {indicator}"),
                                         action == "exclude" ~ glue::glue("Excluded {country} {mech_code} FY{stringr::str_sub(fiscal_year, -2)} {period_type}"),
                                         period_type %in% c("results", "targets") ~ glue::glue("Extra info provided for {country} {mech_code} FY{stringr::str_sub(fiscal_year, -2)} {period_type}: {description}; affecting {indicator}"),
                                         TRUE ~ glue::glue("Extra info provided for {country} {mech_code} FY{stringr::str_sub(fiscal_year, -2)} {period_type}: {description}")))

  return(df_issues_matches)
}

#' Print Known Issues to console
#'
#' @param df data frame from note_knownissues()
#' @param store_excl whether to store exclusions to Global Envir
#'
#' @return console print of known issues
#' @keywords internal

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
    lst_excl_matches <- "Given your dataset, no documented issues to exclude"

  #print info
  purrr::walk(lst_info_matches, usethis::ui_info)

  #store in global envir if desired
  if(store_excl == TRUE)
    lst_excl_matches <<- lst_excl_matches

}
