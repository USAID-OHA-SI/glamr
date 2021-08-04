#' Pull Table of OUs/Countries, UIDs, ISO codes and levels
#'
#' `get_outable` pulls from DATIM to return a dataframe with all PEPFAR
#' Operating Units and countries along with useful information for merging,
#' eg ISO codes, and use in DATIM APIs, eg UIDs and hierarchy levels.
#'
#' `get_outtable` is a wrapper around `identify_ouuids` and `identify_levels`
#' that pulls this information directly from DATIM. The user will need to have
#' a DATIM account to access this data. You can take advantage of storing you
#' credentials locally in a secure way using `set_datim`.
#'
#' @param username DATIM Username, defaults to using `datim_user()` if blank
#' @param password DATIM password, defaults to using `datim_pwd()` if blank
#' @param baseurl base url for the API, default = https://final.datim.org/
#'
#' @export
#' @return data frame with all PEPFAR OUs, countries, their UIDs, ISO codes
#'   and different levels in the DATIM hierarchy
#' @seealso [set_datim()] to store DATIM authentication;
#'   [load_secrets()] to load credentials into session
#' @examples
#' \dontrun{
#'  load_secrets()
#'  ou_table <- datim_outable() }

get_outable <- function(username, password, baseurl = "https://final.datim.org/"){

  df_uid <- identify_ouuids(username, password)

  df_levels <- identify_levels(username, password, baseurl)

  df_outable <- df_uid %>%
    dplyr::rename(operatingunit_uid = uid) %>%
    dplyr::select(-type) %>%
    dplyr::left_join(df_levels, ., by = c("operatingunit" = "country"))

  df_outable <- df_uid %>%
    dplyr::rename(countryname_uid = uid) %>%
    dplyr::select(-type) %>%
    dplyr::left_join(df_outable, ., by = c("countryname" = "country"))

  df_outable <- df_outable %>%
    dplyr::select(operatingunit, operatingunit_uid, operatingunit_iso,
                  countryname, countryname_uid, countryname_iso,
                  dplyr::everything()) %>%
    dplyr::arrange(operatingunit, countryname)

  return(df_outable)

}

#' Pull OU UIDS
#'
#' `identify_ouuids` pulls from DATIM to return a dataframe with all PEPFAR
#' Operating Units and countries and their UIDs. This is one of two components
#' that feeds into `get_outable`.
#'
#' To access the UIDs, the user will need to have a DATIM account. You can
#' take advantage of storing you credentials locally in a secure way
#' using `set_datim`.
#'
#' @param username DATIM Username, defaults to using `datim_user()` if blank
#' @param password DATIM password, defaults to using `datim_pwd()` if blank
#' @param baseurl base url for the API, default = https://final.datim.org/
#'
#' @export
#' @return
#' @seealso [set_datim()] to store DATIM authentication;
#'   [load_secrets()] to load credentials into session
#'
#' @examples
#' \dontrun{
#'  load_secrets()
#'  ous <- identify_ouuids() }

identify_ouuids <- function(username, password, baseurl = "https://final.datim.org/"){

  check_internet()

  if(missing(username))
    username <- datim_user()

  if(missing(password))
    password <- datim_pwd()

  ous <- baseurl %>%
    paste0("api/organisationUnits?filter=level:eq:3") %>%
    httr::GET(httr::authenticate(username,password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten=TRUE) %>%
    purrr::pluck("organisationUnits")

  region_uids <- ous %>%
    dplyr::filter(stringr::str_detect(displayName, "Region")) %>%
    dplyr::pull(id)

  ctrys <- purrr::map_dfr(.x = region_uids,
                          .f = ~ baseurl %>%
                            paste0("api/organisationUnits?filter=level:eq:4&filter=path:like:", .x) %>%
                            httr::GET(httr::authenticate(username,password)) %>%
                            httr::content("text") %>%
                            jsonlite::fromJSON(flatten=TRUE) %>%
                            purrr::pluck("organisationUnits") %>%
                            dplyr::mutate(regional = TRUE))


  uids <- ous %>%
    dplyr::bind_rows(ctrys) %>%
    dplyr::arrange(displayName)

  uids <- uids %>%
    dplyr::rename(uid = id,
                  country = displayName) %>%
    dplyr::mutate(type = ifelse(is.na(regional), "OU", "Country")) %>%
    dplyr::select(-regional)

  return(uids)
}


#' Identify Facility/Community levels in org hierarchy
#'
#' `identify_levels` pulls from DATIM to return a dataframe with all PEPFAR
#' Operating Units and countries with their ISO codes and hierarhcy levels.
#' This is one of two components that feeds into `get_outable`.
#'
#' To access the UIDs, the user will need to have a DATIM account. You can
#' take advantage of storing you credentials locally in a secure way
#' using `set_datim`.
#'
#' @param username DATIM Username, defaults to using `datim_user()` if blank
#' @param password DATIM password, defaults to using `datim_pwd()` if blank
#' @param baseurl base url for the API, default = https://final.datim.org/
#'
#' @export
#' @seealso [set_datim()] to store DATIM authentication;
#'   [load_secrets()] to load credentials into session
#' @examples
#' \dontrun{
#'  #table for all OUs
#'   load_secrets()
#'   identify_levels() }

identify_levels <- function(username, password, baseurl = "https://final.datim.org/"){

  check_internet()

  if(missing(username))
    username <- datim_user()

  if(missing(password))
    password <- datim_pwd()

  df_levels <- baseurl %>%
    paste0(.,"api/dataStore/dataSetAssignments/orgUnitLevels") %>%
    httr::GET(httr::authenticate(username,password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten=TRUE) %>%
    purrr::map_dfr(dplyr::bind_rows) %>%
    dplyr::mutate_if(is.character, ~ dplyr::na_if(., ""))

  #adjust for regional missions
  df_levels <- df_levels %>%
    dplyr::mutate(name4 = ifelse(is.na(name4), name3, name4),
                  iso4 = ifelse(is.na(iso4), iso3, iso4))

  #rename
  df_levels <- df_levels %>%
    dplyr::rename(operatingunit = name3,
                  countryname = name4,
                  operatingunit_iso = iso3,
                  countryname_iso = iso4,
                  psnu = prioritization) %>%
    dplyr::rename_with(.cols= where(is.integer), ~ paste0(., "_lvl")) %>%
    dplyr::select(dplyr::everything(), country_lvl, psnu_lvl,
                  community_lvl, facility_lvl)

  return(df_levels)
}








