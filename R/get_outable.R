#' Pull OU UIDS
#'
#' @param baseurl base url for the API, default = https://final.datim.org/
#' @param username DATIM Username
#' @param password DATIM password, recommend using `mypwd()`
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  ou_table <- datim_outable("userx", mypwd("userx")) }

get_outable <- function(username, password, baseurl = "https://final.datim.org/"){

  df_uid <- identify_ouuids(username, mypwd(username))

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
#' @param baseurl base url for the API, default = https://final.datim.org/
#' @param username DATIM Username
#' @param password DATIM password, recommend using `mypwd()`
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  ous <- identify_ouuids("userx", mypwd("userx")) }

identify_ouuids <- function(username, password, baseurl = "https://final.datim.org/"){

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
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#' @param baseurl base API url, default = https://final.datim.org/
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  #table for all OUs
#'   myuser <- "UserX"
#'   identify_levels(username = myuser, password = mypwd(myuser)) }

identify_levels <- function(username, password, baseurl = "https://final.datim.org/"){

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
                  countryname_iso = iso4) %>%
    dplyr::rename_at(dplyr::vars(country:facility), ~ paste0(., "_lvl"))

  return(df_levels)
}








