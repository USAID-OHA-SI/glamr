#' DATIM Analytics API
#'
#' @param url supply url for API call
#' @param username DATIM username
#' @param password DATIM password, recommend using `mypwd()`
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  myurl <- paste0(baseurl, "api/29/analytics.json?
#'                  dimension=LxhLO68FcXm:udCop657yzi&
#'                  dimension=ou:LEVEL-4;HfVjCurKxh2&
#'                  filter=pe:2018Oct&
#'                  displayProperty=SHORTNAME&outputIdScheme=CODE")
#'  myuser <- "UserX"
#'  df_targets <- get_datim_targets(myurl, myuser, mypwd(myuser)) }

extract_datim <- function(url,username,password) {

  check_internet()

  json <- url %>%
    httr::GET(httr::authenticate(username,password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON()

  if ( NROW(json$rows) > 0 ) {
    metadata <- purrr::map_dfr(json$metaData$items, dplyr::bind_rows, .id = "from")

    suppressMessages(
    df <- tibble::as_tibble(json$rows, .name_repair = ~ json$headers$column)
    )

    orguids <- df$`Organisation unit`

    if(stringr::str_detect(url, "hierarchyMeta=true")){

      orgpath <- dplyr::bind_rows(json$metaData$ouHierarchy) %>%
        tidyr::gather()

      levels <- orgpath$value %>%
        stringr::str_count("/") %>%
        max() + 1

      headers <- paste0("orglvl_", seq(1:levels))

      df <- dplyr::left_join(df, orgpath, by = c("Organisation unit" = "key")) %>%
        tidyr::separate(value, headers, sep = "/")
    }


    df <- df %>%
      dplyr::mutate_all(~plyr::mapvalues(., metadata$from, metadata$name, warn_missing = FALSE)) %>%
      dplyr::mutate(Value = as.numeric(Value)) %>%
      dplyr::bind_cols(orgunituid = orguids) %>%
      clean_pds()

    return(df)

  } else {

    return(NULL)

  }
}


#' Title
#'
#' @param df dataframe from \code{extract_datim()}
#'
#' @return Convert periods from long CY dates to PEPFAR standard FY
#' @export
#'
#' @examples
#' \dontrun{
#' pds <- c("Jan to Mar 2019", "Oct 2018 to Sep 2019")
#' clean_pds(pds) }

clean_pds <- function(df){

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
