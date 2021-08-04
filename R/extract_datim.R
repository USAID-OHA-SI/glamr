#' DATIM Analytics API
#'
#' @param url supply url for API call
#' @param username DATIM Username, defaults to using `datim_user()` if blank
#' @param password DATIM password, defaults to using `datim_pwd()` if blank
#'
#' @export
#' @return  API pull of data from DATIM
#' @examples
#' \dontrun{
#'  myurl <- paste0(baseurl, "api/29/analytics.json?
#'                  dimension=LxhLO68FcXm:udCop657yzi&
#'                  dimension=ou:LEVEL-4;HfVjCurKxh2&
#'                  filter=pe:2018Oct&
#'                  displayProperty=SHORTNAME&outputIdScheme=CODE")
#'  df_targets <- get_datim_targets(myurl, datim_user(), datim_pwd()) }

extract_datim <- function(url,username,password) {

  check_internet()

  package_check('httr')
  package_check('jsonlite')

  if(missing(username))
    username <- datim_user()

  if(missing(password))
    password <- datim_pwd()

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
      convert_datim_pd_to_qtr()

    return(df)

  } else {

    return(NULL)

  }
}
