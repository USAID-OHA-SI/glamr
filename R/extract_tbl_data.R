#' Extract table data from web page
#'
#' @param src_url The http(s)link to the source web page
#' @param tbl_id A unique identifer of the target table
#' @return A data frme
#' @export
#'
extract_tbl_data <- function(src_url, tbl_id) {

  check_internet()

  package_check('rvest')
  package_check('xml2')

  tbl_data <- xml2::read_html(src_url)

  tbl_data <- tbl_data %>%
    rvest::html_nodes(tbl_id) %>%
    rvest::html_table() %>%
    as.data.frame() %>%
    dplyr::rename_at(dplyr::vars(dplyr:everything()), dplyr::funs(stringr::str_replace_all(., "[\\W+]", "_"))) %>%
    dplyr::rename_at(dplyr::vars(dplyr:starts_with("_")), dplyr::funs(substr(., 2, nchar(.)))) %>%
    dplyr::rename_at(dplyr::vars(ends_with("_")), dplyr::funs(substr(., 1, nchar(.)-1))) %>%
    dplyr::rename_at(dplyr::vars(dplyr:everything()), tolower)

  return(tbl_data)
}
