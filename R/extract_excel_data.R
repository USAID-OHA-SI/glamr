#' Extract data from excel link
#'
#' @param src_page The http(s)link to the source web page
#' @param link_id A CSS identifer of the hyperlinked element
#' @param file_sheet The file sheet number or name
#' @param file_ext The extension of the file
#' @return File content as a data frame
#' @export
#'
extract_excel_data <- function(src_page, link_id, file_sheet = 2, file_ext = "xlsx") {

  check_internet()

  package_check('rvest')
  package_check('xml2')

  src_home <- stringr::str_split(src_page, "/")[[1]]
  src_home <- paste0(src_home[1], "//", src_home[3])

  tmp_file <- tempfile(fileext = file_ext)

  file_link <- xml2::read_html(src_page) %>%
    rvest::html_nodes(link_id) %>%
    rvest::html_attr('href') %>%
    .[grepl(paste0(".", file_ext, "$"), .)]

  dta_url <- paste0(src_home, file_link)
  print(dta_url)

  utils::download.file(url = dta_url, destfile = tmp_file, mode = "wb")

  raw_dta <- readxl::read_excel(tmp_file, sheet = file_sheet)

  raw_dta <- raw_dta %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains(" ")), dplyr::funs(stringr::str_replace_all(., " ", "_"))) %>%
    dplyr::rename_at(dplyr::vars(dplyr::everything()), tolower)

  return(raw_dta)
}
