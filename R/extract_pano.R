#' @title Create an active session for PEPFAR Panorama
#'
#' @param username Username for PEPFAR Panorama Account. Recommend using `pano_user()`
#' @param password Password for PEPFAR Panorama Account. Recommend using `pano_pwd()`
#' @param base_url PEPFAR Panorama base url
#'
#' @return login session
#' @export
#'
#' @examples
#' \dontrun{
#'   library(glamr)
#'
#'   s <- pano_session("<my-pano-user>", "<my-password>")
#' }
#'
pano_session <- function(username = NULL,
                         password = NULL,
                         base_url = NULL) {

  if (base::is.null(base_url)) {
    base_url <- "https://pepfar-panorama.org"
  }

  login_url <- base::paste0(base_url, "/forms/mstrauth/")

  # Check user's credentials
  if (base::is.null(username)) {
    username <- pano_user()
  }

  if (base::is.null(password)) {
    password <- pano_pwd()
  }

  # Data for post submission
  login_body <- base::list(
    "project" = "PEPFAR",
    "username" = username,
    "pw" = password
  )

  login_req <- httr::POST(url = login_url, body = login_body)

  login_sess <- login_req %>%
    httr::content("parsed")

  if (!base::is.null(login_sess) & login_sess$status == 1) {
    return(login_sess$mstr_session)
  } else {
    base::stop("ERROR - Unable to create a valid session")
  }
}


#' @title Read html content from web page
#'
#' @param page_url URL of the page to be extracted
#' @param session  Valid and active login session
#'
#' @return html content
#' @export
#'
#' @examples
#' \dontrun{
#'   library(glamr)
#'
#'   s <- pano_session("<my-pano-user>", "<my-password>")
#'   url <- "https://pepfar-panorama.org/forms/downloads"
#'
#'   cont <- pano_content(page_url = url, session = s)
#' }
#'
pano_content <- function(page_url, session) {

  page <- httr::GET(page_url, httr::set_cookies("formsSessionState" = session))

  if (!base::is.null(page)) {
    page <- page %>%
      httr::content("text") %>%
      rvest::read_html()
  } else {
    base::stop("ERROR - Unable to extract page content")
  }

  return(page)
}


#' @title Extract data elements details from html content
#'
#' @param page_html html content
#' @param page_name Current html page name
#' @param page_url  Curretn html page url
#'
#' @return html element
#' @export
#'
#' @examples
#' \dontrun{
#'   library(glamr)
#'
#'   s <- pano_session("<my-pano-user>", "<my-password>")
#'   url <- "https://pepfar-panorama.org/forms/downloads"
#'
#'   cont <- pano_content(page_url = url, session = s)
#'
#'   elts <- pano_elements(page_html = cont, page_url = url)
#' }
#'
pano_elements <- function(page_html,
                          page_name = NA,
                          page_url = NULL) {

  if (base::is.null(page_url)) {
    page_url = "https://pepfar-panorama.org/forms/downloads"
  }

  page_items <- page_html %>%
    rvest::html_elements("li>a") %>%
    rvest::html_text()

  page_hrefs <- page_html %>%
    rvest::html_elements("li>a") %>%
    rvest::html_attr("href") %>%
    stringr::str_remove_all(., "\\\\$")

  page_metas <- page_html %>%
    rvest::html_elements("li") %>%
    rvest::html_attr("class")

  df_elements <- tibble::tibble(
      parent = page_name,
      item = page_items,
      type = page_metas,
      path = page_hrefs
    ) %>%
    dplyr::mutate(path = base::paste0(page_url, "/", page_hrefs))
}


#' @title Download file from PEPFAR Panorama
#'
#' @param item_url   URL for the item to be downlaoded
#' @param session    Login session
#' @param dest       Location and name of the destination file
#' @param uncompress If yes, the downloaded zip file will be decompressed. Default is FALSE
#'
#' @return file content as binary
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   library(tidyverse)
#'   library(glamr)
#'
#'   s <- pano_session("<my-pano-user>", "<my-password>")
#'   url <- "https://pepfar-panorama.org/forms/downloads"
#'
#'   cont <- pano_content(page_url = url, session = s)
#'
#'   elts <- pano_elements(page_html = cont, page_url = url)
#'
#'   f_url <- elts %>% filter(type == "file zipfile") %>% pull(path) %>% first()
#'
#'   pano_download(item_url = f_url, session = s, dest = "./Data/)
#' }
#'
pano_download <- function(item_url, session,
                          dest = NULL,
                          uncompress = FALSE) {

  # Default destination folder
  if (base::is.null(dest)) {
    dest = glamr::si_path("path_msd")
  }

  if(!base::dir.exists(dest)) {
    base::cat(crayon::red("\nDestination is not a valid directory\n"))
    base::cat(dest)
    base::stop("Invalid Directory")
  }

  dfile <- base::paste0(dest, "/", base::basename(item_url))

  item_url %>%
    httr::GET(.,
              httr::write_disk(paste0(dest, "/", base::basename(item_url)), overwrite=TRUE),
              httr::set_cookies("formsSessionState" = session))

  if (uncompress) {
    zip::unzip(dfile, overwrite = TRUE, exdir = dest)
  }
}


#' @title Unpack Pano files directories
#'
#' @param df_pano  Panorama output files items
#' @param session  Valid and active login session
#'
#' @return unnested data frame containing output files
#' @examples
#'
#' @examples
#' \dontrun{
#'   library(tidyverse)
#'   library(glamr)
#'
#'   s <- pano_session("<my-pano-user>", "<my-password>")
#'   url <- "https://pepfar-panorama.org/forms/downloads"
#'
#'   cont <- pano_content(page_url = url, session = s)
#'
#'   elts <- pano_elements(page_html = cont, page_url = url)
#'
#'   pano_unpack(df_pano = elts, session = s)
#' }
#'
pano_unpack <- function(df_pano, session) {

  # filter levels
  df_items <- df_pano %>%
    dplyr::filter(type == "dir")

  # check for valid rows
  if (base::nrow(df_items) == 0) {
    base::print(glue::glue("No directories"))

    return(df_items)
  }

  base::message("unpacking ...")
  base::print(df_items$item %>% base::paste(collapse = ", "))

  # Extract items
  df_items <- df_items %>%
    dplyr::select(item, path) %>%
    purrr::pmap_dfr(function(item, path){
      items <- pano_content(page_url = path, session = session) %>%
        pano_elements(page_name = item, page_url = path)

      return(items)
    })

  # Loop through all nested directories
  if ("dir" %in% df_items$type) {
    df_items <- df_items %>%
      pano_unpack(session = session) %>%
      dplyr::bind_rows(df_items, .)
  }

  return(df_items)
}


#' @title Extract data outputs from Panorama
#'
#' @note This function combines `pano_session()`, `pano_content()`, `pano_elements()`, and in some cases `pano_unpack()`
#'
#' @param item         Panorama data type. Eg: mer, financial, sims, narratives
#' @param version      Data release version: initial or clean
#' @param fiscal_year  Reporting Fiscal year
#' @param quarter      Reporting Quarter
#' @param unpack       If yes, unpack nested directories
#' @param username     Panorama username, recommend using `pano_user()`
#' @param password     Panorama password, recommend using `pano_pwd()`
#' @param base_url     Panorama base url
#'
#' @return list of output files as data frame
#' @export
#' @examples
#' \dontrun{
#'   library(tidyverse)
#'   library(glamr)
#'
#'   pano_extract(item = "mer")
#' }
#'
pano_extract <- function(item = "mer",
                         version = "initial",
                         fiscal_year = 2021,
                         quarter = 3,
                         unpack = FALSE,
                         username = NULL,
                         password = NULL,
                         base_url = NULL) {

  # validation
  if (base::is.null(base_url)) {
    base_url <- "https://pepfar-panorama.org"
  }

  # Links
  data_path <- "/forms/downloads"
  data_url <- base::paste0(base_url, data_path)

  # archived files: update based on fy & qtr
  archive <- NULL

  # Search key
  s_dir <- base::paste0(item, " FY", fiscal_year, " Q", quarter)

  # Current releases
  if (version == "initial") {
    s_dir <- s_dir %>% base::paste0(" Pre-Cleaning")
  }
  else if (version == "clean") {
    s_dir <- s_dir %>% base::paste0(" Post-Cleaning")
  }
  else {
    base::stop("INPUT - Invalid input for version")
  }

  # Session
  sess <- pano_session(username = username,
                       password = password,
                       base_url = base_url)

  # Extract Main directories
  dir_items <- pano_content(
    page_url = data_url,
    session = sess) %>%
    pano_elements()

  if (base::is.null(dir_items) | base::nrow(dir_items) == 0) {
    base::stop("ERROR - No data items found on the main downlaod page")
  }

  # Check reporting period # MER FY2021 Q3 Pre-Cleaning
  mer <- dir_items %>%
    dplyr::filter(stringr::str_detect(item, "^MER")) %>%
    dplyr::pull(item)

  rep_pd <- mer %>%
    stringr::str_extract_all("\\d") %>%
    base::unlist()

  curr_rep_fy <- rep_pd[1:4] %>%
    base::paste0(collapse = "") %>%
    base::as.integer()

  curr_rep_qtr <- rep_pd[5] %>% base::as.integer()

  curr_mer <- mer %>%
    stringr::str_extract("(?<=Q)(.*)(?=-)") %>%
    stringr::str_remove("\\d\\s")

  if (fiscal_year < curr_rep_fy |
      quarter < curr_rep_qtr |
      !stringr::str_detect(s_dir, curr_mer)) {
    archive = TRUE
  } else {
    archive = FALSE
  }

  # Retrieve sub-folders from previous releases
  if (archive == TRUE) {
    s_prev_dir <- base::paste0("Previous ", item, " Releases")

    base::message("Looking into previous releases ...")
    base::print(s_prev_dir)

    # Previous sub-directories
    dir_items <- dir_items %>%
      dplyr::filter(
        stringr::str_detect(
          stringr::str_to_lower(item),
          stringr::str_to_lower(s_prev_dir))) %>%
      dplyr::select(item, path) %>%
      purrr::pmap_dfr(function(item, path){
        items <- pano_content(page_url = path, session = sess) %>%
          pano_elements(page_name = item, page_url = path)

        return(items)
      })
  }

  # Narrow search directories
  dir_items <- dir_items %>%
    dplyr::filter(
      stringr::str_detect(
        stringr::str_to_lower(item),
        stringr::str_to_lower(s_dir)))

  base::message("Search directorie(s): ")
  base::print(dir_items$item %>% paste(collapse = ", "))

  # Extract immediate items
  df_items <- dir_items %>%
    dplyr::select(item, path) %>%
    purrr::pmap_dfr(function(item, path){
      items <- pano_content(page_url = path, session = sess) %>%
        pano_elements(page_name = item, page_url = path)

      return(items)
    })

  # Check for valid data
  if (base::is.null(df_items) | base::nrow(df_items) == 0) {
    base::cat(crayon::red("\nEMPTY - No data items found for this search\n"))
    return(dir_items)
  }

  # Unpack all the folders
  if (unpack == TRUE && "dir" %in% df_items$type) {
    base::message("Unpacking required for ...")

    dirs <- df_items %>%
      dplyr::filter(type == "dir") %>%
      dplyr::pull(item) %>%
      base::paste(collapse = ", ")

    base::print(dirs)

    df_items <- df_items %>%
      dplyr::filter(type == "dir") %>%
      pano_unpack(session = sess)
  }

  #Download files
  df_files <- dir_items %>%
    dplyr::bind_rows(df_items)

  # Return files
  return(df_files)
}
