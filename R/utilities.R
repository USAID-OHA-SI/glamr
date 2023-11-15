#' Check internet connection
#'
#' @return A boolean or stops
#' @keywords internal

check_internet <- function(){

  package_check('curl')

  if(!curl::has_internet())
    stop("No internet connection. Cannot excecute API.")
}


#' Test status of query
#'
#' @param res response
#'
#' @return A boolean or stops
#' @keywords internal

check_status <- function(res){

  package_check('httr')

  if(httr::status_code(res) == 200)
    stop("The API returned an error")
}

#' Check if package exists
#'
#' @param pkg package name
#'
#' @export
#' @keywords internal

package_check <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "needed for this function to work. Please install it."),
         call. = FALSE)
  }
}


#' Test if service is stored in credential manager
#'
#' @param service account, either "email", "datim", "pano", or "s3"
#'
#' @export
#' @return A boolean

is_stored <- function(service = c("datim", "email", "pano", "s3")){

  package_check('keyring')

  accounts <- keyring::key_list()$service

  {{service}} %in% accounts

}

#' Test if option variable is loaded in current session
#'
#' @param opt_var option variable ("datim", "email", "path_msd", "path_datim", "path_vector", "path_raster")
#'
#' @return A boolean
#' @keywords internal

is_loaded <- function(opt_var){

  !is.null(getOption({{opt_var}}))

}


#' Test if package is installed locally
#'
#' @param package name of R package to check
#'
#' @return A boolean
#' @keywords internal

is_installed <- function(package){
  {{package}} %in% rownames(installed.packages())
}


#' @title Clean and connect parts of text together
#'
#' @param txt          String charactors
#' @param connections  Characters to be replaced by connector
#' @param connector    Charactor used as connector
#'
#' @return cleaned text
#' @export
#'
#' @examples
#' \dontrun{
#' connect_text("THIS - is complex (very bad)")}
#'
connect_text <- function(txt,
                         connections = "[^a-zA-Z0-9]",
                         connector = "_") {

  package_check("stringi")

  text <- base::sapply(txt, function(x) {
    x %>%
      stringr::str_split(connections) %>%
      base::unlist() %>%
      stringi::stri_remove_empty() %>%
      base::paste0(collapse = connector)
  }, USE.NAMES = FALSE)

  return(text)
}


#' @title Negate in
#' @description negate `%in%`
#' @export
`%ni%` <- Negate(`%in%`)


#' @title Assignment Pipe Operator
#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`


#' Generate Temporary Folder
#'
#' `temp_folder` created a temporary folder in your AppData directory, which
#' will be automatically removed after you close your RStudio session.
#'
#' @param launch do you want to launch the temp folder in the Windows Explorer?
#'  default = FALSE
#' @param quiet suppresses the output message related to the folder creation
#' and location, for use in sub functions, default = FALSE)
#'
#' @return creates a temp directory and stores it as `folderpath_tmp`
#' @export
#'
#' @examples
#' \dontrun{
#' load_secrets()
#' temp_folder(launch = TRUE)
#' purrr::walk2(.x = df_googlefiles$id,
#'              .y = df_googlefiles$filename,
#'              .f = ~googledrive::drive_download(googledrive::as_id(.x),
#'                                                file.path(folderpath_tmp, .y))) }
temp_folder <- function(launch = FALSE, quiet = FALSE){

  package_check('fs')

  folderpath_tmp <<- fs::dir_create(fs::file_temp())

  if(quiet == FALSE){
    usethis::ui_info("A temporary folder is now available here: {usethis::ui_path(folderpath_tmp)}")
    usethis::ui_info("The folder path is stored as the object {usethis::ui_code('folderpath_tmp')}")
  }

  if(launch == TRUE)
    shell.exec(folderpath_tmp)

  invisible(folderpath_tmp)
}

#' @title Get formatted current date
#'
#' @param fmt Date format
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  curr_date()
#'  curr_date(fmt = "%m/%d/%Y")
#' }
curr_date <- function(fmt = "%Y-%m-%d") {
  d <- base::Sys.Date()

  if (!base::is.null(fmt)) {
    d <- base::format(d, fmt)
  }

  return(d)
}

#' @title Open directory explorer or files
#'
#' @param path Full path of the file to be opened
#'
#' @note This assumes default applications are set for various file type
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  dir_name <- "C:/Users/<username>/Downloads"
#'  open_path(dir_name)
#'
#'  file_name <- "C:/Users/<username>/Downloads/test.csv"
#'  open_path(file_name)
#' }
open_path <- function(path) {
  utils::browseURL(path)
}
