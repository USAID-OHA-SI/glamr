#' Return Latest File
#'
#' `return_latest` checks for a pattern in a folder and provides the most
#' recent file bases on the time the file was modified
#'
#' @param folderpath path to folder where file(s) are located. This path can
#'  also be in the form of a Google ID
#' @param pattern    pattern in file name, regex expressions. If not parttern is
#'  provided, the last file in the folder will be returned.
#' @param quiet suppresses the output message related to the file name creation,
#' @param n number of files (starting with the latest) to return, default = 1
#'   for use in sub functions, default = FALSE
#' @param ...        Any other valid option for `base::list.files()`.
#'
#' @return a vector of the full filepath for the most recent version of a file stub
#' @export
#'
#' @examples
#' \dontrun{
#' file_stub <- "MER_Structured_Datasets_OU_IM_FY18-20"
#' filepath <- return_latest("Data", file_stub)
#' df <- read_psd(filepath) }
#'
#' \dontrun{
#' load_secrets("email")
#' gdrive_folder_id <- "1alhiHrDBJzRftcyJRW-THohz9LbJezzJ"
#' gdrive_path <- return_latest(gdrive_folder_id, "SI")
#' #download
#' drive_download(gdrive_path)
#' #or read directly from the Google Sheet
#' df <- read_sheet(filepath) }

return_latest <- function(folderpath, pattern, n = 1, quiet = FALSE, ...){

  r_env <- ifelse(is_pdap(), "pdap", "local")

  if(r_env == "local" & !file.exists(folderpath))
    r_env <- gdrive_file_exists(folderpath)

  switch (r_env,
          local = return_latest_local(folderpath, pattern, n, quiet, ...),
          gdrive = return_latest_gdrive(folderpath, pattern, n, quiet, ...),
          pdap = return_latest_pdap(folderpath, pattern, n, quiet, ...)
  )

}

#' Local Return Latest File
#'
#' @inheritParams return_latest
#' @keywords internal
#'
return_latest_local <- function(folderpath, pattern, n = 1, quiet = FALSE, ...){

  if(!file.exists(folderpath))
    usethis::ui_stop("Directory ('folderpath') not found: {usethis::ui_path({folderpath})}")

  if(missing(pattern))
    pattern <- ".*"

  pattern_info <- ifelse(pattern == ".*", "", glue::glue(" matching {usethis::ui_path(pattern)}"))

  f <- list.files(folderpath, pattern, full.names = TRUE, ...)

  if(length(f) == 0)
    usethis::ui_stop("No files found{pattern_info}")

  f <- f %>%
    file.info() %>%
    tibble::rownames_to_column(var = "filepath") %>%
    dplyr::slice_max(order_by = mtime,  n = {n}) %>%
    dplyr::pull(filepath)

  if(quiet == FALSE){
    usethis::ui_info("Latest file in {usethis::ui_path(basename(dirname(f)))}{pattern_info}: {usethis::ui_path(basename(f))}")
  }

  return(f)
}


#' PDAP Return Latest File
#'
#' @inheritParams return_latest
#' @keywords internal
#'
return_latest_pdap <- function(folderpath, pattern, n = 1, quiet = FALSE, ...){

  if(!requireNamespace("grabr", quietly = TRUE))
    usethis::ui_stop("Package {usethis::ui_field('grabr')} is required, see - https://usaid-oha-si.github.io/grabr/")

  suppressWarnings(
    f <- grabr::s3_objects(bucket = folderpath,
                           prefix = NULL,
                           access_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                           secret_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"))
  )


  if(!missing(pattern))
    f <- dplyr::filter(f, stringr::str_detect(key, {pattern}))

  pattern_info <- ifelse(missing(pattern), "", glue::glue(" matching {usethis::ui_path(pattern)}"))

  if(nrow(f) == 0)
    usethis::ui_stop("No files found{pattern_info}")

  f <- f %>%
    dplyr::slice_max(order_by = last_modified,  n = {n}) %>%
    dplyr::pull(key)


  if(quiet == FALSE){
    usethis::ui_info("Latest file in {usethis::ui_path(folderpath)}{pattern_info}: {usethis::ui_path(basename(f))}")
  }

  return(f)
}


#' Google Drive Latest File
#'
#' @inheritParams return_latest
#' @keywords internal
#'
return_latest_gdrive <- function(folderpath, pattern, n = 1, quiet = FALSE, ...){

  if(missing(pattern))
    pattern <- ".*"

  pattern_info <- ifelse(pattern == ".*", "", glue::glue(" matching {usethis::ui_path(pattern)}"))

  dir_name <- googledrive::drive_get(googledrive::as_id(folderpath))$name

  f <- googledrive::drive_ls(googledrive::as_id(folderpath), pattern = {pattern})

  if(nrow(f) == 0)
    usethis::ui_stop("No files found{pattern_info}")

  f <- f %>%
    dplyr::mutate(mtime = purrr::map_chr(drive_resource, "modifiedTime") %>%
                    lubridate::ymd_hms(quiet = TRUE, tz = "EST")) %>%
    dplyr::slice_max(order_by = mtime,  n = {n})

  f_id <- dplyr::pull(f, id)

  if(quiet == FALSE){
    usethis::ui_info("Latest file in {usethis::ui_path(dir_name)}{pattern_info}: {usethis::ui_path(f$name)}")
  }

  return(f_id)
}

#' Check whether file exists on Gdrive
#'
#' @inheritParams return_latest
#' @keywords internal
#'
gdrive_file_exists <- function(id){
  result <- tryCatch(
    {
      googledrive::drive_get(googledrive::as_id(id))
      "gdrive"
    },
    error = function(e) {
      "local"
    }
  )

  return(result)
}
