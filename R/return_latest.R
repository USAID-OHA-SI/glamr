#' Return Latest File
#'
#' `return_latest` checks for a pattern in a folder and provides the most
#' recent file bases on the time the file was modified
#'
#' @param folderpath path to folder where file(s) are located.
#' @param pattern    pattern in file name, regex expressions. If not parttern is
#'  provided, the last file in the folder will be returned.
#' @param quiet suppresses the output message related to the file name creation,
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
#' df <- read_rds(filepath) }

return_latest <- function(folderpath, pattern, quiet = FALSE, ...){

  r_env <- ifelse(is_pdap(), "pdap", "local")

  switch (r_env,
          local = return_latest_local(folderpath, pattern, quiet, ...),
          pdap = return_latest_pdap(folderpath, pattern, quiet, ...)
  )

}

#' Local Return Latest File
#'
#' @inheritParams return_latest
#' @keywords internal
#'
return_latest_local <- function(folderpath, pattern, quiet = FALSE, ...){

  if(missing(pattern))
    pattern <- ".*"

  f <- list.files(folderpath, pattern, full.names = TRUE, ...)

  if(length(f) == 0)
    stop("no files found")


  if(length(f) > 1){
    f <- f %>%
      file.info() %>%
      tibble::rownames_to_column(var = "filepath") %>%
      dplyr::filter(mtime == max(mtime)) %>%
      dplyr::pull(filepath)
  }

  if(quiet == FALSE){
    pattern_info <- ifelse(pattern == ".*", "", glue::glue(" matching {usethis::ui_path(pattern)}"))
    usethis::ui_info("Latest file in {usethis::ui_path(basename(dirname(f)))}{pattern_info}: {usethis::ui_path(basename(f))}")
  }

  return(f)
}


#' PDAP Return Latest File
#'
#' @inheritParams return_latest
#' @keywords internal
#'
return_latest_pdap <- function(folderpath, pattern, quiet = FALSE, ...){

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


  if(nrow(f) == 0)
    stop("no files found")


  if(length(f) > 1){
    f <- f %>%
      dplyr::filter(last_modified == max(last_modified)) %>%
      dplyr::pull(key)
  }

  if(quiet == FALSE){
    pattern_info <- ifelse(missing(pattern), "", glue::glue(" matching {usethis::ui_path(pattern)}"))
    usethis::ui_info("Latest file in {usethis::ui_path(folderpath)}{pattern_info}: {usethis::ui_path(basename(f))}")
  }

  return(f)
}
