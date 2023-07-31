#' Return Latest File
#'
#' `return_latest` checks for a pattern in a folder and provides the most
#' recent file bases on the time the file was created.
#'
#' @param folderpath path to folder where file(s) are located.
#' @param pattern    pattern in file name, regex expressions. If not parttern is
#'  provided, the last file in the folder will be returned.
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

return_latest <- function(folderpath, pattern, ...){

  if(missing(pattern))
    pattern <- ".*"

  f <- list.files(folderpath, pattern, full.names = TRUE, ...)

  if(length(f) == 0)
    stop("no files found")


  if(length(f) > 1){
    f <- f %>%
      file.info() %>%
      tibble::rownames_to_column(var = "filepath") %>%
      dplyr::filter(ctime == max(mtime)) %>%
      dplyr::pull(filepath)
  }

  pattern_info <- ifelse(pattern == ".*", "", glue::glue(" matching {usethis::ui_path(pattern)}"))
  usethis::ui_info("Latest file in {usethis::ui_path(basename(dirname(f)))}{pattern_info}: {usethis::ui_path(basename(f))}")

  return(f)
}
