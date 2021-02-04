#' Return Latest
#'
#' @param folderpath path to folder where file(s) are located
#' @param pattern    pattern in file name, regex expressions
#' @param ...        Any other valid option for `base::list.files()`
#'
#' @return the fill filepath for the most recent version of a file stub
#' @export
#'
#' @examples
#' \dontrun{
#' file_stub <- "MER_Structured_Datasets_OU_IM_FY18-20"
#' filepath <- return_latest("Data", file_stub)
#' df <- read_rds(filepath) }

return_latest <- function(folderpath, pattern, ...){

  f <- list.files(folderpath, pattern, full.names = TRUE)

  if(length(f) == 0)
    stop("no files found")


  if(length(f) > 1){
    f <- f %>%
      file.info() %>%
      tibble::rownames_to_column(var = "filepath") %>%
      dplyr::filter(ctime == max(ctime)) %>%
      dplyr::pull(filepath)
  }

  return(f)
}
