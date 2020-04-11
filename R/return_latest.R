#' Return Latest
#'
#' @param folderpath path to folder where file(s) are located
#' @param filepattern pattern
#'
#' @return the fill filepath for the most recent version of a file stub
#' @export
#'
#' @examples
#' #' \dontrun{
#' file_stub <- "MSD_.*"
#' filepath <- return_latest("Data", file_stub)
#' df <- read_rds(filepath) }

return_latest <- function(folderpath, filepattern){

  f <- list.files(out_folder, filepattern, full.names = TRUE)

  if(length(f) == 0)
    stop("no files found")


  if(length(f) > 1){
    f <- f %>%
      file.info() %>%
      rownames_to_column(var = "filepath") %>%
      filter(ctime == max(ctime)) %>%
      pull(filepath)
  }

  return(f)
}
