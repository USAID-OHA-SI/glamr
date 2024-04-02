#' Access SI folderpath
#'
#' `si_path` accesses folder paths stored in global options to make it easier
#' work across analysts/machines. Analysts will first setup the paths using
#' `set_paths()` which then store local folder paths where larger data are stored
#' centrally and outside of projects. This will also work on PEPFAR Workbench to
#' return the location of the MSD, `Sys.getenv("S3_READ")`.
#'
#' @param type folderpath, eg "path_msd" (default), "path_datim", "path_raster", "path_vector", "path_downloads"
#'
#' @return folderpath stored in global options
#' @family stored paths
#' @export
#'
#' @importFrom usethis ui_oops
#' @importFrom usethis ui_code
#' @importFrom usethis ui_path
#'
#' @examples
#' \dontrun{
#' #old
#'   list.files("C:/Users/rshah/Documents/Data", "OU_IM", full.names = TRUE)
#' #new
#'   list.files(si_path("path_msd"), "OU_IM", full.names = TRUE)
#' }

si_path <- function(type = "path_msd"){

  if((!is_loaded(type) || getOption(type) == "NULL") && !is_pdap())
    ui_oops("No folder path stored in your {ui_path('.Rprofile')} matches {ui_path(type)}. Use {ui_code('glamr::set_paths()')} to store one.")

  if(!is_pdap()){
    getOption(type)
  } else {
    pdap_bucket('read')
  }



}


#' Set SI local folder paths
#'
#' `set_paths` stores store local folder paths where larger data are stored
#' centrally and outside of projects. Accessed through use of `si_path()`.
#'
#' @param folderpath_msd folderpath where the MSDs are stored
#' @param folderpath_datim folderpath where DATIM data are store eg (org hierarchy, mech table)
#' @param folderpath_raster folderpath where GIS raster data are stored
#' @param folderpath_vector folderpath where GIS vector data are stored
#' @param folderpath_downloads folderpath to local Downloads folder
#'
#' @return code chunk to paste into .Rprofile
#' @family stored paths
#' @export
#'
#' @importFrom usethis ui_path
#' @importFrom usethis ui_todo
#' @importFrom usethis ui_code_block
#'
#' @examples
#' \dontrun{
#' set_paths <- set_path(folderpath_msd = "C:/Users/rshah/Documents/Data") }

set_paths <- function(folderpath_msd,
                      folderpath_datim,
                      folderpath_raster,
                      folderpath_vector,
                      folderpath_downloads){

   ui_code_block(
     "
    options(
      path_msd = '{validate_path(folderpath_msd, 'path_msd')}',
      path_datim = '{validate_path(folderpath_datim, 'path_datim')}',
      path_raster = '{validate_path(folderpath_raster, 'path_raster')}',
      path_vector = '{validate_path(folderpath_vector, 'path_vector')}',
      path_downloads = '{validate_path(folderpath_downloads, 'path_downloads')}'
    )
    "
   )

   ui_todo("Copy and paste this code in {ui_path('.Rprofile')} to store your SI folder paths.")

   usethis::edit_r_profile("user")
}


#' Validation Check to ensure valid folder path
#'
#' @param path folder path
#' @param type path type
#'
#' @importFrom usethis ui_stop
#' @importFrom usethis ui_path
#' @keywords internal

validate_path <- function(path, type){
  if(!missing(path) && !dir.exists(path))
    ui_stop("The folder path {ui_path(path)} is not valid")

  if(missing(path) && !is.null(getOption(type))){
    use_path <- getOption(type)
  } else if(missing(path)){
    use_path <- 'NULL'
  } else {
    use_path <- path
  }

  return(use_path)
}


