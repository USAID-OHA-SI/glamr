
#' Access SI folderpath
#'
#' @description
#' `si_path` accesses folder paths stored in global options to make it easier
#' work across analysts/machines. Analysts will first setup the paths using
#' `set_paths()` which then store local folder paths where larger data are stored
#' centrally and outside of projects.
#'
#' @param type folderpath, eg "path_msd" (default), "path_datim", "path_raster", "path_vector"
#'
#' @return folderpath stored in global options
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

  if(!is_loaded(type) || getOption(type) == "NULL")
    ui_oops("No folder path stored in your {ui_path('.Rprofile')} matches {ui_path(type)}. Use {ui_code('glamr::set_paths')} to store one.")

  getOption(type)
}


#' Set SI local folder paths
#'
#' @param folderpath_msd folderpath where the MSDs are stored
#' @param folderpath_datim folderpath where DATIM data are store eg (org hierarchy, mech table)
#' @param folderpath_raster folderpath where GIS raster data are stored
#' @param folderpath_vector folderpath where GIS vector data are stored
#'
#' @return code chunk to paste into .Rprofile
#' @export
#'
#' @importFrom usethis ui_stop
#' @importFrom usethis ui_path
#' @importFrom usethis ui_todo
#' @importFrom usethis ui_code_block
#'
#' @examples
#' \dontrun{
#' set_paths <- set_path(folderpath_msd = "C:/Users/rshah/Documents/Data") }

set_paths <- function(folderpath_msd = NULL,
                      folderpath_datim = NULL,
                      folderpath_raster = NULL,
                      folderpath_vector = NULL){

  if(!is.null(folderpath_msd) && !dir.exists(folderpath_msd))
    ui_stop("The folder path provided for {ui_path(folderpath_msd)} is not a valid path")
  if(!is.null(folderpath_datim) && !dir.exists(folderpath_datim))
    ui_stop("The folder path provided for {ui_path(folderpath_datim)} is not a valid path")
  if(!is.null(folderpath_raster) && !dir.exists(folderpath_raster))
    ui_stop("The folder path provided for {ui_path(folderpath_raster)} is not a valid path")
  if(!is.null(folderpath_vector) && !dir.exists(folderpath_vector))
    ui_stop("The folder path provided for {ui_path(folderpath_vector)} is not a valid path")

  folderpath_msd <- ifelse(is.null(folderpath_msd), "NULL", {folderpath_msd})
  folderpath_datim <- ifelse(is.null(folderpath_datim), "NULL", {folderpath_datim})
  folderpath_raster <- ifelse(is.null(folderpath_raster), "NULL", {folderpath_raster})
  folderpath_vector <- ifelse(is.null(folderpath_vector), "NULL", {folderpath_vector})

   ui_todo("Include this code in {ui_path('.Rprofile')} to store your SI folder paths.")

   ui_code_block(
     "
    options(
      path_msd = '{folderpath_msd}',
      path_datim = '{folderpath_datim}',
      path_raster = '{folderpath_raster}',
      path_vector = '{folderpath_vector}'
    )
    "
   )

   usethis::edit_r_profile("user")
}




