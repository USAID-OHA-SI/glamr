#' Import file from Google Drive
#'
#' @param drive_folder Google id for Google Drive Folder
#' @param filename exact name of file on Googl Drive to download
#' @param folderpath path where you want file stored, default = "Data"
#' @param zip should the file be zipped? default = TRUE
#'
#' @return stores file from Google Drive as a zipped file
#' @export
#'
#' @examples
#' \dontrun{
#' library(googledrive)
#' googledrive::drive_auth()
#' fldr <- "Spp-y8DYsdRTrzDqUmK4fX5v"
#' import_drivefile(fldr, "TestFile.csv")
#' }

import_drivefile <- function(drive_folder, filename, folderpath = "Data", zip = TRUE){

  #get files + ids from google drive folder
    files <- googledrive::drive_ls(googledrive::as_id(drive_folder))

  #pull id for specific file
    id <- files %>%
      dplyr::filter(name == filename) %>%
      dplyr::pull(id)

  #download file and store in Data folder
    googledrive::drive_download(googledrive::as_id(id),
                                path = file.path(folderpath, filename),
                                overwrite = TRUE)

  if(zip == TRUE){
    #zip file
    orig_wd <- getwd()
    setwd(folderpath)
    filename %>%
      stringr::str_replace("csv$", "zip") %>%
      zip(filename)
    #remove
    unlink(filename)
    setwd(orig_wd)
  }

}
