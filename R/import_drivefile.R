#' Import file from a Google Drive folder
#'
#' `import_drivefile` is a wrapper around `googledrive::drive_download`, useful
#' for pulling multiple files from a given Google Drive folder (with a Google
#' ID provided) to download by default to the Data folder of a project.
#'
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


#' @title Unpack Google Drive Metadata
#'
#' Google API provides extra metadata stored as a list in the dribble
#' returned, eg modified time, permissions, owner, etc.
#'
#' @param df           Results from Google Drive drive_ls
#' @param show_details Show all metadata fields, default is FALSE
#'
#' @return adds extra meta data to data frame
#' @export
#'
#' @examples
#' \dontrun{
#' library(googledrive)
#' drive_auth()
#' fldr <- as_id("<google-folder-id>")
#' drive_ls(fldr) %>% gdrive_metadata()
#' }
gdrive_metadata <- function(df, show_details = FALSE){

  # Check for drive metadata
  if (!"drive_resource" %in% names(df)) {
    base::cat(
      "\nCould not find metadata column:\n",
      crayon::red("drive_resource"),
      "\n")

    return(NULL)
  }

  # Convert json to df
  df <- df %>%
    dplyr::select(drive_resource) %>%
    jsonlite::toJSON() %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    dplyr::rename_with(~str_remove(., pattern = "drive_resource."))

  # Unpack details?
  if (!show_details) {
    df <- df %>%
      janitor::clean_names() %>%
      dplyr::select(kind, id, name, original_filename,
                    created_time, modified_time, size, trashed,
                    last_modified_by = last_modifying_user_display_name)

  }
  # Show all details (almost all)
  else {
    df <- df %>%
      tidyr::unnest(cols = owners, names_sep = ".") %>%
      tidyr::unnest(cols = permissions, names_sep = ".") %>%
      janitor::clean_names() %>%
      dplyr::select(-c(permission_ids, permissions_domain,
                       permissions_allow_file_discovery),
                    -ends_with(c("_link", "_thumbnail")),
                    -starts_with(c("thumbnail_"))) %>%
      dplyr::filter(permissions_type != "domain")
  }

  # Clean update df
  df <- df %>%
    dplyr::mutate(across(where(base::is.list), base::unlist)) %>%
    dplyr::mutate(across(ends_with("_time"), lubridate::ymd_hms)) %>%
    dplyr::mutate(across(size, base::as.integer))

  return(df)
}
