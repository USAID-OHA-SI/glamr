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

  package_check('jsonlite')
  package_check('janitor')

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


#' @title Get id of googledrive folder
#'
#' @note This function will create a new folder if add is set to TRUE
#'
#' @param name Googledrive folder name
#' @param path Googledrive parent path id
#' @param add  Should folder be added if missing, default is true
#' @param ...  Other arguments to passed on to `drive_mkdir`
#'
#' @return Googledrive folder item it or NULL for non existing folder
#' @export
#'
#' @examples
#' \dontrun{
#'  library(glamr)
#'
#'  gdrive_folder("Test-Folder", "ID-adfdfsdfdfdfs")
#' }
#'
gdrive_folder <- function(name,
                          path = NULL,
                          add = FALSE,
                          ...) {

  # Identify paths
  paths <- name %>%
    stringr::str_replace("^\\/", "") %>%
    stringr::str_replace("\\/$", "") %>%
    stringr::str_split("\\/") %>%
    base::unlist()

  # Default
  drive_id <- path

  # Check every single nested path
  for (p in paths) {

    # find drive folder
    path_id = googledrive::drive_ls(
      path = googledrive::as_id(drive_id),
      pattern = p)

    # Get number of items
    n <- base::nrow(path_id)

    # dubplicated folder
    if (n > 1) {
      drive_id <- NULL
      base::stop(glue::glue("Possible duplicated googledrive item: {p}"))
    }

    # target folder
    if (n == 1) {
      drive_id <- path_id$id
    }

    # create sub-folder, it not present
    if (n == 0 & add == TRUE) {
      # Create folder
      path_id <- googledrive::drive_mkdir(
        name = p,
        path = googledrive::as_id(drive_id),
        ...)

      drive_id <- path_id$id

      base::print(glue::glue("New drive folder: {p} => {drive_id}"))

    } else if (n == 0 & !add) {
      drive_id <- NULL
      base::message(crayon::red(glue::glue("Drive folder does not exist: {p}")))

      return(drive_id)
    }
  }

  # Return final id
  return(drive_id)
}

#' @title Update files
#'
#' @param filename
#' @param to_drive
#' @param to_folder
#' @param add
#' @param overwrite
#' @param ...
#'
#' @return Googledrive file(s) id(s)
#' @export
#'
#' @examples
#' \dontrun{
#'  library(glamr)
#'
#'  list.files("./Graphics", "NIGERIA", full.names = TRUE) %>%
#'       export_drivefile(filename = .,
#'                        to_drive = "<path-id>",
#'                        to_folder = "FY99Q4/VL Suppression",
#'                        add_folder = TRUE)
#'
#' }
#'
export_drivefile <- function(filename, to_drive,
                             to_folder = NULL,
                             add_folder = TRUE,
                             overwrite = TRUE,
                             ...) {

  # Track drive id
  drive_id <- to_drive

  # Identify sub-folder, if applicable
  if (!base::is.null(to_folder)) {

    # Get drive folder id
    drive_id <- gdrive_folder(name = to_folder,
                              path = to_drive,
                              add = add_folder)

    # Re-check id
    if (base::is.null(drive_id)) {
      base::stop("Could not identify drive folder")
    }

    # Folder id should be different
    if (drive_id == to_drive) {
      base::stop("Check drive folder name. ID is the same as parent path")
    }

    base::print(glue::glue("Exporting files to {to_folder} ..."))
  }

  # Convert id into google id
  drive_id <- drive_id %>% googledrive::as_id()

  # Upload file(s) as it is
  files <- filename %>%
    purrr::map_dfr(~googledrive::drive_upload(
      media = .x,
      path = drive_id,
      overwrite = overwrite,
      ...))

  # Upload files
  # if (!base::is.null(export_as)) {
  #
  #   files <- files %>%
  #     dplyr::pull(id) %>%
  #     map2_dfr(.x = .,
  #              .y = export_as,
  #              .f = ~googledrive::drive_rename(file = .x,
  #                                              name = .y,
  #                                              overwrite = overwrite))
  #
  # }

  return(files)
}
