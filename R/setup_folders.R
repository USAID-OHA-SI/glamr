#' Setup Folder Structure in Project
#'
#' @param folder_list list of folders to install
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #standard
#'   folder_setup()
#' #specific
#'   fldrs <- c("Data", "Tableau", "AI")
#'   folder_setup(fldrs)
#' }
folder_setup <- function(folder_list = list("Data", "Images", "Scripts",
                                            "Dataout", "GIS", "Documents", "Graphics", "markdown")) {
  if(!is.list(folder_list))
    stop("Please provide a list of directories to create for the project.")
  print("The following directories will be created:")
  print(glue::glue(crayon::green('{folder_list}')))
  suppressWarnings(
  purrr::walk(folder_list, ~dir.create(.))
  )

}
