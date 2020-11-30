#' Default SI Project Setup
#'
#' @return creates folders, readme, and gitignore
#' @export

si_setup <- function(){
  folder_setup()
  setup_gitignore()
  setup_readme()
}



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
folder_setup <- function(folder_list = list("Data", "Images", "Scripts", "AI",
                                            "Dataout", "GIS", "Documents", "Graphics", "markdown")) {
  if(!is.list(folder_list))
    stop("Please provide a list of directories to create for the project.")
  print("The following directories will be created:")
  print(glue::glue(crayon::green('{folder_list}')))
  suppressWarnings(
  purrr::walk(folder_list, ~dir.create(.))
  )

}


#' Setup gitignore in Project
#'
#' @return adds standard gitignore, plus specific ignores
#' @export
#'
#' @examples
#' \dontrun{
#'   setup_gitignore() }

setup_gitignore <- function(){

  if(!file.exists(".gitignore")){
    cat("#R basics
.Rproj.user
.Rhistory
.RData
.Ruserdata",
file = ".gitignore")
  }

  cat("\n#no data
*.csv
*.txt
*.rds
*.xlsx
*.xls
*.zip
*.png
*.twbx
*.pptx

#nothing from these folders
AI/*
GIS/*
Images/*
Graphics/*",
file = ".gitignore", append = TRUE

  )

}


#' Setup README with Disclaimer in Project
#'
#' @param add_disclaimer should the standard disclaimer be added, default = TRUE
#'
#' @return adds/appends disclaimer to README
#' @export
#'
#' @examples
#' \dontrun{
#' #standard (appends dislaimer if README exists)
#'   setup_readme() }

setup_readme <- function(add_disclaimer = TRUE){

  if(!file.exists("README.md"))
    usethis::use_readme_md()

  if(add_disclaimer == TRUE){
    cat(
      "---\n\n*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*",
      file = "README.md", append = TRUE)
  }
}
