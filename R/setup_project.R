#' Default SI Project Setup
#'
#' `si_setup()` combines three function - `folder_setup()`,
#' `setup_gitignore()`, and `setup_readme()` to create the base OHA/SI project
#' structure with the necessary folders, disclaimers, and ignored files.
#'
#' @return creates folders, readme, and gitignore
#' @export
#' @family project setup

si_setup <- function(){
  folder_setup()
  setup_gitignore()
  setup_readme()
}



#' Setup Folder Structure in Project
#'
#' `folder_setup()` creates an organizational structure that is common across
#' OHA/SI projects so every analyst knows what to expect and where when picking
#' up a new project or one cloned from a co-worker. This function can be used
#' as a stand alone function but primarily serves `si_setup()`
#'
#' The standard setup provides the following folders for these uses:
#'   * Data - where any raw/input data (**xlsx/csv/rds**) specific to the project are stored
#'   * Dataout - where any intermediary or final data (**xlsx/csv/rds**) are output as a product of your code
#'   * Data_public - where all public data lives *
#'   * Scripts - where all the code (**R/py**) are stored (if there is a local order, make sure to add prefixes to each script, e.g. 00_init.R, 01_data-access.R, 02_data-munging.R, ...)
#'   * Images - any **png/jpeg** visual outputs from your code
#'   * Graphics - any **svg/pdf** visual outputs that will be edited in vector graphics editor, eg Adobe Illustrator or Inkscape
#'   * AI - any **ai** files or other files from a graphics editor (exported pngs products will be stored in Images)
#'   * GIS - any **shp** files or other GIS releated inputs
#'   * Documents - any **docx/xlsx/pptx/pdf** documents that relate to the process or are final outputs
#'   * markdown - exported **md** files from a knitr report
#'
#' @param folder_list list of folders to install
#'
#' @export
#' @family project setup
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
                                            "Dataout", "Data_public", "GIS", "Documents", "Graphics", "markdown")) {
  if(!is.list(folder_list))
    stop("Please provide a list of directories to create for the project.")
  usethis::ui_info("The following directories will be created (if they do no already):")
  print(glue::glue(crayon::green('{folder_list}')))
  suppressWarnings(
  purrr::walk(folder_list, ~dir.create(.))
  )

}


#' Setup gitignore in Project
#'
#' `setup_gitignore()` creates a .gitignore file (or appends to an existing)
#' one) the standard file types/folders that should not be published to GitHub
#' due to the sensitive nature of PEPFAR data. This function can be used as a
#' stand alone function but primarily serves `si_setup()`.
#'
#' @return adds standard gitignore, plus specific ignores
#' @export
#'
#' @family project setup
#'
#' @examples
#' \dontrun{
#'   setup_gitignore() }

setup_gitignore <- function(){

  if(!file.exists(".gitignore")){
    usethis::ui_info("Adding a {usethis::ui_code('.gitignore')} file")
    file.create(".gitignore")
  }

  if(missing_section(".gitignore"))
    usethis::ui_info("Adding standard files types and folders to {usethis::ui_code('.gitignore')}")

  if(!file.exists(".gitignore") || missing_section(".gitignore")){
    cat("#R basics
.Rproj.user
.Rhistory
.RData
.Ruserdata
.httr-oauth
.DS_Store
.quarto

#no data
*.csv
*.gz
*.txt
*.rds
*.xlsx
*.xls
*.zip
*.png
*.pptx
*.tfl
*.twb
*.twbx
*.tbs
*.tbm
*.hyper
*.sql
*.parquet
*.svg
*.dfx
*.json
*.dta
*.shp
*.dbf

#nothing from these folders
AI/*
GIS/*
Images/*
Graphics/*
Data/*
Dataout/*",
        file = ".gitignore", append = TRUE)
  }


}


#' Setup README with Disclaimer in Project
#'
#' `setup_readme()` establishes a README.md with the standard USAID disclaimer
#' (or appends to one that currently exists). This function can be used as a
#' stand alone function but primarily serves `si_setup()`.
#'
#' @param add_disclaimer should the standard disclaimer be added, default = TRUE
#'
#' @return adds/appends disclaimer to README
#' @export
#'
#' @family project setup
#'
#' @examples
#' \dontrun{
#' #standard (appends dislaimer if README exists)
#'   setup_readme() }

setup_readme <- function(add_disclaimer = TRUE){

  if(!file.exists("README.md")){
    usethis::ui_info("Adding a {usethis::ui_code('README.md')} file")
    usethis::use_readme_md()
  }

  if(add_disclaimer == TRUE && missing_section("README.md")){
    usethis::ui_info("Adding standard disclaimer to {usethis::ui_code('README.md')}")
    cat(
      "\n---\n\n*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*",
      file = "README.md", append = TRUE)
  }
}



#' Check if standard text section is missing from file
#'
#' @param file either README.md or .gitignore
#' @keywords internal

missing_section <- function(file){

  suppressWarnings(text <- utils::read.delim(file, col.names = "x"))

  text <- text %>%
    dplyr::filter(stringr::str_detect(x,
          "Disclaimer: The findings|nothing from these folders")) %>%
    nrow()

  missing <- text == 0

  return(missing)
}
