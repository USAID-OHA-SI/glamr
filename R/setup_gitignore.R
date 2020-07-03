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
    cat(".Rproj.user
        .Rhistory
        .RData
        .Ruserdata",
        file = ".gitignore")
  }

  cat("
      #no data
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
      GIS/*
      Images/*
      Graphics/*",
      file = ".gitignore",
      append = TRUE

  )

}
