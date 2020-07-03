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
      "---

      *Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*",
      file = "README.md", append = TRUE)
  }
}
