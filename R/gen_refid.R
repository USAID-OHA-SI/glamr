#' Generate Reference ID
#'
#' This function returns a unique reference id that can be used in scripts and
#' cited in associated plots to help find the associated code on GitHub.
#'
#' A best practice would be to store the character string output as an object
#' called `ref_id` in the top matter of your script. If you run
#' `gophr::get_metadata` after this is stored as an object, it will
#' automatically store this for use in `metadata$caption`.
#'
#' @return 8 character string
#' @export
#'
#' @examples \dontrun{
#' library(glamr)
#' library(gophr)
#' library(ggplot2)
#' library(glue)
#' #create a reference id to include in a plot
#' gen_ref_id()
#' ref_id <- "1e64716c"
#' get_metadata()
#' #plot with ref id
#' ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point() +
#'   labs(caption = glue("Source: Edgar Anderson's Iris Data | Ref id: {ref_id}"))
#' Or
#' ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point() +
#'   labs(caption = metadata$caption)
#' }
gen_ref_id <- function(){
  Sys.time() %>%
    digest::sha1() %>%
    substr(start = 1, stop = 8)
}
