#' Check internet connection
#'
#' @return A boolean or stops
#' @export

check_internet <- function(){

  if(!curl::has_internet())
    stop("No internet connection. Cannot excecute API.")
}


#' Test status of query
#'
#' @param res response
#'
#' @return A boolean or stops
#' @export

check_status <- function(res){

  if(httr::status_code(res) == 200)
    stop("The API returned an error")
}


#' Test if service is stored in credential manager
#'
#' @param service account, either "datim" or "email"
#'
#' @return A boolean
#' @export
#'

is_stored <- function(service = c("datim", "email")){

  accounts <- keyring::key_list()$service

  {{service}} %in% accounts

}

#' Test if option variable is loaded in current session
#'
#' @param opt_var option variable ("datim", "email", "path_msd", "path_datim", "path_vector", "path_raster")
#'
#' @return A boolean
#' @export
#'

is_loaded <- function(opt_var){

  !is.null(getOption({{opt_var}}))

}


#' Test if package is installed locally
#'
#' @param package name of R package to check
#'
#' @return A boolean
#' @export

is_installed <- function(package){
  {{package}} %in% rownames(installed.packages())
}


#' @title connect_text
#'
#' @param txt       String charactors
#' @param connector Charactor used as connector
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' connect_text("THIS - is complex (very bad)")}
#'
connect_text <- function(txt,
                         connections = "[^a-zA-Z0-9]",
                         connector = "_") {

  text <- base::sapply(txt, function(x) {
    x %>%
      stringr::str_split(connections) %>%
      base::unlist() %>%
      stringi::stri_remove_empty() %>%
      base::paste0(collapse = connector)
  }, USE.NAMES = FALSE)

  return(text)
}
