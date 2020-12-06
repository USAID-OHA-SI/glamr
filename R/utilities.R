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

#' Test if service is loaded in current session
#'
#' @param service account, either "datim" or "email"
#'
#' @return A boolean
#' @export
#'

is_loaded <- function(service = c("datim", "email")){

  !is.null(getOption({{service}}))

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
