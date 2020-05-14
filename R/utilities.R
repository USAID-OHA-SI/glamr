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
#' @return A boolean or stops
#' @export

check_status <- function(res){

  if(httr::status_code(res) == 200)
    stop("The API returned an error")
}
