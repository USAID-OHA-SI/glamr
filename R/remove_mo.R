#' Remove M&O funding
#'
#' When working with financial data, its often useful to remove M&O
#' funding from the data.
#'
#' @param df financial or comprehensive budget dataset
#'
#' @return a dataset excluding M&O
#' @export
#'

remove_mo <- function(df){

  #check that mechanism exists in MSD before starting (OUxIM or PSNUxIM, not PSNU)
  if(any(c("mech_code", "mechanism_id") %in% names(df)) == FALSE) {
    stop('This dataset does not have mechanisms. Make sure it is OUxIM or PSNUxIM')
  }

  df<-df%>%
    dplyr::filter(record_type =="Implementing Mechanism")
  return(df)

}
