#' Remove Supply Chain Funding
#'
#' When working with financial data, its often useful to remove Supply Chain
#' mechanism and funding from the data.
#'
#' The list of SCH mechanisms is maintained by the EA team on a Google Sheet.
#' A USAID email is required to access the dataset.
#'
#' @param df this can be either a financial structured dataset or an MSD
#' @param poc you can choose to filter either the SCH list or the SGAC list;
#'  default is both
#' @param flag_only allows you keep full dataset, but identify the mechanisms
#' that are supply chain, mech_sch as a logical; default = FALSE
#'
#' @return a df without supply chain mechanisms
#' @seealso [set_email()] to store USAID email;
#'   [load_secrets()] to load credentials into session
#' @export
#'
#' @examples
#' \dontrun{
#' #authenticate
#' load_secrets()
#' #remove SCh using SGAC list
#' df <- remove_shc(df, poc = "SGAC") }

remove_sch <- function(df, poc= c("SCH","SGAC"), flag_only = FALSE){

  if ( !googlesheets4::gs4_has_token())
    stop("Function requires authentication,
         use googlesheets4::gs4_auth() or glamr::load_secrets()")


  sheet_id <- googlesheets4::as_sheets_id('1mCJWDo4FPW2cQ6LpbsSjtnRjT7sUpPEOqxfT2zQNo64')

  suppressMessages(
    df_check <- googlesheets4::read_sheet(sheet_id, "Cross check SGAC-SCGH")
    )

  lst_mech <- df_check%>%
    dplyr::filter(POC %in% poc)%>%
    dplyr::mutate(mech_id = as.character(`Mech ID`))%>%
    dplyr::distinct(mech_id)%>%
    dplyr::pull(mech_id)

  if(flag_only == FALSE) {
    df <- dplyr::filter(df, !mech_code %in% lst_mech)
  } else {
    df <- dplyr::mutate(df, mech_sch = mech_code %in% lst_mech)
  }

  return(df)
}
