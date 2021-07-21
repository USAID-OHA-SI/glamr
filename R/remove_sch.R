
#' A Function to Pull Out Supply Chain Data
#'
#' @param df this can be either a financial strucutured dataset or an MSD
#' @param poc you can choose to filter either the SCH list, the SGAC list; default is both
#'
#' @return a df without supply chain mechanisms
#' @export
#'
#'
remove_sch<-function(df,poc=c("SCH","SGAC")){

  if ( !googlesheets4::gs4_has_token())
    stop("Function requires authentication,
         use googlesheets4::gs4_auth() or glamr::load_secrets()")


  sheet_id<-'1mCJWDo4FPW2cQ6LpbsSjtnRjT7sUpPEOqxfT2zQNo64'
  suppressMessages(df_check <- googlesheets4::read_sheet(googlesheets4::as_sheets_id(sheet_id), "Cross check SGAC-SCGH"))

  LST_Mech<-df_check%>%
    dplyr::filter(POC %in% poc)%>%
    dplyr::mutate(`Mech ID`=as.character(`Mech ID`))%>%
    dplyr::distinct(`Mech ID`)%>%
    dplyr::pull(`Mech ID`)

  df<-df%>%
    dplyr::filter(!mech_code %in% LST_Mech)

  return(df)
}
