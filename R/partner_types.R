#' A code to add in partner types
#'
#' @param df You can use either a MER or financial dataset
#'
#' @return a df with the partner types provided by USAID. This includes the updates for regional partners, which in FY21 are classified as local. These adjustments can be found on the adjusted column
#' @export
#'

partner_types<-function(df){
  
  if ( !googlesheets4::gs4_has_token())
    stop("Function requires authentication,
         use googlesheets4::gs4_auth() or glamr::load_secrets()")
  
  
  sheet_id<-'1tGk1TR8l3WacR8qMIK0AQvFynABijAaLHeIctE1nUoM'
df_Partners<- googlesheets4::read_sheet(googlesheets4::as_sheets_id(sheet_id))%>%
    dplyr::rename(`mech_code`=`Mechanism ID`)%>%
    dplyr::mutate(`mech_code`=as.character(`mech_code`))%>%
    dplyr::rename(`partner_type_usaid`=`Partner Type`)
 
  
  
  df<-dplyr::left_join(df,df_Partners,by="mech_code")
  
  #add in special partners
  df<-df%>%
    dplyr::mutate(`partner_type_usaid_adjusted`=`partner_type_usaid`)
  
   df<-df%>%
    dplyr::mutate(`partner_type_usaid_adjusted`=
                    case_when(`fiscal_year`=="2021" & `partner_type_usaid`== "Regional" ~ "Local"
                              ,TRUE ~ partner_type_usaid_adjusted))
  
  df<-df%>%
    dplyr::mutate(`partner_type_usaid_adjusted`=
                    case_when(`fiscal_year`!="2021" & `partner_type_usaid`== "Regional" ~ "International"
                              ,TRUE ~ partner_type_usaid_adjusted))
    
   
  
  return(df)
}



