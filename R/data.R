#' Current PEPFAR Data Calendar
#'
#' A dataset the dates for the release of the MER Structured Dataset
#'
#' @format A data frame with 8 rows and 5 variables:
#' \describe{
#'   \item{fiscal_year}{fiscal year, start = October}
#'   \item{quarter}{fiscal quarter, integer 1-4}
#'   \item{type}{data release type, initial or clean}
#'   \item{entry_open}{date entry begins into DATIM}
#'   \item{entry_close}{date DATIM is closed and data are frozen}
#'   \item{msd_release}{date the MSD is released on PEPFAR Panorma}
#' }
#' @source \url{https://datim.zendesk.com/hc/en-us/articles/115001940503-PEPFAR-Data-Calendar}
"pepfar_data_calendar"


#' Current List of PEPFAR OUs/Countries and their ISO codes
#'
#' A dataset PEPFAR Operating Units and Countries along with their ISO codes.
#' This is a useful dataset for having a full set of PEPFAR countries or to
#' align ISO codes with external datasources. Pulled from DATIM and the FSD.
#'
#' The list of PEPFAR acceleration countries was defined by Amb. Nkengasong
#' during a DP's retreat for the [Zaidi 2023-06-08 re: Moving
#' countries to green!]. These are countries "where enhanced attention and
#' focus might help 'move the dial' on achieving and sustaining the
#' UNAIDS 95-95-95 targets by 2025"
#'
#' @format A data frame with 55 rows and 7 variables:
#' \describe{
#'   \item{operatingunit}{PEPFAR Operating Unit (countries + 3 regional programs)}
#'   \item{operatingunit_iso}{PEPFAR Operating Unit ISO-3}
#'   \item{operatingunit_uid}{PEPFAR Operating Unit unique id from DATIM}
#'   \item{country}{PEPFAR Country Name}
#'   \item{country_iso}{PEPFAR Country Name ISO-3}
#'   \item{country_uid}{PEPFAR Country unique id from DATIM}
#'   \item{pepfar_accel}{Is the country prioritized in the 8+1+1 group defined by SGAC?}
#' }
#' @source \url{https://final.datim.org/}
"pepfar_country_list"


#' Cross-walk of Country names
#'
#' A dataset PEPFAR Operating Units and Countries along with their ISO codes, alternative names from other sources.
#' This is a useful dataset designed to help with data cleaning / matching from different sources.
#'
#' @format A data frame with 55 rows and 29 variables:
#' \describe{
#'   \item{iso3c}{ISO-3 Code}
#'   \item{contient}{Continent name}
#'   \item{region}{World Region Name}
#'   \item{region23}{Alternative name for World Region}
#'   \item{un_region_name}{US Region Name}
#'   \item{iso_name_en}{ISO Country name in English}
#'   \item{iso_name_fr}{SO Country name in French}
#'   \item{country_name_de}{Regular Expression of Country name in German}
#'   \item{country_name_de_regrex}{Country name in German}
#'   \item{country_name_en}{Country name in English}
#'   \item{country_name_en_regrex}{Regular Expression of Country name in English}
#'   \item{un_name_ar}{UN Name in Arabic}
#'   \item{un_name_en}{UN Name in English}
#'   \item{un_name_es}{UN Name in Spanish}
#'   \item{un_name_fr}{UN Name in French}
#'   \item{un_name_ru}{UN Name in ru}
#'   \item{un_name_zh}{UN Name in zh}
#'   \item{sovereignt}{rnaturalearth sovereign territory name}
#'   \item{admin}{rnaturalearth administrative unit name}
#'   \item{name}{rnaturalearth country name}
#'   \item{operatingunit}{PEPFAR Operating Unit (countries + 3 regional programs)}
#'   \item{operatingunit_iso}{PEPFAR Operating Unit ISO-3}
#'   \item{operatingunit_uid}{PEPFAR Operating Unit unique id from DATIM}
#'   \item{country}{PEPFAR Country Name}
#'   \item{country_uid}{PEPFAR Country unique id from DATIM}
#' }
#' @source
#' \itemize{
#'   \url{https://final.datim.org/}
#'   \url{https://www.naturalearthdata.com/}
#'   \url{https://vincentarelbundock.github.io/countrycode/}
#' }
"pepfar_country_xwalk"
