
#' Extract MSD Source Information
#'
#' This function is used primarily to extract the data from the source file of
#' a MER Structured Dataset, MER NAT_SUBNAT StructuredDataset, Financial
#' Structure Dataset, or DATIM Genie export. It can also be used to extact
#' information from the filename about the fiscal year, quarter, or period.
#'
#' @param path path to the folder containing MSDs or specific MSD file
#' @param type not required unless providing a folder in `path`; default = "OU_IM_FY19"
#' other examples include: "PSNU_IM", "NAT_SUBNAT", "PSNU"
#' @param return from the info, what should be returned; default = "source"
#' other options are: "period", "fiscal_year", "quarter"
#'
#' @return vector of information related to what is being asked in `return`
#' @export
#'
#' @examples
#' \dontrun{
#'  source_info() #works if you have stored path to the MSD folder via glamr::set_paths()
#'  source_info("../Data", type = "PSNUxIM")
#'  source_info("../Data", type = "PSNUxIM", return = "period")
#'  source_info("../Downloads/Genie_PSNU_IM_Jupiter_Daily_c9f5889f-86c9-44e7-ab63-fa86c587d251.zip")
#'  source_info("../Data/MER_Structured_Datasets_NAT_SUBNAT_FY15-21_20210618_v2_1.rds") }
#'
#' \dontrun{
#' library(tidyverse)
#' library(glamr)
#' library(glue)
#'
#' df <- si_path() %>%
#'   return_latest("OU_IM_FY19") %>%
#'   read_rds()
#'
#' df_viz <- df %>%
#'   filter(operatingunit == "Saturn",
#'          indicator == "TX_NEW",
#'          standardizeddisaggregate == "Total Numerator") %>%
#'   count(fiscal_year, wt = targets, name = "targets")
#'
#' df_viz %>%
#'   ggplot(aes(fiscal_year, targets)) +
#'   geom_col() +
#'   labs(caption = glue("Source: {source_info()}")) }
source_info <- function(path, type, return = "source"){

  if(missing(path) && is.null(getOption("path_msd")))
    stop("No path to a file or folder was provided.")

  if(missing(path))
    path <- si_path()

  if(!file.exists(path))
    stop("File/folder do not exist or path is not correct.")

  if(file.info(path)$isdir && missing(type))
    type <- "OU_IM_FY19"

  if(file.info(path)$isdir)
    path <- return_latest(path, type)

  #strip out full filepath to just keep name
  file_name <- basename(path)

  #identify the type of file
  file_type <- dplyr::case_when(stringr::str_detect(file_name, "(Genie|NAT_SUBNAT)") ~ stringr::str_extract(path, "(Frozen|Daily|NAT_SUBNAT)"),
                                stringr::str_detect(file_name, "Financial_Structured_Datasets") ~ "FSD",
                                stringr::str_detect(file_name, "MER_Structured_Datasets") ~ "MSD")

  #capture the dataset date for use in figuring out relvant FY period
  file_date <- ifelse(stringr::str_detect(file_name, "Genie"),
                      file.info(path)$ctime %>% format("%Y-%m-%d"),
                      stringr::str_extract(file_name, "[:digit:]{8}"))

  #depending on the type, create a dataframe with relevant info
  if(file_type == "Frozen") {
    #frozen
    info <- pepfar_data_calendar %>%
      dplyr::filter(as.Date(entry_close) <= file_date) %>%
      dplyr::filter(entry_close == max(entry_close)) %>%
      dplyr::mutate(period = glue::glue("FY{stringr::str_sub(fiscal_year, -2)}Q{quarter}"),
                    source = glue::glue("{period}{stringr::str_sub(type, end = 1)} DATIM Genie [{file_date}]"))
  } else if(file_type == "Daily") {
    #daily
    info <- pepfar_data_calendar %>%
      tidyr::pivot_longer(dplyr::starts_with("entry"),
                          names_to = "datim_status",
                          names_prefix = "entry_",
                          values_to = "date") %>%
      dplyr::mutate(type = ifelse(datim_status == "open", "provisional", type),
                    date = as.Date(date)) %>%
      dplyr::filter(date <= as.Date(file_date)) %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::mutate(type = "provisional",
                    period = glue::glue("FY{stringr::str_sub(fiscal_year, -2)}Q{quarter}"),
                    source = glue::glue("{period}{stringr::str_sub(type, end = 1)} DATIM Genie [{file_date}]"))
  } else {
    #MSD/FSD/NAT_SUBNAT
    info <- pepfar_data_calendar %>%
      dplyr::mutate(entry_close = stringr::str_remove_all(entry_close, "-")) %>%
      dplyr::filter(entry_close == file_date) %>%
      dplyr::mutate(period = glue::glue("FY{stringr::str_sub(fiscal_year, -2)}Q{quarter}"),
                    source = glue::glue("{period}{stringr::str_sub(type, end = 1)} {file_type}"))

  }

  #extract key element
  info <- info[[return]]

  return(info)

}


