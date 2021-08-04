## code to prepare `pepfar_data_calendar`

#source: https://datim.zendesk.com/hc/en-us/articles/115001940503-PEPFAR-Data-Calendar

pepfar_data_calendar <-
  tibble::tribble(
    ~fiscal_year, ~quarter,     ~type,  ~entry_open, ~entry_close,
           2021L,       1L, "initial", "2021-01-06", "2021-02-12",
           2021L,       1L,   "clean", "2021-03-01", "2021-03-19",
           2021L,       2L, "initial", "2021-04-01", "2021-05-14",
           2021L,       2L,   "clean", "2021-06-01", "2021-06-18",
           2021L,       3L, "initial", "2021-07-01", "2021-08-13",
           2021L,       3L,   "clean", "2021-09-01", "2021-09-17",
           2021L,       4L, "initial", "2021-10-01", "2021-11-12",
           2021L,       4L,   "clean", "2021-12-01", "2021-12-17"
    )




usethis::use_data(pepfar_data_calendar, overwrite = TRUE)


## PEPFAR country list

library(tidyverse)
library(glamr)

curr_fy <- source_info(return = "fiscal_year")

df_msd <- si_path() %>%
  return_latest("OU_IM") %>%
  read_rds()

df_cntry <- df_msd %>%
  filter(fiscal_year == curr_fy,
         fundingagency %ni% c("Dedup", "Default"),
         indicator != "TX_NET_NEW") %>%
  distinct(operatingunit, countryname)

ou_table <- get_outable(datim_user(), datim_pwd())  %>%
  select(operatingunit, operatingunit_iso, countryname, countryname_iso)

pepfar_country_list <- df_cntry %>%
  left_join(ou_table, by = c("operatingunit", "countryname")) %>%
  relocate(starts_with("op")) %>%
  arrange(operatingunit, countryname)


usethis::use_data(pepfar_country_list, overwrite = TRUE)
