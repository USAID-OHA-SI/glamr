## code to prepare `pepfar_data_calendar`

#source: https://datim.zendesk.com/hc/en-us/articles/115001940503-PEPFAR-Data-Calendar

pepfar_data_calendar <-
 tibble::tribble(
    ~fiscal_year, ~quarter,     ~type,  ~entry_open, ~entry_close, ~msd_release,
           2021L,       1L, "initial", "2021-01-06", "2021-02-12", "2021-02-19",
           2021L,       1L,   "clean", "2021-03-01", "2021-03-19", "2021-03-26",
           2021L,       2L, "initial", "2021-04-01", "2021-05-14", "2021-05-21",
           2021L,       2L,   "clean", "2021-06-01", "2021-06-18", "2021-06-25",
           2021L,       3L, "initial", "2021-07-01", "2021-08-13", "2021-08-20",
           2021L,       3L,   "clean", "2021-09-01", "2021-09-17", "2021-09-24",
           2021L,       4L, "initial", "2021-10-01", "2021-11-12", "2021-11-19",
           2021L,       4L,   "clean", "2021-12-01", "2021-12-17", "2022-01-07",
           2022L,       1L, "initial", "2022-01-05", "2022-02-11", "2022-02-18",
           2022L,       1L,   "clean", "2022-03-01", "2022-03-18", "2022-03-25",
           2022L,       2L, "initial", "2022-04-01", "2022-05-13", "2022-05-20",
           2022L,       2L,   "clean", "2022-06-01", "2022-06-17", "2022-06-24",
           2022L,       3L, "initial", "2022-07-01", "2022-08-12", "2022-08-19",
           2022L,       3L,   "clean", "2022-09-01", "2022-09-16", "2022-09-23",
           2022L,       4L, "initial", "2022-10-03", "2022-11-10", "2022-11-22",
           2022L,       4L,   "clean", "2022-12-01", "2022-12-16", "2023-01-06",
           2023L,       1L, "initial", "2023-01-04", "2023-02-10", "2023-02-17",
           2023L,       1L,   "clean", "2023-02-28", "2023-03-17", "2023-03-24",
           2023L,       2L, "initial", "2023-03-31", "2023-05-12", "2023-05-19",
           2023L,       2L,   "clean", "2023-05-31", "2023-06-16", "2023-06-27",
           2023L,       3L, "initial", "2023-06-30", "2023-08-11", "2023-08-18",
           2023L,       3L,   "clean", "2023-08-31", "2023-09-15", "2023-09-22",
           2023L,       4L, "initial", "2023-10-02", "2023-11-14", "2023-11-21",
           2023L,       4L,   "clean", "2023-11-30", "2023-11-15", "2024-01-05"
    ) %>%
  dplyr::filter(fiscal_year >= max(fiscal_year) - 1) %>%
  dplyr::mutate(dplyr::across(c(entry_open, entry_close, msd_release), as.Date))

usethis::use_data(pepfar_data_calendar, overwrite = TRUE)


## PEPFAR country list

library(tidyverse)
devtools::load_all()

gophr::get_metadata()

# df_msd <- si_path() %>%
#   return_latest("OU_IM") %>%
#   gophr::read_msd()

# df_cntry <- df_msd %>%
#   filter(fiscal_year == curr_fy,
#          funding_agency %ni% c("Dedup", "Default"),
#          indicator != "TX_NET_NEW") %>%
#   distinct(operatingunit, country)

df_fsd <- si_path() %>%
  return_latest("Financial") %>%
  gophr::read_psd()

df_cntry <- df_fsd %>%
  filter(fiscal_year == metadata$curr_fy,
         str_detect(country, "Region", negate = TRUE),
         cop_budget_total > 0,
         funding_agency %ni% c("Dedup", "Default")) %>%
  distinct(operatingunit, country)


ou_table <- grabr::get_outable(datim_user(), datim_pwd())  %>%
  select(operatingunit, operatingunit_iso, operatingunit_uid,
         country, country_iso, country_uid)

pepfar_country_list <- df_cntry %>%
  left_join(ou_table, by = c("operatingunit", "country")) %>%
  relocate(starts_with("op")) %>%
  arrange(operatingunit, country)

#Amb Nkengasong's 8 + 1 + 1 countries (2023-06-09 re:Moving countries to green!)
pepfar_focus <- c("Cameroon", "Cote d'Ivoire", "Ethiopia", "Kenya",
                  "Mozambique", "Nigeria", "Tanzania", "Zambia",
                  "South Africa", "Philippines")

pepfar_country_list <- pepfar_country_list %>%
  mutate(pepfar_accel = country %in% pepfar_focus)

usethis::use_data(pepfar_country_list, overwrite = TRUE)


## PEPFAR, UN and NE countries cross-walk

  library(tidyverse)
  library(glamr)
  library(countrycode)
  library(rnaturalearth)
  library(janitor)

# PEPFAR Countries
  cntries <- glamr::pepfar_country_list

# NE Countries
  ne_cntries <- rnaturalearth::ne_countries() %>%
    sf::st_as_sf() %>%
    sf::st_drop_geometry() %>%
    tibble::as_tibble() %>%
    select(iso_a3, sovereignt, admin, name)

# Countrycode
  cntrycodes <- countrycode::codelist %>%
    select(iso3c, continent, region, region23, un.region.name,
           starts_with("iso.name"),
           starts_with("country.name"),
           starts_with("un.name"))

# Combine and keep only PEPFAR Countries
  pepfar_country_xwalk <- cntrycodes %>%
    filter(!is.na(iso3c)) %>%
    left_join(ne_cntries %>% filter(!is.na(iso_a3)),
              by = c("iso3c" = "iso_a3")) %>%
    left_join(cntries, by = c("iso3c" = "country_iso")) %>%
    filter(!is.na(operatingunit_iso)) %>%
    clean_names()

# publish dataset
  usethis::use_data(pepfar_country_xwalk, overwrite = TRUE)
