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
