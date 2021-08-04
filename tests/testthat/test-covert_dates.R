context("convert periods")

test_that("period converts", {
  expect_equal(tibble::tibble(Period = "Apr to Jun 2018") %>% convert_datim_pd_to_qtr() %>% dplyr::pull(), "FY18Q3")
  expect_equal(tibble::tibble(Period = "Oct to Dec 2018") %>% convert_datim_pd_to_qtr() %>% dplyr::pull(), "FY19Q1")
  expect_equal(tibble::tibble(Period = "Oct 2020 to Sep 2021") %>% convert_datim_pd_to_qtr() %>% dplyr::pull(), "FY21")
  expect_equal(tibble::tibble(Period = "July 2021 to June 2022") %>% convert_datim_pd_to_qtr() %>% dplyr::pull(), "July 2021 to June 2022")
})



