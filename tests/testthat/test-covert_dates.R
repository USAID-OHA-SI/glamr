context("convert periods")

test_that("period converts", {

  #new format
  expect_equal(tibble::tibble(Period = "October - December 2023") %>% convert_datim_pd_to_qtr() %>% dplyr::pull(), "FY24Q1")
  expect_equal(tibble::tibble(Period = "January - March 2024") %>% convert_datim_pd_to_qtr() %>% dplyr::pull(), "FY24Q2")
  expect_equal(tibble::tibble(Period = "July - September 2024") %>% convert_datim_pd_to_qtr() %>% dplyr::pull(), "FY24Q4")
  expect_equal(tibble::tibble(Period = "October 2023 - September 2024") %>% convert_datim_pd_to_qtr() %>% dplyr::pull(), "FY24")

  #old format
  expect_equal(tibble::tibble(Period = "Apr to Jun 2018") %>% convert_datim_pd_to_qtr() %>% dplyr::pull(), "FY18Q3")
  expect_equal(tibble::tibble(Period = "Oct to Dec 2018") %>% convert_datim_pd_to_qtr() %>% dplyr::pull(), "FY19Q1")
  expect_equal(tibble::tibble(Period = "Oct 2020 to Sep 2021") %>% convert_datim_pd_to_qtr() %>% dplyr::pull(), "FY21")

  #shouldn't process
  expect_equal(tibble::tibble(Period = "July 2021 to June 2022") %>% convert_datim_pd_to_qtr() %>% dplyr::pull(), "July 2021 to June 2022")
})



