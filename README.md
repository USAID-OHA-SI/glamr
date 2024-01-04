# glamr <img src='man/figures/logo.png' align="right" height="120" />

SI utilities package

<!-- badges: start -->
[![R-CMD-check](https://github.com/USAID-OHA-SI/glamr/workflows/R-CMD-check/badge.svg)](https://github.com/USAID-OHA-SI/glamr/actions)
[![glamr status badge](https://usaid-oha-si.r-universe.dev/badges/glamr)](https://usaid-oha-si.r-universe.dev/glamr)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![:name status badge](https://usaid-oha-si.r-universe.dev/badges/:name)](https://usaid-oha-si.r-universe.dev/)
<!-- badges: end -->

## Overview

When using PEPFAR data, the OHA SI PEPFAR by and large uses the same MER Structured Datasets to answer the same analytical questions each period. This package is a sister package of `ICPIutilities` for working primiarly with data from DATIM and the MER Structured Datasets and plotting them using `glitr`. Focal users are analysts in USAID/GH/OHA who are using R to pull data from DATIM or perform the same repeated functions each quarter like creating TX_NET_NEW targets or assessing achievement.


## Installation

`glamr` is not on CRAN, so you will have to install it directly from [rOpenSci](https://usaid-oha-si.r-universe.dev/packages) or [GitHub](https://github.com/USAID-OHA-SI/) using the code found below..

``` r
## SETUP

  #install from rOpenSci
    install.packages('glamr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
    
  #alt: install from GitHub using pak
    #install.packages("pak")
    #pak::pak("USAID-OHA-SI/glamr")
    
  #load the package
    library(glamr)

## LIST TYPES OF STYLES INCLUDED WITH PACKAGE
  ls("package:glamr")
```


---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*

