<!-- badges: start -->
[![Travis build status](https://travis-ci.org/USAID-OHA-SI/glamr.svg?branch=master)](https://travis-ci.org/USAID-OHA-SI/glamr)
<!-- badges: end -->

<img src='man/figures/logo.png' align="right" height="120" />

# glamr
SI utilities package

## Overview

When using PEPFAR data, the OHA SI PEPFAR by and large uses the same MER Structured Datasets to answer the same analytical questions each period. This package is a sister package of `ICPIutilities` for working primiarly with data from DATIM and the MER Structured Datasets and plotting them using `glitr`. Focal users are analysts in USAID/GH/OHA who are using R to pull data from DATIM or perform the same repeated functions each quarter like creating TX_NET_NEW targets or assessing achievement.


## Installation

`glamr` is not on CRAN, so you will have to install it directly from Github using devtools.

If you do not have the devtools package installed, you will have to run the `install.packages("devtools")` line in the code below as well.

``` r
## SETUP

  #install
    install.packages("devtools")
    devtools::install_github("USAID-OHA-SI/glamr")
    
  #load the package
    library(glamr)

## LIST TYPES OF STYLES INCLUDED WITH PACKAGE
  ls(package:glamr)
```


---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*

