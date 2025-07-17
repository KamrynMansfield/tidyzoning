
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zoneR

<!-- badges: start -->

<!-- badges: end -->

zoneR helps analyze the housing capacity of cities using data that
follow the Open Zoning Feed Specification (OZFS) standards.

## Installation

You can install the development version of zoneR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KamrynMansfield/tidyzoning")
```

## Introduction

The main function of zoneR is `zr_run_zoning_checks()` which reads in a
`.bldg`, `.parcel`, and `.zoning` file. It calculates the zoning
requirements for each parcel, checks them against the building
characteristics, and returns a data frame stating if the parcel is
allowed or not.

Inside the `zr_run_zoning_checks()` function, different variables are
made from the OZFS files and used in other functions to perform the
checks.

**bldg_data:** The `.bldg` file read in as a list using
`rjson::fromJSON()`

**parcel_dims:** Created from the `zr_get_parcel_dims()` function. A
simple features data frame with all the centroid and dimensional data
from the `.parcel` file. It contains one row per parcel.

**parcel_data:** One row of the parcel_dims data frame representing a
unique parcel

**parcel_geo:** Created from the `zr_get_parcel_geo()` function. A
simple features data frame containing the geometry of each parcel side
without the centroid or dimensional data.

**zoning_data:** The data in the `.zoning` file read in as a simple
features data frame using `sf::st_read()`.

**district_data:** One row of the zoning_data data frame representing a
unique district.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(zoneR)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
