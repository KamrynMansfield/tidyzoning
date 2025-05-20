
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyzoning <a href="https://kamrynmansfield.github.io/tidyzoning/"><img src="man/figures/logo.png" align="right" height="136" alt="tidyzoning website" /></a>

<!-- badges: start -->

<!-- badges: end -->

The purpose of the tidyzoning package is to analyze housing capacity
under current zoning laws. It is designed to read three types of
datasets.

- **tidybuilding:** A json file with attributes to represent a building

- **tidyzoning:** A geojson file of city zoning codes formatted to
  follow the Open Zoning Feed Specification

- **tidyparcel:** A geojson of with the city’s parcel data with labeled
  parcel sides (front, side interior, side exterior, rear) and parcel
  dimensions (lot_width, lot_depth, lot_area)

The functions of tiyzoning allow the user to find which parcels allow
the specified building based on the zoning regulations of each parcel.

## Installation

You can install the development version of tidyzoning from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KamrynMansfield/tidyzoning")
```

## Example

``` r
library(tidyzoning)
unify_tidybuilding("inst/extdata/bldg_12_fam.json")
#>   height width depth roof_type parking height_eave stories total_units     type
#> 1     60    65    76      flat       8          60       4          12 4_family
#>   gross_fl_area total_bedrooms fl_area_first fl_area_top units_0bed units_1bed
#> 1         13200             23          4400        4400          0          1
#>   units_2bed units_3bed units_4bed
#> 1         11          0          0
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

<!-- You can also embed plots, for example: -->

<!-- ```{r pressure, echo = FALSE} -->

<!-- plot(pressure) -->

<!-- ``` -->

<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
