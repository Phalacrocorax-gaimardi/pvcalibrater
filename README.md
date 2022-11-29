
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pvcalibrater

<!-- badges: start -->
<!-- badges: end -->

pvcalibrater calibrates an agent-based-model for solar photo-voltaic
adoption by Irish households.

## Installation

You can install the development version of pvcalibrater from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Phalacrocorax-gaimardi/pvcalibrater")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(pvcalibrater)
## survey questions
head(pv_qanda)
#>     code question answercode answer
#> 1    age      AGE          1  18-24
#> 2    age      AGE          2  25-34
#> 3    age      AGE          3  35-44
#> 4    age      AGE          4  45-54
#> 5    age      AGE          5    55+
#> 6 gender   Gender          1   Male
## survey data
head(pv_data)
#>   ID age gender class region qc1 q1 q3 q5 q7 q9_1 q10b q15 q16 q17b q17c q17a_1
#> 1  1   2      1     1      2   1  1  1  1  1    1    2   2   5    2    1      1
#> 2  2   3      2     2      3   1  4  1  3  1    4    2   3   6    2    5      5
#> 3  3   2      2     1      3   2  1  1  2  1   NA    3   3   2    3    2      2
#> 4  4   4      2     2      1   2  3  2  1  2    4    1   2   6    3    3      3
#> 5  5   3      2     1      1   1  2  2  3  3   NA    3   4   6    3    3      1
#> 6  6   3      1     1      1   2  2  1  1  1    3    2   5   1    2    1      1
#>   q17a_2 q17a_3 q17a_5 qsp20 qsp21 qj qk qh qg qf qsp22_7
#> 1      1      1      1     1     1  3  3  1  2  1       1
#> 2      5      5      5     5     2  3  6  4  2  3       1
#> 3      2      2      2     2     1  2  6  2  2  2       2
#> 4      2      3      4     3     1  2  3  8  2  1       2
#> 5      1      1      3     4     1  4  5  3  2  4       2
#> 6      1      1      5     4     1  2  8  3  1  3       1
## restrict surey data to owner-occupiers (perhaps with a mortgage, but excluding apartments) 
pv_data_oo <- pv_data %>% dplyr::filter(q1 %in% 2:4,q3 %in% 1:2)
##transform from likelihood to Likert scores to adoption utilities
pv_util <- transform_to_utils(pv_data)
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
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
