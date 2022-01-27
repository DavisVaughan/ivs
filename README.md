
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iv

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/DavisVaughan/iv/branch/main/graph/badge.svg)](https://app.codecov.io/gh/DavisVaughan/iv?branch=main)
[![R-CMD-check](https://github.com/DavisVaughan/iv/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DavisVaughan/iv/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

iv is a package dedicated to working with intervals in a generic way. It
introduces a new type, the **i**nterval **v**ector, which is generally
referred to as an **iv**. An iv is created from two parallel vectors
representing the starts (inclusive) and ends (exclusive) of the
intervals, like this:

``` r
library(iv)

# Interval vector of integers
iv(1:5, 7:11)
#> <iv<integer>[5]>
#> [1] [1, 7)  [2, 8)  [3, 9)  [4, 10) [5, 11)

# Interval vector of dates
starts <- as.Date("2019-01-01") + 0:2
ends <- starts + c(2, 5, 10)

iv(starts, ends)
#> <iv<date>[3]>
#> [1] [2019-01-01, 2019-01-03) [2019-01-02, 2019-01-07) [2019-01-03, 2019-01-13)
```

Interval vectors are completely generic, meaning that you can create
them from any comparable type that is supported by
[vctrs](https://vctrs.r-lib.org). This means that user defined S3 types
work automatically, like `hms::hms()`, `bit64::integer64()`, and
`clock::year_month_day()`.

## Installation

You can install the development version of iv from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DavisVaughan/iv")
```

## Inspiration

This package was inspired by many sources!

-   [IRanges](https://github.com/Bioconductor/IRanges) is the closest
    equivalent, and inspired many of the function names seen here. It is
    mainly focused on integer intervals, and always uses closed
    intervals. It is also based on S4, and unfortunately that currently
    means it can’t be used as a column in a tibble with the current
    limitations in vctrs.

-   [intervals](https://github.com/edzer/intervals) is another R package
    that supports intervals. It supports integer/numeric intervals and
    allows for varying the endpoint bounds.

-   [Maintaining Knowledge about Temporal
    Intervals](https://cse.unl.edu/~choueiry/Documents/Allen-CACM1983.pdf)
    is a paper by James Allen that `iv_locate_relates()` is based on. It
    is a great primer on interval algebra.

-   [Why numbering should start at
    0](https://www.cs.utexas.edu/users/EWD/transcriptions/EWD08xx/EWD831.html)
    is a small white paper that describes why right-open intervals are
    often the best choice.
