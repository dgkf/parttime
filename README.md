
# parttime

<!-- badges: start -->

[![status](https://img.shields.io/static/v1?label=status&message=experimental&color=red)]()
[![R-CMD-check](https://github.com/dgkf/parttime/workflows/R-CMD-check/badge.svg)](https://github.com/dgkf/parttime/actions)
[![Coverage](https://codecov.io/gh/dgkf/parttime/branch/master/graph/badge.svg)](https://app.codecov.io/gh/dgkf/parttime?branch=master)
<!-- badges: end -->

A package for a partial datetime class and generics

# Installation

``` r
devtools::install_github("dgkf/parttime")
```

# Quick Start

The `parttime` package aims to make uncertainty in datetimes a central
feature by offering the `partial_time` datetime class.

This includes:

-   parsing of a wider range of datetime string formats
-   internal representations that captures date component missingness
-   overloading of operators for comparison
-   mechanisms for resolving datetime uncertainty
-   imputation

## Overview

`partial_time`s can be parsed from strings. Any missing data is not
immediately imputed with a known date. Instead, its uncertainty is
preserved as a central part of the `partial_time` class.

``` r
list(pttms <- as.parttime(c("2022", "2022-02")))
```

    ## [[1]]
    ## <partial_time<YMDhms+tz>[2]> 
    ## [1] "2022"    "2022-02"

We can access the components of each datetime as though the
`partial_time` is a matrix of datetime fields, or using
`lubridate`-style accessors and assignment functions.

``` r
pttms[, "year"]
##    2022 2022-02 
##    2022    2022

pttms[[1, "year"]]
## [1] 2022

year(pttms)  # the first row are names of elements in a named numeric vector
##    2022 2022-02 
##    2022    2022

year(pttms[1])
## [1] 2022

month(pttms[2]) <- 3
pttms
## <partial_time<YMDhms+tz>[2]> 
## [1] "2022"    "2022-03"

month(pttms[1]) <- 3
pttms
## <partial_time<YMDhms+tz>[2]> 
## [1] "2022-03" "2022-03"

month(pttms) <- NA
pttms
## <partial_time<YMDhms+tz>[2]> 
## [1] "2022" "2022"
```

Because `partial_time` objects may have uncertainty, comparison between
times conveys this uncertainty. As a brief example, if we compare our
dates from above we see that it is unclear whether one is greater-than
the other.

``` r
list(pttms <- as.parttime(c("2022", "2022-02")))
```

    ## [[1]]
    ## <partial_time<YMDhms+tz>[2]> 
    ## [1] "2022"    "2022-02"

``` r
pttms[1] > pttms[2]
## [1] NA

pttms[2] > pttms[1]
## [1] NA
```

This is because `"2022"` could be any date within the calendar year (and
even outside the calendar year if the timezone is unknown!, see
[below](#partial-datetime-comparisons)). In this sense, there are two
other modes of comparison - to determine whether a `partial_time`
*possibly* or *definitely* satisfies a criteria.

``` r
definitely(pttms[1] > pttms[2])
## [1] FALSE

possibly(pttms[2] > pttms[1])
## [1] TRUE
```

As well, a few helper functions are provided to perform imputation. All
imputation functions are wrappers around `impute_time` with varying
defaults for default timestamp and resolution to which imputation is
performed.

``` r
impute_date_max(pttms[2])  # resolve date fields with maximum value
## <partial_time<YMDhms+tz>[1]> 
## [1] "2022-02-28"

impute_time(pttms[1], "1999-06-05T04:03:02")  # arbitrary imputation
## <partial_time<YMDhmsZ>[1]> 
## [1] "2022-06-05 04:03:02.000"
```

## The `partial_time` class

`partial_time`s are like any other time, but may include `NA`s for some
of their fields. For example, `"1999"` only tells us information about a
year, the month, day, hour, etc. are still unknown. `partial_time`s
should be used for situations when a specific point in time is intended,
but exactly when it occurred is unknown.

## The `timespan` class

Similarly, a `timespan` class is offered, which is meant to represent a
range of times, denoted by a starting and ending `partial_time`.
Timespans might represent a range from the start to the end of a day,
like a `partial_time`, but can also represent ranges where the start and
end are partial times with different resolution.

# Examples

## Parsing Incomplete Timestamps

Parse ISO8601 timestampes using the `parsedate` package’s parser, but
retains information about missingness in the timestamp format.

``` r
iso8601_dates <- c(
  NA,
  "2001",
  "2002-01-01",
  "2004-245", # yearday
  "2005-W13",  # yearweek
  "2006-W02-5",  # yearweek + weekday
  "2007-10-01T08",
  "2008-09-20T08:35",
  "2009-08-12T08:35.048",  # fractional minute
  "2010-07-22T08:35:32",
  "2011-06-13T08:35:32.123",  # fractional second
  "2012-05-23T08:35:32.123Z",  # Zulu time
  "2013-04-14T08:35:32.123+05",  # time offset from GMT
  "2014-03-24T08:35:32.123+05:30",  # time offset with min from GMT
  "20150101T08:35:32.123+05:30"  # condensed form
)

as.parttime(iso8601_dates)
```

    ## <partial_time<YMDhms+tz>[15]> 
    ##  [1] NA                             "2001"                        
    ##  [3] "2002-01-01"                   "2004-09-01"                  
    ##  [5] "2005"                         "2006-01-13"                  
    ##  [7] "2007-10-01 08"                "2008-09-20 08:35"            
    ##  [9] "2009-08-12 08:35:02.880"      "2010-07-22 08:35:32.000"     
    ## [11] "2011-06-13 08:35:32.123"      "2012-05-23 08:35:32.123+0000"
    ## [13] "2013-04-14 08:35:32.123+0500" "2014-03-24 08:35:32.123+0530"
    ## [15] "2015-01-01 08:35:32.123+0530"

## Imputing Timestamps

``` r
impute_time("2019", "2000-01-02T03:04:05.006+0730")
```

    ## <partial_time<YMDhms+0730>[1]> 
    ## [1] "2019-01-02 03:04:05.006"

## Partial Datetime Comparisons

Partial timestamps include uncertainty, which means that there is often
uncertainty when comparing between timestamps. To help resolve this
uncertainty there are two helper functions, `possibly` and `definitely`
resolving this uncertainty for when the windows of uncertainty overlap,
or equal (to a given resolution).

``` r
options(parttime.assume_tz_offset = 0)  # assume GMT
parttime(2019) < parttime(2020)
```

    ## [1] TRUE

``` r
options(parttime.assume_tz_offset = NA)  # don't assume a timezone
parttime(2019) < parttime(2020)
```

    ## [1] NA

``` r
possibly(parttime(2019) < parttime(2020))
```

    ## [1] TRUE

``` r
definitely(parttime(2019) < parttime(2020))
```

    ## [1] FALSE

Given uncertainty in timestamps, we can’t be sure these are equal. In
this situation, `==` will return `NA`.

``` r
parttime(2019) == parttime(2019)
```

    ## [1] NA

``` r
options(parttime.assume_tz_offset = 0)
definitely(parttime(2019) == parttime(2019), by = "year")
```

    ## [1] TRUE

``` r
options(parttime.assume_tz_offset = NA)
definitely(parttime(2019) == parttime(2019), by = "year")
```

    ## [1] FALSE

## Timespans

Cast a partial time’s missingness to a range of possible values

``` r
as.timespan(parttime(2019))
```

    ## <timespan[1]>
    ## [1] [2019 — 2020)

## Tidyverse Compatible `vctrs`

`tibble`-style formatting makes it easy to see which components of each
`partial_time` are missing.

``` r
library(dplyr)

tibble(dates = iso8601_dates) %>%
  mutate(
    parttimes = as.parttime(dates),
    imputed_times = impute_time_min(parttimes)
  )
```

    ## # A tibble: 15 × 3
    ##    dates               parttimes                    imputed_times               
    ##    <chr>               <pttm>                       <pttm>                      
    ##  1 <NA>                NA                           NA                          
    ##  2 2001                2001                         2001-01-01 00:00:00.000-1200
    ##  3 2002-01-01          2002-01-01                   2002-01-01 00:00:00.000-1200
    ##  4 2004-245            2004-09-01                   2004-09-01 00:00:00.000-1200
    ##  5 2005-W13            2005                         2005-01-01 00:00:00.000-1200
    ##  6 2006-W02-5          2006-01-13                   2006-01-13 00:00:00.000-1200
    ##  7 2007-10-01T08       2007-10-01 08                2007-10-01 08:00:00.000-1200
    ##  8 2008-09-20T08:35    2008-09-20 08:35             2008-09-20 08:35:00.000-1200
    ##  9 2009-08-12T08:35.0… 2009-08-12 08:35:02.880      2009-08-12 08:35:02.880-1200
    ## 10 2010-07-22T08:35:32 2010-07-22 08:35:32.000      2010-07-22 08:35:32.000-1200
    ## 11 2011-06-13T08:35:3… 2011-06-13 08:35:32.123      2011-06-13 08:35:32.123-1200
    ## 12 2012-05-23T08:35:3… 2012-05-23 08:35:32.123+0000 2012-05-23 08:35:32.123+0000
    ## 13 2013-04-14T08:35:3… 2013-04-14 08:35:32.123+0500 2013-04-14 08:35:32.123+0500
    ## 14 2014-03-24T08:35:3… 2014-03-24 08:35:32.123+0530 2014-03-24 08:35:32.123+0530
    ## 15 20150101T08:35:32.… 2015-01-01 08:35:32.123+0530 2015-01-01 08:35:32.123+0530

# Roadmap

## Summary

The `partial_time` class is pretty complete. The `timespan` and
`partial_difftime` classes are still under construction!

## In-development :construction:

| status                  | class              | function/op                                   | description                |
|-------------------------|--------------------|-----------------------------------------------|----------------------------|
| :ballot_box_with_check: | `partial_time`     | `parttime`                                    | create `partial_time`      |
| :ballot_box_with_check: | `partial_time`     | `as.parttime`                                 | cast to `partial_time`     |
| :ballot_box_with_check: | `partial_time`     | `>`,`<`,`<=`,`>=`                             | comparison operators       |
| :ballot_box_with_check: | `partial_time`     | `possibly`,`definitely`                       | uncertainty resolvers      |
| :ballot_box_with_check: | `partial_time`     | `==`,`!=`                                     | equivalence operators      |
| :ballot_box_with_check: | `partial_time`     | `min`,`max`,`pmin`,`pmax`                     | partial time extremes      |
| :ballot_box_with_check: | `partial_time`     | `impute_time`                                 | imputing partial time      |
| :ballot_box_with_check: | `partial_time`     | `to_gmt`                                      | convert to gmt timezone    |
| :ballot_box_with_check: | `partial_time`     | `print`                                       | printing                   |
| :ballot_box_with_check: | `partial_time`     | `format`                                      | format as character        |
| :ballot_box_with_check: | `partial_time`     | `<vctrs>`                                     | misc `vctrs` functions     |
| :ballot_box_with_check: | `partial_time`     | `<pillar>`                                    | misc `pillar` functions    |
| :black_square_button:   | `partial_difftime` | `difftime`                                    | create `partial_difftime`  |
| :black_square_button:   | `partial_difftime` | `as.difftime`                                 | cast to `partial_difftime` |
| :black_square_button:   | `partial_difftime` | `>`,`<`,`<=`,`>=`                             | comparison operators       |
| :black_square_button:   | `partial_difftime` | `possibly`,`definitely`                       | uncertainty resolvers      |
| :black_square_button:   | `partial_difftime` | `==`,`!=`                                     | equivalence operators      |
| :black_square_button:   | `partial_difftime` | `min`,`max`,`pmin`,`pmax`                     | partial difftime extremes  |
| :black_square_button:   | `partial_difftime` | `print`                                       | printing                   |
| :black_square_button:   | `partial_difftime` | `format`                                      | format as character        |
| :black_square_button:   | `partial_difftime` | `<vctrs>`                                     | misc `vctrs` functions     |
| :black_square_button:   | `partial_difftime` | `<pillar>`                                    | misc `pillar` functions    |
| :black_square_button:   |                    | `` `-`(partial_time, partial_difftime) ``     | subraction                 |
| :black_square_button:   |                    | `` `-`(partial_time, partial_time) ``         | subraction                 |
| :black_square_button:   |                    | `` `-`(partial_difftime, partial_difftime) `` | subraction                 |
| :black_square_button:   |                    | `` `-`(partial_difftime, partial_difftime) `` | addition                   |
