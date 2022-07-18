
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
    ## <partial_time<Y[38;5;246mM[39m[31mD[39m[31mh[39m[31mm[39m[31ms[39mZ>[2]> 
    ## [1] "2022"    "2022[38;5;246m-[39m[38;5;246m0[39m2"

We can access the components of each datetime as though the
`partial_time` is a matrix of datetime fields, or using
`lubridate`-style accessors and assignment functions.

``` r
pttms[, "year"]
##    2022 2022-02 
##    2022    2022

pttms[[1, "year"]]
## [1] 2022

year(pttms)
##    2022 2022-02 
##    2022    2022

year(pttms[1])
## [1] 2022

month(pttms[2]) <- 3
pttms
## <partial_time<Y[38;5;246mM[39m[31mD[39m[31mh[39m[31mm[39m[31ms[39mZ>[2]> 
## [1] "2022"    "2022[38;5;246m-[39m[38;5;246m0[39m3"

month(pttms[1]) <- 3
pttms
## <partial_time<YM[31mD[39m[31mh[39m[31mm[39m[31ms[39mZ>[2]> 
## [1] "2022[38;5;246m-[39m[38;5;246m0[39m3" "2022[38;5;246m-[39m[38;5;246m0[39m3"

month(pttms) <- NA
pttms
## <partial_time<Y[31mM[39m[31mD[39m[31mh[39m[31mm[39m[31ms[39mZ>[2]> 
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
    ## <partial_time<Y[38;5;246mM[39m[31mD[39m[31mh[39m[31mm[39m[31ms[39mZ>[2]> 
    ## [1] "2022"    "2022[38;5;246m-[39m[38;5;246m0[39m2"

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
## Warning in strptime(apply(match_m[i, c("year", "week", "weekday"), drop = FALSE], : (0-based) yday 367 in year 9999 is invalid
## <partial_time<YMD[31mh[39m[31mm[39m[31ms[39m[31m+tz[39m>[1]> 
## [1] "2022[38;5;246m-[39m[38;5;246m0[39m2[38;5;246m-[39m28"

impute_time(pttms[1], "1999-06-05T04:03:02")  # arbitrary imputation
## <partial_time<YMDhmsZ>[1]> 
## [1] "2022[38;5;246m-[39m[38;5;246m0[39m6[38;5;246m-[39m[38;5;246m0[39m5 [38;5;246m0[39m4[38;5;246m:[39m03[38;5;246m:[39m02[38;5;246m.[39m000"
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

    ## <partial_time<[38;5;246mY[39m[38;5;246mM[39m[38;5;246mD[39m[38;5;246mh[39m[38;5;246mm[39m[38;5;246ms[39m[38;5;246m+tz[39m>[15]> 
    ##  [1] [31mNA[39m                             "2001"                         "2002[38;5;246m-[39m[38;5;246m0[39m1[38;5;246m-[39m[38;5;246m0[39m1"                  
    ##  [4] "2004[38;5;246m-[39m[38;5;246m0[39m9[38;5;246m-[39m[38;5;246m0[39m1"                   "2005"                         "2006[38;5;246m-[39m[38;5;246m0[39m1[38;5;246m-[39m13"                  
    ##  [7] "2007[38;5;246m-[39m10[38;5;246m-[39m[38;5;246m0[39m1 [38;5;246m0[39m8"                "2008[38;5;246m-[39m[38;5;246m0[39m9[38;5;246m-[39m20 [38;5;246m0[39m8[38;5;246m:[39m35"             "2009[38;5;246m-[39m[38;5;246m0[39m8[38;5;246m-[39m12 [38;5;246m0[39m8[38;5;246m:[39m35[38;5;246m:[39m02[38;5;246m.[39m880"     
    ## [10] "2010[38;5;246m-[39m[38;5;246m0[39m7[38;5;246m-[39m22 [38;5;246m0[39m8[38;5;246m:[39m35[38;5;246m:[39m32[38;5;246m.[39m000"      "2011[38;5;246m-[39m[38;5;246m0[39m6[38;5;246m-[39m13 [38;5;246m0[39m8[38;5;246m:[39m35[38;5;246m:[39m32[38;5;246m.[39m123"      "2012[38;5;246m-[39m[38;5;246m0[39m5[38;5;246m-[39m23 [38;5;246m0[39m8[38;5;246m:[39m35[38;5;246m:[39m32[38;5;246m.[39m123"     
    ## [13] "2013[38;5;246m-[39m[38;5;246m0[39m4[38;5;246m-[39m14 [38;5;246m0[39m8[38;5;246m:[39m35[38;5;246m:[39m32[38;5;246m.[39m123+0500" "2014[38;5;246m-[39m[38;5;246m0[39m3[38;5;246m-[39m24 [38;5;246m0[39m8[38;5;246m:[39m35[38;5;246m:[39m32[38;5;246m.[39m123+0530" "2015[38;5;246m-[39m[38;5;246m0[39m1[38;5;246m-[39m[38;5;246m0[39m1 [38;5;246m0[39m8[38;5;246m:[39m35[38;5;246m:[39m32[38;5;246m.[39m123+0530"

## Imputing Timestamps

``` r
>  impute_time("2019", "2000-01-02T03:04:05.006+0730")
## <partial_time<YMDhms+tz>[1]>
## [1] "2019-01-02 03:04:05.006"
```

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

## TRUE # but we know they’re equal within the same year

options(parttime.assume_tz_offset = NA) definitely(parttime(2019) ==
parttime(2019), by = “year”) ## NA # with an unknown timezone, these may
not even be within the same year


    ## Timespans

    Cast a partial time's missingness to a range of possible values

    ```r
    as.timespan(parttime(2019))
    ## <timespan[1]>
    ## [1] [2019 — 2020) 

## Tidyverse Compatible `vctrs`

``` r
tibble(dates = iso8601_dates) %>%
  mutate(
    parttimes = as.parttime(dates), 
    imputed_times = as.POSIXct(impute_time(parttimes)))
```

<img src="https://user-images.githubusercontent.com/18220321/64467475-b086ad00-d0cd-11e9-8e39-9a6e7e84a44e.png" width="75%"></img>

# Roadmap

| class              | function/op                                   | description                | status |
|--------------------|-----------------------------------------------|----------------------------|--------|
| `partial_time`     | `parttime`                                    | create `partial_time`      | ✔︎\*︎  |
| `partial_time`     | `as.parttime`                                 | cast to `partial_time`     | ✔︎\*︎  |
| `partial_time`     | `>`,`<`,`<=`,`>=`                             | comparison operators       | ✔︎\*︎  |
| `partial_time`     | `possibly`,`definitely`                       | uncertainty resolvers      | ✔︎\*︎  |
| `partial_time`     | `==`,`!=`                                     | equivalence operators      | ✔︎\*︎  |
| `partial_time`     | `min`,`max`,`pmin`,`pmax`                     | partial time extremes      | ✔︎\*︎  |
| `partial_time`     | `impute_time`                                 | imputing partial time      | ✔︎\*︎  |
| `partial_time`     | `to_gmt`                                      | convert to gmt timezone    | ✔︎\*︎  |
| `partial_time`     | `print`                                       | printing                   | ✔︎\*︎  |
| `partial_time`     | `format`                                      | format as character        | ✔︎\*︎  |
| `partial_time`     | `<vctrs>`                                     | misc `vctrs` functions     | ✔︎\*︎  |
| `partial_time`     | `<pillar>`                                    | misc `pillar` functions    | ✔︎\*︎  |
| `partial_difftime` | `difftime`                                    | create `partial_difftime`  | ︎      |
| `partial_difftime` | `as.difftime`                                 | cast to `partial_difftime` | ︎      |
| `partial_difftime` | `>`,`<`,`<=`,`>=`                             | comparison operators       | ︎      |
| `partial_difftime` | `possibly`,`definitely`                       | uncertainty resolvers      | ︎      |
| `partial_difftime` | `==`,`!=`                                     | equivalence operators      | ︎      |
| `partial_difftime` | `min`,`max`,`pmin`,`pmax`                     | partial difftime extremes  | ︎      |
| `partial_difftime` | `print`                                       | printing                   |        |
| `partial_difftime` | `format`                                      | format as character        |        |
| `partial_difftime` | `<vctrs>`                                     | misc `vctrs` functions     |        |
| `partial_difftime` | `<pillar>`                                    | misc `pillar` functions    |        |
|                    | `` `-`(partial_time, partial_difftime) ``     | subraction                 | ︎      |
|                    | `` `-`(partial_time, partial_time) ``         | subraction                 | ︎      |
|                    | `` `-`(partial_difftime, partial_difftime) `` | subraction                 | ︎      |
|                    | `` `-`(partial_difftime, partial_difftime) `` | addition                   | ︎      |

\*[Debating internal representation of
class](https://github.com/dgkf/parttime/issues/5), which would require
tweaking existing implementations
