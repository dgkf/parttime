# parttime

[![status](https://img.shields.io/static/v1?label=status&message=experimental&color=red)]()
[![travisci](https://api.travis-ci.org/dgkf/parttime.svg?branch=master)](https://travis-ci.org/dgkf/parttime)
[![codecov](https://codecov.io/gh/dgkf/parttime/branch/master/graph/badge.svg)](https://codecov.io/gh/dgkf/parttime)

A package for a partial datetime class and generics

# Installation

```r
devtools::install_github("dgkf/parttime")
```

# Example

## Parsing Incomplete Timestamps

Parse ISO8601 timestampes using the `parsedate` package's pareser, but retain
information about missingness in the timestamp format.

```r
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
  "20150101T08:35:32.123+05:30")  # condensed form
  
as.parttime(iso8601_dates)

#> <partial_time<YMDhms+tz>[15]> 
#>  [1] NA                             "2001+0000"                   
#>  [3] "2002-01-01+0000"              "2004-09-01+0000"             
#>  [5] "2005+0000"                    "2006-01-13+0000"             
#>  [7] "2007-10-01 08+0000"           "2008-09-20 08:35+0000"       
#>  [9] "2009-08-12 08:35:02.880+0000" "2010-07-22 08:35:32+0000"    
#> [11] "2011-06-13 08:35:32.123+0000" "2012-05-23 08:35:32.123+0000"
#> [13] "2013-04-14 08:35:32.123+0500" "2014-03-24 08:35:32.123+0530"
#> [15] "2015-01-01 08:35:32.123+0530" 
```

## Imputing Timestamps

```r
>  impute_time("2019", "2000-01-02T03:04:05.006+0730")

#> <partial_time<YMDhms+tz>[1]> 
#> [1] "2019-01-02 03:04:05.006" 
```

## Datetime Comparisons

Partial timestamps include uncertainty, which means that there is often
uncertainty when comparing between timestamps. To help resolve this uncertainty
there are two helper functions, `possibly` and `definitely` resolving this uncertainty for when the windows of uncertainty overlap, or equal (to a given resolution).

```r
options(parttime.assume_tz_offset = 0)  # assume GMT
parttime(2019) < parttime(2020)
#> TRUE

options(parttime.assume_tz_offset = NA)  # don't assume a timezone
parttime(2019) < parttime(2020)
#> NA  # due to potentially different timezones, this can't be certain

possibly(parttime(2019) < parttime(2020))
#> TRUE 

definitely(parttime(2019) < parttime(2020))
#> FALSE

parttime(2019) == parttime(2019)
#> NA  # given uncertainty in timestamps, we can't be sure these are equal

options(parttime.assume_tz_offset = 0)
definitely(parttime(2019) == parttime(2019), by = "year")
#> TRUE  # but we know they're equal within the same year

options(parttime.assume_tz_offset = NA)
definitely(parttime(2019) == parttime(2019), by = "year")
#> NA  # with an unknown timezone, these may not even be within the same year
```

## Timespans

Cast a partial time's missingness to a range of possible values

```r
as.timespan(parttime(2019))
#> <timespan[1]>
#> [1] [2019 — 2020) 
```

## Tidyverse Compatible (coming with `vctrs` support in `dplyr` 0.9.0)

```r
# Not yet working because `dplyr` doesn't yet support `vctrs_rcrd` classes
tibble(dates = iso8601_dates) %>%
  mutate(
    parttimes = as.parttime(dates), 
    imputed_times = as.POSIXct(impute_time(parttimes)))
```

<img src="https://user-images.githubusercontent.com/18220321/64467475-b086ad00-d0cd-11e9-8e39-9a6e7e84a44e.png" width="75%"></img>

# Roadmap

|class|function/op|description|status|
|---|---|---|---|
|`partial_time`|`parttime`|create `partial_time`|✔︎*︎|
|`partial_time`|`as.parttime`|cast to `partial_time`|✔︎*︎|
|`partial_time`|`>`,`<`,`<=`,`>=`|comparison operators|✔︎*︎|
|`partial_time`|`possibly`,`definitely`|uncertainty resolvers|✔︎*︎|
|`partial_time`|`==`,`!=`|equivalence operators|✔︎*︎|
|`partial_time`|`min`,`max`,`pmin`,`pmax`|partial time extremes|✔︎*︎|
|`partial_time`|`impute_time`|imputing partial time|✔︎*︎|
|`partial_time`|`to_gmt`|convert to gmt timezone|✔︎*︎|
|`partial_time`|`print`|printing|✔︎*︎|
|`partial_time`|`format`|format as character|✔︎*︎|
|`partial_time`|`<vctrs>`|misc `vctrs` functions|✔︎*︎|
|`partial_time`|`<pillar>`|misc `pillar` functions|✔︎*︎|
|`partial_difftime`|`difftime`|create `partial_difftime`|︎|
|`partial_difftime`|`as.difftime`|cast to `partial_difftime`|︎|
|`partial_difftime`|`>`,`<`,`<=`,`>=`|comparison operators|︎|
|`partial_difftime`|`possibly`,`definitely`|uncertainty resolvers|︎|
|`partial_difftime`|`==`,`!=`|equivalence operators|︎|
|`partial_difftime`|`min`,`max`,`pmin`,`pmax`|partial difftime extremes|︎|
|`partial_difftime`|`impute_time`|imputing partial time|︎|
|`partial_difftime`|`to_gmt`|convert to gmt timezone|︎|
|`partial_difftime`|`print`|printing||
|`partial_difftime`|`format`|format as character||
|`partial_difftime`|`<vctrs>`|misc `vctrs` functions||
|`partial_difftime`|`<pillar>`|misc `pillar` functions||
||``` `-`(partial_time, partial_difftime) ```|subraction|︎|
||``` `-`(partial_time, partial_time) ```|subraction|︎|
||``` `-`(partial_difftime, partial_difftime) ```|subraction|︎|
||``` `-`(partial_difftime, partial_difftime) ```|addition|︎|

*[Debating internal representation of class](#1), which would require tweaking
existing implementations
