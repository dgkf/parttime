# parttime

A package for a partial datetime class and generics

# Installation

```r
devtools::install_github("kelkhofd/parttime", host = "https://github.roche.com/api/v3")
```

# Example

## Parsing Incomplete Timestamps

Parse ISO8601 timestampes using the `parsedate` package, but retain information about missingness in the timestamp format. 

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
  
parttime(iso8601_dates)
```

```
 [1] "0000*-01*-01* 01*:00*:00*.000* +00*00*"
 [2] "2001 -01*-01* 01*:00*:00*.000* +00*00*"
 [3] "2002 -01 -01  01*:00*:00*.000* +00*00*"
 [4] "2004 -09 -01  01*:00*:00*.000* +00*00*"
 [5] "2005 -01*-01* 01*:00*:00*.000* +00*00*"
 [6] "2006 -01 -13  01*:00*:00*.000* +00*00*"
 [7] "2007 -10 -01  08 :00*:00*.000* +00*00*"
 [8] "2008 -09 -20  08 :35 :00*.000* +00*00*"
 [9] "2009 -08 -12  08 :35 :02 .001  +00*00*"
[10] "2010 -07 -22  08 :35 :32 .000* +00*00*"
[11] "2011 -06 -13  08 :35 :32 .000  +00*00*"
[12] "2012 -05 -23  08 :35 :32 .000  +00 00"
[13] "2013 -04 -14  08 :35 :32 .000  +05 00*"
[14] "2014 -03 -24  08 :35 :32 .000  +05 30"
[15] "2015 -01 -01  08 :35 :32 .000  +05 30"
```

## Imputing With a Timestamp

```r
impute_time("2019", "2000-01-02T03:04:05.006+0730")
```

```
[1] "2019-01-01 19:34:05 GMT"
```

## Tidyverse Compatible (in-progress)

```r
tibble(dates = iso8601_dates) %>%
  mutate(
    parttimes = parttime(dates), 
    imputed_times = as.POSIXct(impute_time(parttimes)))
```

![image](https://user-images.githubusercontent.com/18220321/64467475-b086ad00-d0cd-11e9-8e39-9a6e7e84a44e.png)
