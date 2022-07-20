iso8601_dates <- c(
  NA,
  "2001",
  "2002-03-04",
  "2003-095", # yearday
  "2004-W13",  # yearweek (will produce warnings unless as.parttime warn = FALSE)
  "2005-W23-2",  # yearweek and weekday
  "2006-07-08T09",
  "2007-08-09T10:11",
  "2008-09-10T11:12.2169",  # fractional minute
  "2009-10-11T12:13:14",
  "2010-11-12T13:14:15.016",  # fractional second
  "2011-12-13T14:15:16.017Z",  # Zulu time
  "2012-01-14T15:16:17.018+07",  # time offset from GMT
  "2013-02-15T16:17:18.019+07:30",  # time offset with min from GMT
  "20140316T17:18:19.020+08:00"  # condensed form
)
