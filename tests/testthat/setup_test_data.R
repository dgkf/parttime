iso8601_dates <- c(
  NA,
  "2001",
  "2002-03-04",
  "2003-095",  # yearday
  "2004-W13",  # yearweek (will produce warnings unless as.parttime warn = FALSE)
  "2005-W23-3",  # yearweek and weekday
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

# From the SDTMIG version 3.4
# (https://www.cdisc.org/system/files/members/standard/foundational/SDTMIG_v3.4.pdf)

# From the SDTMIG version 3.4, Page 39: decreasing precision, fully specified
cdisc_datetimes_decreasing_sensitivity <- c(
  "2003-12-15T13:14:17.123",
  "2003-12-15T13:14:17",
  "2003-12-15T13:14",
  "2003-12-15T13",
  "2003-12-15",
  "2003-12",
  "2003"
)

# From the SDTMIG version 3.4, Page 40: intervals of uncertainty
cdisc_intervals <- c(
  "2003-12-15T10:00/2003-12-15T10:30",  # Between 10:00 and 10:30 on the morning of December 15, 2003
  "2003-01-01/2003-02-15",  # Between the first of this year (2003) until "now" (February 15, 2003)
  "2003-12-01/2003-12-10",  # Between the first and the tenth of December, 2003
  "2003-01-01/2003-06-30"  # Sometime in the first half of 2003
)

# From the SDTMIG version 3.4, Page 40: missing middle parts
cdisc_missing_in_middle <- c(
  "2003-12-15T13:15:17",  # December 15, 2003 13:15:17; Date/time to the nearest second
  "2003-12-15T-:15",  # December 15, 2003 ??:15 Unknown hour with known minutes
  "2003-12-15T13:-:17",  # December 15, 2003 13:??:17 Unknown minutes with known date, hours, and seconds
  "2003---15",  # The 15th of some month in 2003, time not collected Unknown month and time with known year and day
  "--12-15",  # December 15, but can't remember the year, time not collected; Unknown year with known month and day
  "-----T07:15",  # 7:15 of some unknown date Unknown date with known hour and minute

  # All single variants of missingness (in addition to those in the SDTMIG
  "--12-15T13:15:17",  # missing year
  "2003---15T13:15:17",  # missing month
  "2003-12--T13:15:17",  # missing day
  "2003-12-15T-:15:17",  # missing hour
  "2003-12-15T13:-:17",  # missing minute
  "2003-12-15T13:15",  # missing second (is just truncation)

  # Testing pairs of missingness with year and one other part
  "----15T13:15:17",  # missing year, month
  "--12--T13:15:17",  # missing year, day
  "--12-15T-:15:17",  # missing year, hour
  "--12-15T13:-:17",  # missing year, minute
  "--12-15T13:15" # missing year, second (second is truncation not a dash)
)

# A general list of invalid datetimes
invalid_datetime_general <- c(
  "20221",  # 5 digit years are invalid
  "2022-1",  # 1 digit months are invalid
  "2022-001",  # 3 digit months are invalid
  "2022:01",  # date separator must be a dash

  "2022-01-1",  # 1 digit days are invalid
  "2022-01-001",  # 3 digit days are invalid
  "2022-01-01t01",  # T must be upper case
  "2022-01-01T1",  # 1 digit hours are invalid
  "2022-01-01T001",  # 3 digit hours are invalid
  "2022-01-01T001",  # 3 digit hours are invalid
  "2022-01-01T01-01",  # time separator must be colon
  "2022-01-01T01:1",  # 1 digit minutes are invalid
  "2022-01-01T01:001",  # 3 digit minutes are invalid
  "2022-01-01T01:01:1",  # 1 digit seconds are invalid
  "2022-01-01T01:01:001",  # 3 digit seconds are invalid

  "foo",  # general text that is not date-like
  ""  # an empty string is not a valid dose
)

# Date-times that are invalid with CDISC but are valid with ISO 8601
invalid_datetime_cdisc <- c(
  # From the middle of page 39 in the SDTMIG version 3.4:
  # Extended format ISO 8601 (with dashes and colons) is required
  "202201",  # year and month in non-extended format
  "20220101T010101",  # year, month, day, hour, minute, second in non-extended format
  # From the top of page 39 in the SDTMIG version 3.4:
  # comma separator for fractions is not allowed by CDISC
  "2022-01-01T01:01:01,0",
  # Missing second should not be coded in this way; it should be truncated
  "2003-12-15T13:15:-"
)
