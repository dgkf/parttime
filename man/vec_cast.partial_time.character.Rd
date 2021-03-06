% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_partial_time_coersion.R
\name{vec_cast.partial_time.character}
\alias{vec_cast.partial_time.character}
\title{Coerce character date representations to parttime objects}
\usage{
\method{vec_cast.partial_time}{character}(x, to, ...)
}
\arguments{
\item{x}{Vectors to cast.}

\item{to}{Type to cast to. If \code{NULL}, \code{x} will be returned as is.}

\item{...}{For \code{vec_cast_common()}, vectors to cast. For
\code{vec_cast()}, \code{vec_cast_default()}, and \code{vec_restore()}, these
dots are only for future extensions and should be empty.}
}
\description{
Coerce character date representations to parttime objects
}
\examples{
dates <- c(
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

as.parttime(dates)

\dontrun{
### vctrs experiments informing design of parttime ###

# using a rcrd (record) style vector
rcrd_test <- vctrs::new_rcrd(
  fields = list(a = 1:3, b = 4:6),
  class = numeric())
 
tibble(x = 1:3, y = rcrd_test)
# okay

tibble(x = 1:3) \%>\% mutate(y = rcrd_test)
# Error: Column `y` must be length 3 (the number of rows) or one, not 2
 
tibble(x = 1:3) \%>\% { .$y <- rcrd_test; . }  
# okay (stand-in for mutate until dplyr v0.9.0)

}

}
