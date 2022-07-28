# parttime 0.0.2 (dev)

* `as.parttime` will now, by default, show warnings when some strings fail to
  parse into the provided format. Behavior can be configured using the new
  `on.na` parameter.

* extends `lubridate`-style accessor and assignment helpers, such as `year()`
  and `year()<-`. Unfortuntely, many are provided as S4 generics, and may be
  masked by other packages that extend these generics. Some generic-like
  functions are provided only as non-generic forms in `lubridate` and will
  always be masked, such as `tz()` (@dgkf, #14).

* added `format` parameter to `as.parttime`, allowing for custom regular
  expressions or functions to be used to parse non-iso formats. If other
  established standards are regularly needeed, they could be included with the
  package (@dgkf, #13).

* added `res` ("resolution") parameter to the `impute_*` family of functions,
  allowing for only some field up until the provided resolution to be imputed.
  With this, also added `impute_date_*` alternatives which default to imputing
  to a `"day"` resolution (@dgkf, #12).

* rename `is_partial_*` to `has_partial_*` to avoid ambiguity with class checks

* removed some deprecated `pillar` interfaces

* bring package up-to-speed with `vctrs` package changes from last few years

* fixed subset assignment operators [<- and [[<- 

* improved imputation as to not introduce dates that aren't viable for the given
  month

# parttime 0.0.1

* initial release including loose parsing of ISO 8601 datetime formats using
parsedate

* tibble column formatting using pillar

* coersion to and from POSIX, Date and character
