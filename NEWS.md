# parttime 0.0.2 (dev)

* rename `is_partial_*` to `has_partial_*` to avoid ambiguity with class checks

* removed some deprecated `pillar` interfaces

* bring package up-to-speed with `vctrs` package changes from last few years

# parttime 0.0.1.2

* fixed subset assignment operators [<- and [[<- 

# parttime 0.0.1.1

* improved imputation as to not introduce dates that aren't viable for the given
month

# parttime 0.0.1

* initial release including loose parsing of ISO 8601 datetime formats using
parsedate

* tibble column formatting using pillar

* coersion to and from POSIX, Date and character
