library(dplyr)
library(tidyr)
library(purrr)

perf <- tibble(n = 10 ^ (2:3))

today <- as.numeric(Sys.time())
dates_posix <- as.POSIXlt(
  today + rnorm(max(perf$n) * 2, today / 10, today / 10), 
  origin = "1970-01-01")
dates_parttime <- as.parttime(as.character(dates_posix))

perf %>% 
  mutate(
    sample_a = lapply(n, sample, x = 1:max(n)),
    sample_b = lapply(n, sample, x = 1:max(n))) %>%
  mutate(
    posix = pmap(list(sample_a, sample_b), ~
      time_it(dates_posix[..1] < dates_posix[..2], 10)),
    parttime = pmap(list(sample_a, sample_b), ~
      time_it(dates_parttime[..1] < dates_parttime[..2], 10))) %>%
  select(-starts_with("sample")) %>%
  unnest(.sep = ".") %>%
  select(n, ends_with("elapsed")) %>%
  rename_all(~gsub("\\.elapsed", "", .)) %>%
  group_by(n) %>%
  summarize_all(list(mean = mean, sd = sd)) %>%
  mutate(factor = parttime_mean / posix_mean) %>%
  
  capture.output() %>%
  paste0(collapse = "\n") %>%
  message("Comparison Operator Performance:\n", .)

#' Roughly 10x slower than base POSIX as of 2019-05-17
#' 
#' # A tibble: 4 x 6
#'        n posix_mean parttime_mean posix_sd parttime_sd factor
#'    <dbl>      <dbl>         <dbl>    <dbl>       <dbl>  <dbl>
#' 1    100     0.0017        0.0110 0.000483     0.00189   6.47
#' 2   1000     0.0122        0.0525 0.000422     0.00135   4.30
#' 3  10000     0.0830        0.484  0.00696      0.0369    5.83
#' 4 100000     0.763         7.71   0.00702      0.437    10.1 