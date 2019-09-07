library(dplyr)
library(tidyr)
library(purrr)

perf <- tibble(n = rep(10 ^ (1:5), each = 5))

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
      as.list(system.time(dates_posix[..1] < dates_posix[..2], 10))),
    parttime = pmap(list(sample_a, sample_b), ~
      as.list(system.time(dates_parttime[..1] < dates_parttime[..2], 10))),
    names = map(posix, names)) %>%
  select(-starts_with("sample")) %>%
  unnest(.sep = ".") %>%
  unnest() %>%
  filter(names == "elapsed") %>%
  select(-names) %>%
  group_by(n) %>%
  summarize_all(list(mean = mean, sd = sd)) %>%
  mutate(factor = parttime_mean / posix_mean)

#' Roughly 10x slower than base POSIX as of 2019-09-07
#' # A tibble: 5 x 6
#'        n posix_mean parttime_mean posix_sd parttime_sd factor
#'    <dbl>      <dbl>         <dbl>    <dbl>       <dbl>  <dbl>
#' 1     10   0.000600        0.0212 0.000894    0.0172    35.3 
#' 2    100   0.00880         0.0192 0.0163      0.000447   2.18
#' 3   1000   0.01000         0.0786 0.00274     0.0238     7.86
#' 4  10000   0.088           0.681  0.00784     0.0445     7.74
#' 5 100000   0.786           8.62   0.0226      0.372     11.0 