# Shared functions, v01

# This file was created on 2020-08-08

suppressMessages(suppressWarnings(library(broom)))
suppressMessages(suppressWarnings(library(magrittr)))
suppressMessages(suppressWarnings(library(readxl)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(tidyverse)))



bbpad <- function(x) {
  one_digit <- (x < 10)
  two_digits <- (x >=10) & (x < 100)
  x <- as.character(x)
  x[one_digit] <- paste0("  ", x[one_digit])
  x[two_digits] <- paste0(" ", x[two_digits])
  return(x)
}



calculate_mean_sd <- function(dat, v, round_digits=1) {
  dat %>%
    summarize(
      mn=round(mean({{v}}), round_digits), 
      std=round(sd({{v}}), round_digits)) %>%
    mutate(mean_sd=paste0(mn, "(", std, ")")) %>%
    select(-mn, -std) %>%
    data.frame
}
# Test data
data.frame(g=rep(1:2, each=10), v=1:20) %>%
  group_by(g) %>%
  calculate_mean_sd(v)



calculate_median_iqr <- function(dat, v, round_digits=2) {
  v_name <- deparse(substitute(v))
  dat %>%
    summarize(
      median=round(median({{v}}), round_digits), 
      q25=round(quantile({{v}}, probs=0.25), round_digits), 
      q75=round(quantile({{v}}, probs=0.75), round_digits)) %>%
    mutate(median_iqr=paste0(median, " (", q25, ", ", q75, ")")) %>%
    select(-median, -q25, -q75) %>%
    data.frame -> dat
  dat
}
# Test data
data.frame(g=rep(1:2, each=10), x=1:20) %>%
  group_by(g) %>%
  calculate_median_iqr(x)



calculate_percentages <- function(dat, v, v_level=1) {
  dat %>%
    summarize(
      numerator=sum({{v}}==v_level),
      denominator=n()) %>%
    ungroup %>%
    mutate(pct=round(100*numerator/denominator, 0)) %>%
    mutate(pct=paste0(
      bbpad(round(100*numerator/denominator, 0)), "% (",
      bbpad(numerator),"/", bbpad(denominator), ")")) %>%
    select(-numerator, -denominator) %>%
    data.frame
}
# Test data
data.frame(g=rep(1:2, each=20), x=rep(c(0:1, 0:1), c(19, 1, 8, 12))) %>%
  group_by(g) %>%
  calculate_percentages(x)



compare_means <- function(dat, v1, v2, verbose=FALSE) {
  d <- data.frame(y=dat[[v1]], x=dat[[v2]])
  lm(y~x, data=d) %>% 
    tidy %>%
    filter(term=="x") %>%
    select(p.value) %>%
    unlist %>%
    round_p
}



compare_proportions <- function(dat, v1, v2, verbose=FALSE) {
  dat %>%
    pull({{v1}}) %>%
    factor -> f1
  dat %>%
    pull({{v2}}) %>%
    factor -> f2
  table(f1, f2) -> x
  if (verbose) print(x)
  if (min(dim(x)) < 2) return("Could not compute p-value")
  fisher.test(x) %>%
    extract2("p.value") %>%
    round_p
}



round_p <- function(p1) {
  ifelse(p1 < 0.001, "p<0.001", paste0("p=", round(p1, 3)))
}
