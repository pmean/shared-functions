# Shared functions, v01

# This file was created on 2020-08-08

suppressMessages(suppressWarnings(library(broom)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(magrittr)))
suppressMessages(suppressWarnings(library(readxl)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(tidyverse)))
# general functions

# These functions join strings in various ways.

"%0%" <- function(x, y) {paste0(x        , y)}
"%1%" <- function(x, y) {paste0(x, "\n"  , y)}
"%2%" <- function(x, y) {paste0(x, "\n\n", y)}
"%b%" <- function(x, y) {paste0(x, ' '   , y)}
"%c%" <- function(x, y) {paste0(x, ','   , y)}
"%C%" <- function(x, y) {paste0(x, ', '  , y)}
"%d%" <- function(x, y) {paste0(x, "."   , y)}
"%D%" <- function(x, y) {paste0(x, ". "  , y)}
"%s%" <- function(x, y) {paste0(x, "/"   , y)}
"%.%" <- function(x, y) {paste0(x, ". "  , y)}
"% %" <- function(x, y) {paste0(x, " "   , y)}

"%p%" <- function(x, y) {paste0(x,  '(', y, ')')}
"%P%" <- function(x, y) {paste0(x, ' (', y, ')')}
"%q%" <- function(x, y) {paste0(x,  '"', y, '"')}
"%Q%" <- function(x, y) {paste0(x, ' "', y, '"')}
"%[%" <- function(x, y) {paste0(x,  "[", y, "]")}

brack <- function(x) {"" %[% x}
paren <- function(x) {"" %p% x}
quote <- function(x) {"" %q% x}

br <- function(x, n=1) {x %0% str_c(rep("\n", n), collapse="")}

# Test these functions
if (verbose) {
  cat("\n\n***Testing various string infix operators***\n")
  x <- "abc"
  y <- "def"
  br("") %>% cat
  '"abc" %0% "def" produces: ' %1% x %0% y      %>% br    %>% cat
  '"abc" %1% "def" produces: ' %1% x %1% y      %>% br    %>% cat
  '"abc" %2% "def" produces: ' %1% x %2% y      %>% br(2) %>% cat
  
  '"abc" %b% "def" produces: ' %1% x %b% y      %>% br    %>% cat
  '"abc" %c% "def" produces: ' %1% x %c% y      %>% br    %>% cat
  '"abc" %C% "def" produces: ' %1% x %C% y      %>% br    %>% cat
  '"abc" %d% "def" produces: ' %1% x %d% y      %>% br    %>% cat
  '"abc" %D% "def" produces: ' %1% x %D% y      %>% br    %>% cat
  '"abc" %s% "def" produces: ' %1% x %s% y      %>% br    %>% cat
  '"abc" %.% "def" produces: ' %1% x %.% y      %>% br(2) %>% cat
  
  '"abc" %p% "def" produces: ' %1% x %p% y      %>% br    %>% cat
  '"abc" %P% "def" produces: ' %1% x %P% y      %>% br    %>% cat
  '"abc" %q% "def" produces: ' %1% x %q% y      %>% br    %>% cat
  '"abc" %Q% "def" produces: ' %1% x %Q% y      %>% br    %>% cat
  '"abc" %[% "def" produces: ' %1% x %[% y      %>% br(2) %>% cat
  
  'br("abc", 1) produces: '    %1% br("abc", 1) %>% br    %>% cat
  'br("abc", 2) produces: '    %1% br("abc", 2) %>% br(2) %>% cat
  
  'brack("abc") produces: '    %1% brack(x)     %>% br    %>% cat
  'paren("abc") produces: '    %1% paren(x)     %>% br    %>% cat
}


bpad <- function(x) {
  one_digit <- (x < 10)
  x <- as.character(x)
  x[one_digit] <- paste0(" ", x[one_digit])
  return(x)
}

bbpad <- function(x) {
  one_digit <- (x < 10)
  two_digits <- (x >=10) & (x < 100)
  x <- as.character(x)
  x[one_digit] <- paste0("  ", x[one_digit])
  x[two_digits] <- paste0(" ", x[two_digits])
  return(x)
}

bbbpad <- function(x) {
  one_digit <- (x < 10)
  two_digits <- (x >=10) & (x < 100)
  three_digits <- (x >=100) & (x < 1000)
  x <- as.character(x)
  x[one_digit] <- paste0("   ", x[one_digit])
  x[two_digits] <- paste0("  ", x[two_digits])
  x[three_digits] <- paste0(" ", x[two_digits])
  return(x)
}

calculate_logistic_confidence_intervals <- function(dat) {
  dat %>%
    tidy %>%
    filter(term != "(Intercept)") %>%
    mutate(odds.ratio=round(exp(estimate), 1)) %>%
    mutate(lower.ci=round(exp(estimate-1.96*std.error), 1)) %>%
    mutate(upper.ci=round(exp(estimate+1.96*std.error), 1)) %>%
    mutate(p.value=round(p.value, 4)) %>%
    mutate(p.value=pmax(p.value, 0.0001)) %>%
    select(term, odds.ratio, lower.ci, upper.ci, p.value) %>%
    data.frame
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

# This function adds a leading zeros to digits less than 10

zpad <- function(x) {
  message_tail <- " in zzzpad may produce nonsensical results"
  if(any(x<0))           message("Note: Negative values", message_tail)
  if(any(x != trunc(x))) message("Note: Fractional values", message_tail)
  if(any(x>99))        message("Note: Values > 9999", message_tail)
  case_when(
    x <   10 ~ paste0("0", x),
    TRUE     ~ as.character(x)
  ) %>% return
}

# Test this function
if (verbose) {
  cat("\n\n***Testing zzzpad***\n")
  zpad(c(8:12)) %>% print
}
# If you have the time and energy, check out
# these cases as well.
# if (verbose) zpad(-3)
# if (verbose) zpad(0.5)
# if (verbose) zpad(98:103)

zzzpad <- function(x) {
  message_tail <- " in zzzpad may produce nonsensical results"
  if(any(x<0))           message("Note: Negative values", message_tail)
  if(any(x != trunc(x))) message("Note: Fractional values", message_tail)
  if(any(x>9999))        message("Note: Values > 9999", message_tail)
  case_when(
    x <   10 ~ paste0("000", x),
    x <  100 ~ paste0( "00", x),
    x < 1000 ~ paste0(  "0", x),
    TRUE     ~ as.character(x)
  ) %>% return
}

# Test this function
if (verbose) {
  cat("\n\n***Testing zzzpad***\n")
  zzzpad(c(8:12, 98:102, 998:1002)) %>% print
}
# If you have the time and energy, check out
# these cases as well.
# if (verbose) zzzpad(-3)
# if (verbose) zzzpad(0.5)
# if (verbose) zzzpad(9998:10003)

