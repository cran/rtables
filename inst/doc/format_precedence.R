## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(comment = "#")

## ---- message=FALSE-----------------------------------------------------------
library(rtables)
ADSL <- ex_adsl

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  analyze(vars = "AGE", afun = mean)

adsl_analyzed <- build_table(lyt, ADSL)
adsl_analyzed
table_structure(adsl_analyzed)

## -----------------------------------------------------------------------------
lyt0 <- basic_table() %>%
  split_cols_by("ARM") %>%
  analyze(vars = "AGE", afun = mean)

build_table(lyt0, ADSL)

## -----------------------------------------------------------------------------
lyt1 <- basic_table() %>%
  split_cols_by("ARM") %>%
  analyze(vars = "AGE", afun = function(x) {
    rcell(mean(x), format = "xx.xx", label = "Mean")
  })

build_table(lyt1, ADSL)

lyt1a <- basic_table() %>%
  split_cols_by("ARM") %>%
  analyze(vars = "AGE", afun = function(x) {
    in_rows(
      "Mean" = rcell(mean(x)),
      .formats = "xx.xx"
    )
  })

build_table(lyt1a, ADSL)

## -----------------------------------------------------------------------------
lyt2 <- basic_table() %>%
  split_cols_by("ARM") %>%
  analyze(vars = "AGE", afun = function(x) {
    in_rows(
      "Mean" = rcell(mean(x), format = "xx.xxx"),
      .formats = "xx.xx"
    )
  })

build_table(lyt2, ADSL)

## -----------------------------------------------------------------------------
lyt3 <- basic_table() %>%
  split_cols_by("ARM") %>%
  analyze(vars = "AGE", mean, format = "xx.x")

build_table(lyt3, ADSL)

## -----------------------------------------------------------------------------
lyt4 <- basic_table() %>%
  split_cols_by("ARM") %>%
  analyze(
    vars = "AGE", afun = function(x) {
      rcell(mean(x), format = "xx.xx", label = "Mean")
    },
    format = "xx.x"
  )

build_table(lyt4, ADSL)

lyt4a <- basic_table() %>%
  split_cols_by("ARM") %>%
  analyze(
    vars = "AGE", afun = function(x) {
      in_rows(
        "Mean" = rcell(mean(x)),
        "SD" = rcell(sd(x)),
        .formats = "xx.xx"
      )
    },
    format = "xx.x"
  )

build_table(lyt4a, ADSL)

## -----------------------------------------------------------------------------
lyt5 <- basic_table() %>%
  split_cols_by("ARM") %>%
  analyze(
    vars = "AGE", afun = function(x) {
      in_rows(
        "Mean" = rcell(mean(x), format = "xx.xx"),
        "SD" = rcell(sd(x))
      )
    },
    format = "xx.x"
  )

build_table(lyt5, ADSL)

## -----------------------------------------------------------------------------
lyt6 <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  analyze(vars = "AGE", afun = mean, format = "xx.xx")

build_table(lyt6, ADSL)

## -----------------------------------------------------------------------------
lyt7 <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  analyze(vars = "AGE", afun = function(x) {
    rcell(mean(x), format = "xx.xx", label = "Mean", format_na_str = "<missing>")
  })

build_table(lyt7, ADSL)

lyt7a <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  analyze(vars = "AGE", afun = function(x) {
    in_rows(
      "Mean" = rcell(mean(x), format = "xx.xx"),
      .format_na_strs = "<MISSING>"
    )
  })

build_table(lyt7a, ADSL)

## -----------------------------------------------------------------------------
lyt8 <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  analyze(vars = "AGE", afun = function(x) {
    in_rows(
      "Mean" = rcell(mean(x), format = "xx.xx", format_na_str = "<missing>"),
      .format_na_strs = "<MISSING>"
    )
  })

build_table(lyt8, ADSL)

## -----------------------------------------------------------------------------
lyt9 <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  analyze(vars = "AGE", mean, format = "xx.xx", na_str = "not available")

build_table(lyt9, ADSL)

## -----------------------------------------------------------------------------
lyt10 <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  analyze(
    vars = "AGE", afun = function(x) {
      rcell(mean(x), format = "xx.xx", label = "Mean", format_na_str = "<missing>")
    },
    na_str = "not available"
  )

build_table(lyt10, ADSL)

lyt10a <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  analyze(
    vars = "AGE", afun = function(x) {
      in_rows(
        "Mean" = rcell(mean(x)),
        "SD" = rcell(sd(x)),
        .formats = "xx.xx",
        .format_na_strs = "<missing>"
      )
    },
    na_str = "not available"
  )

build_table(lyt10a, ADSL)

## -----------------------------------------------------------------------------
lyt11 <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  analyze(
    vars = "AGE", afun = function(x) {
      in_rows(
        "Mean" = rcell(mean(x), format_na_str = "<missing>"),
        "SD" = rcell(sd(x))
      )
    },
    format = "xx.xx",
    na_str = "not available"
  )

build_table(lyt11, ADSL)

