## -----------------------------------------------------------------------------
keep_2_levels <- function(varnm, dat = ex_adsl) {
  keep_split_levels(levels(dat[[varnm]])[1:2])
}

## -----------------------------------------------------------------------------
library(rtables)
lyt <- basic_table() |>
  split_cols_by("ARM") |>
  split_cols_by("STRATA1") |>
  split_rows_by("SEX", split_fun = keep_2_levels("SEX")) |>
  split_rows_by("BMRKR2", split_fun = keep_2_levels("BMRKR2")) |>
  analyze("AGE")
table_structure(build_table(lyt, ex_adsl))

## -----------------------------------------------------------------------------
lyt2 <- basic_table() |>
  split_cols_by("ARM") |>
  split_cols_by("STRATA1") |>
  split_rows_by("SEX", split_fun = keep_2_levels("SEX")) |>
  split_rows_by("BMRKR2", split_fun = keep_2_levels("BMRKR2")) |>
  analyze("AGE") |>
  analyze("BMRKR1")
table_structure(build_table(lyt2, ex_adsl))

## -----------------------------------------------------------------------------
trim_adsl <- subset(ex_adsl, RACE %in% levels(ex_adsl$RACE)[1:3] & SEX %in% c("F", "M"))
trim_adsl$RACE <- factor(trim_adsl$RACE)
trim_adsl$SEX <- factor(trim_adsl$SEX)

nice_mean <- function(x) {
  in_rows("Average Age" = mean(x), .formats = list("Average Age" = "xx.x"))
}

lyt3 <- basic_table(top_level_section_div = "-") |>
  split_cols_by("ARM") |>
  analyze("AGE", afun = nice_mean) |>
  split_rows_by("SEX", nested = FALSE) |>
  analyze("AGE", afun = nice_mean) |>
  split_rows_by("RACE", nested = FALSE) |>
  analyze("AGE", afun = nice_mean)

tbl3 <- build_table(lyt3, trim_adsl)
tbl3

## -----------------------------------------------------------------------------
nice_mean_cfun <- function(x, labelstr) {
  lbl <- paste0(labelstr, " (Ave. Age)")
  in_rows(mean(x), .labels = lbl, .formats = "xx.x")
}

lyt3b <- basic_table(top_level_section_div = "-") |>
  split_cols_by("ARM") |>
  summarize_row_groups("AGE", cfun = nice_mean_cfun) |>
  split_rows_by("SEX", split_fun = keep_2_levels("SEX")) |>
  summarize_row_groups("AGE", cfun = nice_mean_cfun) |>
  split_rows_by("RACE", split_fun = keep_2_levels("RACE")) |>
  analyze("AGE", afun = nice_mean)

tbl3b <- build_table(lyt3b, trim_adsl)
head(tbl3b)

## -----------------------------------------------------------------------------
lyt3c <- basic_table(top_level_section_div = "-") |>
  split_cols_by("ARM") |>
  analyze("AGE", afun = nice_mean) |>
  ## split_rows_by("SEX", nested = FALSE) |>
  ## analyze("AGE", afun = nice_mean) |>
  split_rows_by("RACE", nested = FALSE) |>
  analyze("AGE", afun = nice_mean)

tbl3c <- build_table(lyt3c, trim_adsl)
tbl3c

## -----------------------------------------------------------------------------
lyt4 <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("STRATA1", split_fun = keep_2_levels("STRATA1")) |>
  split_rows_by("SEX", split_fun = keep_2_levels("SEX")) |>
  analyze("AGE") |>
  analyze("BMRKR2")

build_table(lyt4, ex_adsl)

## -----------------------------------------------------------------------------
lyt4a <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("STRATA1", split_fun = keep_2_levels("STRATA1")) |>
  split_rows_by("SEX", split_fun = keep_2_levels("SEX")) |>
  analyze("AGE") |>
  analyze("BMRKR2", at_sibling = "SEX", show_labels = "visible")

build_table(lyt4a, ex_adsl)

## -----------------------------------------------------------------------------
lyt4b <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("STRATA1", split_fun = keep_2_levels("STRATA1")) |>
  split_rows_by("SEX", split_fun = keep_2_levels("SEX")) |>
  analyze("AGE") |>
  analyze("BMRKR2", at_sibling = "STRATA1", show_labels = "visible")

build_table(lyt4b, ex_adsl)

## -----------------------------------------------------------------------------
lyt4c <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("STRATA1", split_fun = keep_2_levels("STRATA1")) |>
  split_rows_by("SEX", split_fun = keep_2_levels("SEX")) |>
  analyze("AGE") |>
  analyze("BMRKR2", nested = FALSE, show_labels = "visible")

build_table(lyt4c, ex_adsl)

## -----------------------------------------------------------------------------
lyt4d <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("RACE", split_fun = keep_2_levels("RACE")) |>
  split_rows_by("STRATA1", split_fun = keep_2_levels("STRATA1")) |>
  split_rows_by("SEX", split_fun = keep_2_levels("SEX")) |>
  analyze("AGE") |>
  analyze("BMRKR2", at_sibling = "STRATA1", show_labels = "visible")

build_table(lyt4d, ex_adsl)

## -----------------------------------------------------------------------------
lyt4c <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("RACE", split_fun = keep_2_levels("RACE")) |>
  split_rows_by("STRATA1", split_fun = keep_2_levels("STRATA1")) |>
  split_rows_by("SEX", split_fun = keep_2_levels("SEX")) |>
  analyze("AGE") |>
  analyze("BMRKR2", nested = FALSE, show_labels = "visible")

build_table(lyt4c, ex_adsl)

## -----------------------------------------------------------------------------
complex_lyt <- basic_table() |>
  split_rows_by("STRATA1", split_fun = keep_2_levels("RACE")) |>
  split_rows_by("STRATA2", split_fun = keep_2_levels("STRATA2")) |>
  analyze("ARM") |>
  split_rows_by("SEX", split_fun = keep_2_levels("SEX")) |>
  split_rows_by("RACE", split_fun = keep_2_levels("RACE")) |>
  split_rows_by("STRATA1", split_fun = keep_2_levels("STRATA1")) |>
  analyze("BMRKR1") |>
  split_rows_by("BMRKR2", split_fun = keep_2_levels("BMRKR2"), at_sibling = "RACE") |>
  split_rows_by("COUNTRY", split_fun = keep_2_levels("COUNTRY")) |>
  analyze("AGE") |>
  split_rows_by("SITEID", split_fun = drop_split_levels, at_sibling = "RACE") |>
  split_rows_by("BEP01FL", split_fun = keep_2_levels("BEP01FL")) |>
  analyze("AGE")

## -----------------------------------------------------------------------------
get_row_anchor_list(complex_lyt)

## -----------------------------------------------------------------------------
lyt_stack <- basic_table() |>
  split_rows_by("STRATA1", split_fun = keep_2_levels("STRATA1")) |>
  split_rows_by("STRATA2", split_fun = keep_2_levels("STRATA2")) |>
  split_rows_by("RACE", split_fun = keep_2_levels("RACE")) |>
  split_rows_by("SEX", split_fun = keep_2_levels("SEX")) |>
  analyze("AGE")

## -----------------------------------------------------------------------------
lyt_stack2 <- lyt_stack |>
  analyze("BMRKR1", at_sibling = "STRATA2")

## ----error = TRUE-------------------------------------------------------------
try({
lyt_stack2 |>
  analyze("BMRKR2", at_sibling = "RACE")
})

## -----------------------------------------------------------------------------
lyt_stack3 <- lyt_stack |>
  analyze("BMRKR2", at_sibling = "RACE", show_labels = "visible") |>
  analyze("BMRKR1", at_sibling = "STRATA2", show_labels = "visible")

## -----------------------------------------------------------------------------
build_table(lyt_stack3, ex_adsl)

## -----------------------------------------------------------------------------
lyt_stack

## -----------------------------------------------------------------------------
lyt <- basic_table() |>
  split_cols_by("ARM") |>
  analyze("AGE") |>
  split_rows_by("RACE", split_fun = keep_2_levels("RACE")) |>
  analyze("AGE") |>
  split_rows_by("SEX", split_fun = keep_2_levels("SEX")) |>
  analyze("AGE")

build_table(lyt, ex_adsl)

## -----------------------------------------------------------------------------
lyt2 <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("STRATA1", split_fun = keep_2_levels("STRATA1")) |>
  analyze("AGE") |>
  split_rows_by("RACE", split_fun = keep_2_levels("RACE")) |>
  analyze("AGE") |>
  split_rows_by("SEX", split_fun = keep_2_levels("SEX")) |>
  analyze("AGE")

build_table(lyt2, ex_adsl)

## -----------------------------------------------------------------------------
lyt_good <- basic_table() |>
  split_cols_by("ARM") |>
  analyze("AGE") |>
  split_rows_by("RACE",
    split_fun = keep_2_levels("RACE"),
    at_sibling = "AGE"
  ) |>
  analyze("AGE") |>
  split_rows_by("SEX",
    split_fun = keep_2_levels("SEX"),
    at_sibling = "AGE"
  ) |>
  analyze("AGE")

build_table(lyt_good, ex_adsl)

## -----------------------------------------------------------------------------
lyt_good_subgrp <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("STRATA1", split_fun = keep_2_levels("STRATA1")) |>
  analyze("AGE") |>
  split_rows_by("RACE",
    split_fun = keep_2_levels("RACE"),
    at_sibling = "AGE"
  ) |>
  analyze("AGE") |>
  split_rows_by("SEX",
    split_fun = keep_2_levels("SEX"),
    at_sibling = "AGE"
  ) |>
  analyze("AGE")

build_table(lyt_good, ex_adsl)

