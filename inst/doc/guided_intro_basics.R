## -----------------------------------------------------------------------------
library(rtables)
lyt <- basic_table() |>
  analyze("AGE")

build_table(lyt, ex_adsl)

## -----------------------------------------------------------------------------
lyt2 <- basic_table() |>
  analyze("BMRKR2")

build_table(lyt2, ex_adsl)

## -----------------------------------------------------------------------------
lyt3 <- basic_table() |>
  split_cols_by("ARM") |>
  analyze("BMRKR2")

build_table(lyt3, ex_adsl)

## -----------------------------------------------------------------------------
lyt4 <- basic_table() |>
  split_rows_by("SEX") |>
  analyze("BMRKR2")

build_table(lyt4, ex_adsl)

## -----------------------------------------------------------------------------
lyt5 <- basic_table() |>
  split_rows_by("SEX") |>
  summarize_row_groups("SEX") |>
  analyze("BMRKR2")

build_table(lyt5, ex_adsl)

## -----------------------------------------------------------------------------
lyt_basic <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("SEX") |>
  summarize_row_groups("SEX") |>
  analyze("BMRKR2")

build_table(lyt_basic, ex_adsl)

