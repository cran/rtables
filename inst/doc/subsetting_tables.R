## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(comment = "")

## -----------------------------------------------------------------------------
library(rtables)
library(dplyr)

tbl <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX", split_fun = drop_split_levels) %>%
  analyze(c("AGE", "STRATA1")) %>%
  build_table(ex_adsl %>% filter(SEX %in% c("M", "F")))

tbl

## -----------------------------------------------------------------------------
tbl[1, 1]

## -----------------------------------------------------------------------------
tbl[3, 1]

## -----------------------------------------------------------------------------
tbl[3, 1, drop = TRUE]

## -----------------------------------------------------------------------------
tbl[1:3, 1:2]

## -----------------------------------------------------------------------------
tbl[2:4, ]

## -----------------------------------------------------------------------------
tbl2 <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_cols_by("SEX", split_fun = drop_split_levels) %>%
  analyze(c("AGE", "STRATA1")) %>%
  build_table(ex_adsl %>% filter(SEX %in% c("M", "F")))

tbl2

## -----------------------------------------------------------------------------
col_paths_summary(tbl2)

## -----------------------------------------------------------------------------
row_paths_summary(tbl2)

## -----------------------------------------------------------------------------
value_at(tbl2, c("AGE",  "Mean"), c("ARM", "A: Drug X", "SEX", "F"))

## -----------------------------------------------------------------------------
cell_values(tbl2, c("AGE", "Mean"), c("ARM", "A: Drug X"))

## -----------------------------------------------------------------------------
cell_values(tbl2, c("AGE",  "Mean"), c("ARM", "A: Drug X", "SEX", "F"))

