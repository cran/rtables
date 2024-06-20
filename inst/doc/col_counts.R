## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(comment = "#")

## -----------------------------------------------------------------------------
library(dplyr)
library(rtables)
lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  split_cols_by("SEX", split_fun = keep_split_levels(c("F", "M"))) %>%
  analyze("AGE")

tbl <- build_table(lyt, ex_adsl)
tbl

## -----------------------------------------------------------------------------
col_counts(tbl) <- c(17, 18, 19, 17, 18, 19)
tbl

## -----------------------------------------------------------------------------
col_counts(tbl) <- c(17, 18, NA, 17, 18, 19)
tbl

## -----------------------------------------------------------------------------
lyt2 <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_cols_by("SEX",
    split_fun = keep_split_levels(c("F", "M")),
    show_colcounts = TRUE
  ) %>%
  analyze("AGE")

tbl2 <- build_table(lyt2, ex_adsl)
tbl2

## -----------------------------------------------------------------------------
lyt3 <- basic_table() %>%
  split_cols_by("ARM", show_colcounts = TRUE) %>%
  split_cols_by("SEX", split_fun = keep_split_levels(c("F", "M"))) %>%
  analyze("AGE")

tbl3 <- build_table(lyt3, ex_adsl)
tbl3

## -----------------------------------------------------------------------------
facet_colcount(tbl3, c("ARM", "C: Combination"))

## -----------------------------------------------------------------------------
facet_colcount(tbl3, c("ARM", "C: Combination")) <- 75
tbl3

## -----------------------------------------------------------------------------
facet_colcounts_visible(tbl3, c("ARM", "A: Drug X", "SEX")) <- TRUE
tbl3

## ----error=TRUE---------------------------------------------------------------
## BEWARE
tbl4 <- tbl3
colcount_visible(tbl4, c("ARM", "A: Drug X", "SEX", "F")) <- FALSE
tbl4

## -----------------------------------------------------------------------------
coldf <- make_col_df(tbl3)
facet_colcount(tbl3, coldf$path[[1]][c(1, 2)]) <- NA_integer_
print(tbl3) # Keeps the missing space
colcount_na_str(tbl3) <- "NaN"
tbl3 # Shows NaN

