## ----include = FALSE----------------------------------------------------------
suggested_dependent_pkgs <- c("dplyr")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = all(vapply(
    suggested_dependent_pkgs,
    requireNamespace,
    logical(1),
    quietly = TRUE
  ))
)

## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(comment = "#")

## ----message=FALSE------------------------------------------------------------
library(rtables)
library(dplyr)

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("ARMCD", show_colcounts = TRUE, colcount_format = "N=xx") %>%
  split_cols_by("STRATA2", show_colcounts = TRUE) %>%
  split_rows_by("STRATA1") %>%
  add_overall_col("All") %>%
  summarize_row_groups() %>%
  analyze("AGE", afun = max, format = "xx.x")

tbl <- build_table(lyt, ex_adsl)
tbl

## -----------------------------------------------------------------------------
dim(tbl)
nrow(tbl)
ncol(tbl)

## -----------------------------------------------------------------------------
table_structure(tbl)

## -----------------------------------------------------------------------------
table_structure(tbl, detail = "row") # or "subtable"

## -----------------------------------------------------------------------------
coltree_structure(tbl)

## -----------------------------------------------------------------------------
make_row_df(tbl)[, c("label", "name", "abs_rownumber", "path", "node_class")]

## -----------------------------------------------------------------------------
row_paths(tbl)

## -----------------------------------------------------------------------------
make_row_df(tbl, visible_only = FALSE)[, c("label", "name", "abs_rownumber", "path", "node_class")]

## -----------------------------------------------------------------------------
make_col_df(tbl)[, c("label", "name", "abs_pos", "path", "leaf_indices")]

## -----------------------------------------------------------------------------
make_col_df(tbl, visible_only = FALSE)[, c("label", "name", "abs_pos", "path", "leaf_indices")]

## -----------------------------------------------------------------------------
col_paths(tbl)

## -----------------------------------------------------------------------------
row_paths_summary(tbl)

## -----------------------------------------------------------------------------
col_paths_summary(tbl)

## -----------------------------------------------------------------------------
table_shell(tbl)

## -----------------------------------------------------------------------------
value_formats(tbl)

