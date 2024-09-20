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

raw_lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_cols_by("SEX") %>%
  split_rows_by("RACE") %>%
  summarize_row_groups() %>%
  split_rows_by("STRATA1") %>%
  summarize_row_groups() %>%
  analyze("AGE")

raw_tbl <- build_table(raw_lyt, DM)
raw_tbl

## -----------------------------------------------------------------------------
trim_rows(raw_tbl)

## -----------------------------------------------------------------------------
coltrimmed <- raw_tbl[, col_counts(raw_tbl) > 0]
h_coltrimmed <- head(coltrimmed, n = 14)
h_coltrimmed

## -----------------------------------------------------------------------------
table_structure(h_coltrimmed)

## -----------------------------------------------------------------------------
row_paths_summary(h_coltrimmed)

## -----------------------------------------------------------------------------
pruned <- prune_table(coltrimmed)
pruned

## -----------------------------------------------------------------------------
pruned2 <- prune_table(coltrimmed, low_obs_pruner(10, "mean"))
pruned2

## -----------------------------------------------------------------------------
pruned3 <- prune_table(coltrimmed, low_obs_pruner(10, "sum"), stop_depth = 1)
pruned3

## -----------------------------------------------------------------------------
pruned4 <- prune_table(coltrimmed, low_obs_pruner(16, "sum"))
pruned4

## -----------------------------------------------------------------------------
cont_n_allcols

## -----------------------------------------------------------------------------
sort_at_path(pruned, path = c("RACE", "ASIAN", "STRATA1"), scorefun = cont_n_allcols)
# B and C are swapped as the global count (sum of all column counts) of strata C is higher than the one of strata B

## -----------------------------------------------------------------------------
sort_at_path(pruned, path = c("RACE", "*", "STRATA1"), scorefun = cont_n_allcols)
# All subtables, i.e. ASIAN, BLACK..., and WHITE, are reordered separately

## -----------------------------------------------------------------------------
tmptbl <- sort_at_path(pruned, path = c("RACE", "ASIAN", "STRATA1"), scorefun = cont_n_allcols)
tmptbl <- sort_at_path(tmptbl, path = c("RACE", "BLACK OR AFRICAN AMERICAN", "STRATA1"), scorefun = cont_n_allcols)
tmptbl <- sort_at_path(tmptbl, path = c("RACE", "WHITE", "STRATA1"), scorefun = cont_n_allcols)
tmptbl

## -----------------------------------------------------------------------------
table_structure(pruned)

## -----------------------------------------------------------------------------
row_paths_summary(pruned)

## -----------------------------------------------------------------------------
ethsort <- sort_at_path(pruned, path = c("RACE"), scorefun = cont_n_allcols, decreasing = FALSE)
ethsort

## -----------------------------------------------------------------------------
sort_at_path(pruned, path = c("RACE", "*", "STRATA1"), cont_n_onecol(5))

## -----------------------------------------------------------------------------
more_analysis_fnc <- function(x) {
  in_rows(
    "median" = median(x),
    "mean" = mean(x),
    .formats = "xx.x"
  )
}

raw_lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by(
    "RACE",
    split_fun = drop_and_remove_levels("WHITE") # dropping WHITE levels
  ) %>%
  summarize_row_groups() %>%
  split_rows_by("STRATA1") %>%
  summarize_row_groups() %>%
  analyze("AGE", afun = more_analysis_fnc)

tbl <- build_table(raw_lyt, DM) %>%
  prune_table() %>%
  print()

## -----------------------------------------------------------------------------
table_structure(tbl) # Direct inspection into the tree-like structure of rtables

## -----------------------------------------------------------------------------
scorefun <- function(tt) {
  # Here we could use browser()
  sum(unlist(row_values(tt)))
}
sort_at_path(tbl, c("RACE", "*", "STRATA1", "*", "AGE"), scorefun)

## -----------------------------------------------------------------------------
cont_n_onecol

## -----------------------------------------------------------------------------
scorefun_onecol <- function(colpath) {
  function(tt) {
    # Here we could use browser()
    unlist(cell_values(tt, colpath = colpath), use.names = FALSE)[1] # Modified to lose the list names
  }
}
sort_at_path(tbl, c("RACE", "*", "STRATA1", "*", "AGE"), scorefun_onecol(colpath = c("ARM", "A: Drug X")))

## -----------------------------------------------------------------------------
# Simpler table
tbl <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_cols_by("SEX",
    split_fun = drop_and_remove_levels(c("U", "UNDIFFERENTIATED"))
  ) %>%
  analyze("AGE", afun = more_analysis_fnc) %>%
  build_table(DM) %>%
  prune_table() %>%
  print()

sort_at_path(tbl, c("AGE"), scorefun_onecol(colpath = c("ARM", "B: Placebo", "SEX", "F")))

## -----------------------------------------------------------------------------
silly_name_scorer <- function(tt) {
  nm <- obj_name(tt)
  print(nm)
  nm
}

sort_at_path(ethsort, "RACE", silly_name_scorer) # Now, it is sorted alphabetically!

## -----------------------------------------------------------------------------
silly_gender_diffcount <- function(tt) {
  ## (1st) content row has same name as object (STRATA1 level)
  rpath <- c(obj_name(tt), "@content", obj_name(tt))
  ## the [1] below is cause these are count (pct%) cells
  ## and we only want the count part!
  mcount <- unlist(cell_values(
    tt,
    rowpath = rpath,
    colpath = c("ARM", "C: Combination", "SEX", "M")
  ))[1]
  fcount <- unlist(cell_values(
    tt,
    rowpath = rpath,
    colpath = c("ARM", "C: Combination", "SEX", "F")
  ))[1]
  (mcount - fcount) / fcount
}

sort_at_path(pruned, c("RACE", "*", "STRATA1"), silly_gender_diffcount)

