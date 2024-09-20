## ----include = FALSE----------------------------------------------------------
suggested_dependent_pkgs <- c("dplyr", "tibble")
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

lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX", split_fun = drop_split_levels) %>%
  analyze(c("AGE", "STRATA1"))

tbl <- build_table(lyt, ex_adsl %>% filter(SEX %in% c("M", "F")))
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
tbl[, c("ARM", "A: Drug X")]

## -----------------------------------------------------------------------------
top_left(tbl) <- "SEX"
main_title(tbl) <- "Table 1"
subtitles(tbl) <- c("Authors:", " - Abcd Zabcd", " - Cde Zbcd")

main_footer(tbl) <- "Please regard this table as an example of smart subsetting"
prov_footer(tbl) <- "Do remember where you read this though"

fnotes_at_path(tbl, rowpath = c("M", "AGE", "Mean"), colpath = c("ARM", "A: Drug X")) <- "Very important mean"

## -----------------------------------------------------------------------------
tbl[3, 3]

## -----------------------------------------------------------------------------
tbl[, 2:3]
tbl[1:3, 3, keep_topleft = TRUE]

## -----------------------------------------------------------------------------
tbl[10, 1]
col_paths_summary(tbl) # Use these to find the right path to value or label
row_paths_summary(tbl) #

# To select column value, use `NULL` for `rowpath`
fnotes_at_path(tbl, rowpath = NULL, colpath = c("ARM", "A: Drug X")) <- "Interesting"
tbl[3, 1]

# reindexing of {2} as {1}
fnotes_at_path(tbl, rowpath = c("M", "AGE", "Mean"), colpath = NULL) <- "THIS mean"
tbl # {1}, {2}, and {3} are present
tbl[10, 2] # only {1} which was previously {2}

## -----------------------------------------------------------------------------
tbl[1:3, 2:3, keep_titles = TRUE]
tbl[1:3, 2:3, keep_titles = FALSE, keep_footers = TRUE]

# Referential footnotes are not influenced by `keep_footers = FALSE`
tbl[1:3, keep_titles = TRUE, keep_footers = FALSE]

## -----------------------------------------------------------------------------
lyt2 <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_cols_by("SEX", split_fun = drop_split_levels) %>%
  split_rows_by("RACE", split_fun = drop_split_levels) %>%
  summarize_row_groups() %>%
  analyze(c("AGE", "STRATA1"))

tbl2 <- build_table(lyt2, ex_adsl %>% filter(SEX %in% c("M", "F") & RACE %in% (levels(RACE)[1:3])))
tbl2

## -----------------------------------------------------------------------------
col_paths_summary(tbl2)

## -----------------------------------------------------------------------------
row_paths_summary(tbl2)

## -----------------------------------------------------------------------------
tbl2[c("RACE", "ASIAN"), c("ARM", "C: Combination")]

## -----------------------------------------------------------------------------
value_at(tbl2, c("RACE", "ASIAN", "AGE", "Mean"), c("ARM", "A: Drug X", "SEX", "F"))

## -----------------------------------------------------------------------------
cell_values(tbl2, c("RACE", "ASIAN", "AGE", "Mean"), c("ARM", "A: Drug X"))

## -----------------------------------------------------------------------------
cell_values(tbl2, c("RACE", "ASIAN", "AGE", "Mean"), c("ARM", "A: Drug X", "SEX", "F"))

