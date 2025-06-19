## -----------------------------------------------------------------------------
library(rtables)

keep_rc <- c("ASIAN", "WHITE") ## chosen for brevity

afun <- function(x) {
  list(
    Mean = rcell(mean(x), format = "xx.x"),
    Median = rcell(median(x), format = "xx.x")
  )
}

lyt <- basic_table() |>
  split_cols_by("ARM", split_fun = keep_split_levels(c("A: Drug X", "C: Combination"))) |>
  split_cols_by("SEX", split_fun = keep_split_levels(c("F", "M"))) |>
  add_overall_col("All") |>
  split_rows_by("RACE", split_fun = keep_split_levels(keep_rc)) |>
  summarize_row_groups() |>
  split_rows_by("STRATA1") |>
  summarize_row_groups() |>
  analyze("AGE", afun = afun) |>
  analyze("BMRKR1", nested = FALSE, show_labels = "visible")

tbl <- build_table(lyt, DM)
tbl

## -----------------------------------------------------------------------------
table_structure(tbl)

## -----------------------------------------------------------------------------
col_info(tbl)

## -----------------------------------------------------------------------------
rpsummry <- row_paths_summary(tbl)

## -----------------------------------------------------------------------------
head(rpsummry)

tbl[rpsummry$path[[6]], ]

## -----------------------------------------------------------------------------
tbl[c("RACE", "ASIAN"), ]

## -----------------------------------------------------------------------------
tbl[c("RACE", "WHITE", "STRATA1", "B"), ]

## -----------------------------------------------------------------------------
table_structure(tbl[c("RACE", "WHITE", "STRATA1", "B"), ])

## -----------------------------------------------------------------------------
tbl[c("RACE", "ASIAN", "@content"), ]

## -----------------------------------------------------------------------------
tbl[c("RACE", "WHITE", "STRATA1", "B", "AGE"), ]
tbl[c("RACE", "WHITE", "STRATA1", "B", "AGE", "Median"), ]

## -----------------------------------------------------------------------------
col_paths_summary(tbl)

## -----------------------------------------------------------------------------
head(tbl[, c("ARM", "A: Drug X")])
head(tbl[, c("ARM", "C: Combination", "SEX", "M")])
head(tbl[, c("All", "All")])

## -----------------------------------------------------------------------------
lytdup <- basic_table() |>
  analyze("STRATA1") |>
  split_rows_by("STRATA1") |>
  analyze("AGE")

tbldup <- build_table(lytdup, DM)
tbldup

## -----------------------------------------------------------------------------
row_paths_summary(tbldup)

## -----------------------------------------------------------------------------
tbldup[c("STRATA1", "A"), ]

tbldup[c("STRATA1[2]", "A"), ]

## -----------------------------------------------------------------------------
tbl[c("RACE", "*", "STRATA1", "B", "AGE", "Median"), ]

## -----------------------------------------------------------------------------
tbl[c("RACE", "*", "STRATA1", "*", "AGE", "Median"), ]

## -----------------------------------------------------------------------------
tt_normalize_row_path(tbl, c("RACE", "*", "STRATA1", "*", "AGE", "Median"))

## -----------------------------------------------------------------------------
tt_row_path_exists(tbl, c("RACE", "*", "STRATA1", "*", "AGE", "Median"))

## -----------------------------------------------------------------------------
tt_row_path_exists(tbl, c("RACE", "*", "STRATA1", "*", "FAKEFAKEFAKE", "Median"))

## -----------------------------------------------------------------------------
tt_normalize_row_path(tbl, c("*", "Mean"))

## -----------------------------------------------------------------------------
tbl[, c("ARM", "*", "SEX", "F")]

## -----------------------------------------------------------------------------
tbl2 <- head(tbl)
facet_colcounts_visible(tbl2, c("ARM", "A: Drug X", "SEX")) <- TRUE
tbl2

## -----------------------------------------------------------------------------
facet_colcount(tbl2, c("ARM", "A: Drug X", "SEX", "M")) <- 5
tbl2

## -----------------------------------------------------------------------------
facet_colcount(tbl2, c("ARM", "A: Drug X", "SEX", "F")) <- NA_integer_
tbl2

## -----------------------------------------------------------------------------
tbl3 <- tbl

section_div_at_path(tbl3, c("RACE", "*")) <- "*"
section_div_at_path(tbl3, c("RACE", "*", "STRATA1", "B")) <- "+"
tbl3

## -----------------------------------------------------------------------------
tree_children(tbl)

## -----------------------------------------------------------------------------
multi_step_children <- function(tbl, indices) {
  print(obj_name(tbl))
  ret <- tree_children(tbl)
  for (i in indices) {
    print(obj_name(ret[[i]]))
    ret <- tree_children(ret[[i]])
  }
  ret
}

## -----------------------------------------------------------------------------
multi_step_children(tbl, 1)

## -----------------------------------------------------------------------------
multi_step_children(tbl, 2)

## -----------------------------------------------------------------------------
multi_step_children(tbl, c(1, 1))

## -----------------------------------------------------------------------------
multi_step_children(tbl, c(1, 1, 1))

## -----------------------------------------------------------------------------
multi_step_children(tbl, c(1, 1, 1, 2))

## -----------------------------------------------------------------------------
multi_step_children(tbl, c(1, 1, 1, 2, 1))

## -----------------------------------------------------------------------------
## child is AGE analysis table within RACE->WHITE->STRATA1->A
multi_step_children(tbl, c(1, 2, 1, 1))

## -----------------------------------------------------------------------------
## children are individual rows of that AGE table
multi_step_children(tbl, c(1, 2, 1, 1, 1))

## -----------------------------------------------------------------------------
tb <- multi_step_children(tbl, c(1, 1, 1))[[2]] ## second ie B strata
tb

content_table(tb)

## -----------------------------------------------------------------------------
obj_label(tb)

## -----------------------------------------------------------------------------
col_paths_summary(tbl)

