## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(comment = "#")

## ---- message=FALSE-----------------------------------------------------------
library(rtables)

lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  analyze("AGE")

tbl <- build_table(lyt, DM)
tbl

## -----------------------------------------------------------------------------
lyt2 <- basic_table() %>%
  split_cols_by("ARM", ref_group = "B: Placebo") %>%
  analyze("AGE", afun = function(x, .ref_group) {
    in_rows(
      "Difference of Averages" = rcell(mean(x) - mean(.ref_group), format = "xx.xx")
    )
  })

tbl2 <- build_table(lyt2, DM)
tbl2

## -----------------------------------------------------------------------------
lyt3 <- basic_table() %>%
  split_cols_by("ARM", ref_group = "B: Placebo") %>%
  analyze("AGE", afun = function(x, .ref_group, .in_ref_col) {
      in_rows("Difference of Averages" = non_ref_rcell(mean(x) - mean(.ref_group),
                                                       is_ref = .in_ref_col,
                                                       format = "xx.xx"))
  })

tbl3 <- build_table(lyt3, DM)
tbl3

lyt4 <- basic_table() %>%
  split_cols_by("ARM", ref_group = "B: Placebo") %>%
  analyze("AGE", afun = function(x, .ref_group, .in_ref_col) {
      in_rows(
          "Difference of Averages" = non_ref_rcell(mean(x) - mean(.ref_group),
                                                   is_ref = .in_ref_col,
                                                   format = "xx.xx"),
         "another row" = non_ref_rcell("aaa", .in_ref_col)
      )
  })

tbl4 <- build_table(lyt4, DM)
tbl4

## -----------------------------------------------------------------------------
lyt5 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM", ref_group = "B: Placebo") %>%
  split_rows_by("SEX", split_fun = drop_split_levels) %>%
  analyze("AGE", afun = function(x, .ref_group, .ref_full, .in_ref_col) {
    in_rows(
      "is reference (.in_ref_col)" = rcell(.in_ref_col),
      "ref cell N (.ref_group)" = rcell(length(.ref_group)),
      "ref column N (.ref_full)" = rcell(length(.ref_full))
    )
  })

tbl5 <- build_table(lyt5, subset(DM, SEX %in% c("M", "F")))
tbl5

