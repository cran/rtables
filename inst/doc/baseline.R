## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(comment = "")

## -----------------------------------------------------------------------------
library(rtables)

basic_table() %>%
  split_cols_by("ARM") %>%
  analyze("AGE") %>%
  build_table(DM)

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by("ARM", ref_group = "B: Placebo") %>%
  analyze("AGE", afun = function(x, .ref_group) {
    in_rows(
      "Difference of Averages" = rcell(mean(x) - mean(.ref_group), format = "xx.xx")
    )
  }) %>%
  build_table(DM)

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by("ARM", ref_group = "B: Placebo") %>%
  analyze("AGE", afun = function(x, .ref_group, .in_ref_col){
      in_rows("Difference of Averages" = non_ref_rcell(mean(x) - mean(.ref_group),
                                                       is_ref = .in_ref_col,
                                                       format = "xx.xx"))
  }) %>%
  build_table(DM)

basic_table() %>%
  split_cols_by("ARM", ref_group = "B: Placebo") %>%
  analyze("AGE", afun = function(x, .ref_group, .in_ref_col){
      in_rows(
          "Difference of Averages" = non_ref_rcell(mean(x) - mean(.ref_group),
                                                   is_ref = .in_ref_col,
                                                   format = "xx.xx"),
         "another row" = non_ref_rcell("aaa", .in_ref_col)
      )
  }) %>%
  build_table(DM)


## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by("ARM", ref_group = "B: Placebo") %>%
  add_colcounts() %>%
  split_rows_by("SEX", split_fun = drop_split_levels) %>%
  analyze("AGE", afun = function(x, .ref_group, .ref_full, .in_ref_col){
    in_rows(
      "is reference (.in_ref_col)" = rcell(.in_ref_col),
      "ref cell N (.ref_group)" = rcell(length(.ref_group)),
      "ref column N (.ref_full)" = rcell(length(.ref_full))
    )
  }) %>%
  build_table(subset(DM, SEX %in% c("M", "F")))

