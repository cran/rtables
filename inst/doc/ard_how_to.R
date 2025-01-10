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

## -----------------------------------------------------------------------------
library(rtables)
ADSL <- ex_adsl # Example ADSL dataset

# Very simple table
lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  analyze(c("AGE", "SEX"))

tbl <- build_table(lyt, ADSL)
tbl

## -----------------------------------------------------------------------------
as_result_df(tbl)

as_result_df(tbl, data_format = "strings")
as_result_df(tbl, simplify = TRUE)
as_result_df(tbl, simplify = TRUE, keep_label_rows = TRUE)
as_result_df(tbl, simplify = TRUE, keep_label_rows = TRUE, expand_colnames = TRUE)

## -----------------------------------------------------------------------------
as_result_df(tbl, make_ard = TRUE)

## -----------------------------------------------------------------------------
# rcell and in_rows are the core of any analysis function
rc <- rcell(c(1, 2), stat_names = c("Rand1", "Rand2"))
print(obj_stat_names(rc)) # c("Rand1", "Rand2")

rc_row <- in_rows(
  .list = list(a = c(NA, 1), b = c(1, NA)),
  .formats = c("xx - xx", "xx.x - xx.x"),
  .format_na_strs = list(c("asda", "lkjklj")),
  .stat_names = list(c("A", "B"), c("B", "C"))
)

# Only a getter for this object
print(obj_stat_names(rc_row)) # list(a = c("A", "B"), b = c("B", "C"))

# if c("A", "B"), one for each row
# if single list, duplicated
rc_row <- in_rows(
  .list = list(a = c(NA, 1), b = c(1, NA)),
  .formats = c("xx - xx", "xx.x - xx.x"),
  .format_na_strs = list(c("asda", "lkjklj")),
  .stat_names = c("A", "B")
)
print(obj_stat_names(rc_row)) # c("A", "B") # one for each row
print(lapply(rc_row, obj_stat_names)) # identical to above + row names

rc_row <- in_rows(
  .list = list(a = c(NA, 1), b = c(1, NA)),
  .formats = c("xx - xx", "xx.x - xx.x"),
  .format_na_strs = list(c("asda", "lkjklj")),
  .stat_names = list(c("A", "B")) # It is duplicated, check it yourself!
)

## -----------------------------------------------------------------------------
mean_sd_custom <- function(x) {
  mean <- mean(x, na.rm = FALSE)
  sd <- sd(x, na.rm = FALSE)

  rcell(c(mean, sd),
    label = "Mean (SD)", format = "xx.x (xx.x)" # ,
    # stat_names = c("Mean", "SD")
  )
}
counts_percentage_custom <- function(x) {
  cnts <- table(x)
  out <- lapply(cnts, function(x) {
    perc <- x / sum(cnts)
    rcell(c(x, perc), format = "xx. (xx.%)")
  })
  in_rows(
    .list = as.list(out), .labels = names(cnts),
    .stat_names = list(c("Count", "Percentage"))
  )
}

lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") %>%
  split_cols_by("ARM", split_fun = keep_split_levels(c("A: Drug X", "B: Placebo"))) %>%
  analyze(vars = "AGE", afun = mean_sd_custom) %>%
  analyze(vars = "SEX", afun = counts_percentage_custom)

tbl <- build_table(lyt, ex_adsl)

as_result_df(tbl, make_ard = TRUE)

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_rows_by("STRATA2") %>%
  summarize_row_groups() %>%
  split_cols_by("ARM") %>%
  split_cols_by("STRATA1") %>%
  analyze(c("AGE", "SEX"))

tbl <- build_table(lyt, ex_adsl)

as_result_df(tbl, make_ard = TRUE)

