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

## ----init, echo = FALSE, results = "hidden"-----------------------------------
suppressPackageStartupMessages(library(rtables))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tibble))

## XXX put this somewhere else so everyone can share it
fixed_shell <- function(tt) {
  mystr <- table_shell_str(tt)
  regex_hits <- gregexpr("[(]N=[[:digit:]]+[)]", mystr)[[1]]
  hit_lens <- attr(regex_hits, "match.length")
  if (regex_hits[1] > 0) {
    for (i in seq_along(regex_hits)) {
      start <- regex_hits[i]
      len <- hit_lens[i]
      substr(mystr, start, start + len - 1) <- padstr("(N=xx)", len, just = "center")
    }
  }
  cat(mystr)
}


knitr::opts_chunk$set(comment = "")

## -----------------------------------------------------------------------------
lyt <- basic_table() |>
  split_cols_by("ARM") |>
  split_cols_by("SEX")

build_table(lyt, ex_adsl)

## -----------------------------------------------------------------------------
lyt2 <- basic_table() |>
  split_rows_by("STRATA1") |>
  split_rows_by("BMRKR2") |>
  analyze("AGE")

build_table(lyt2, ex_adsl)

## -----------------------------------------------------------------------------
adsl <- subset(ex_adsl, as.character(SEX) %in% c("F", "M", "U"))
qtable(adsl, col_vars = "SEX")

## -----------------------------------------------------------------------------
lyt_pre <- basic_table() |>
  split_cols_by("SEX", split_fun = remove_split_levels("U")) |>
  analyze("STRATA1")

build_table(lyt_pre, adsl)

## -----------------------------------------------------------------------------
lyt_emp <- basic_table() |>
  split_cols_by("SEX", split_fun = drop_split_levels) |>
  analyze("STRATA1")

build_table(lyt_emp, adsl)

## -----------------------------------------------------------------------------
lyt_bad_emp <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("RACE", split_fun = drop_split_levels) |>
  split_rows_by("SEX", split_fun = drop_split_levels) |>
  analyze("AGE")

build_table(lyt_bad_emp, adsl)

## -----------------------------------------------------------------------------
combodf <- tribble(
  ~valname, ~label, ~levelcombo, ~exargs,
  "A_C", "Arms A+C", c("A: Drug X", "C: Combination"), list()
)

lyt_combo1 <- basic_table() |>
  split_cols_by("ARM", split_fun = add_combo_levels(combodf), show_colcounts = TRUE)

build_table(lyt_combo1, ex_adsl)

## -----------------------------------------------------------------------------
lyt_tig <- basic_table() |>
  split_rows_by("AESOC", split_fun = trim_levels_in_group("AEDECOD")) |>
  analyze("AEDECOD")

build_table(lyt_tig, ex_adae)

## -----------------------------------------------------------------------------
lyt_tig2 <- basic_table(title = "Observed Toxicity Grades") |>
  split_rows_by("AESOC", split_fun = trim_levels_in_group("AEDECOD")) |>
  split_rows_by("AEDECOD", split_fun = trim_levels_in_group("AETOXGR")) |>
  analyze("AETOXGR")

build_table(lyt_tig2, ex_adae)

## -----------------------------------------------------------------------------
map <- tribble(
  ~AESOC, ~AEDECOD,
  "cl A", "dcd A.1.1.1.2",
  "cl B", "dcd B.1.1.1.1",
  "cl B", "dcd B.2.2.3.1",
  "cl D", "dcd D.1.1.1.1"
)

lyt_ttm <- basic_table() |>
  split_rows_by("AESOC", split_fun = trim_levels_to_map(map)) |>
  analyze("AEDECOD")

build_table(lyt_ttm, ex_adae)

## -----------------------------------------------------------------------------
lyt_ttm2 <- basic_table() |>
  split_rows_by("AESOC", split_fun = trim_levels_to_map(map)) |>
  split_rows_by("AEDECOD", split_fun = trim_levels_in_group("AETOXGR")) |>
  analyze("AETOXGR")

build_table(lyt_ttm2, ex_adae)

## ----echo = FALSE-------------------------------------------------------------
library(tibble)

tpose_afun <- function(x, .var, .spl_context) {
  spldf <<- .spl_context
  mycol <- tail(tail(.spl_context$cur_col_split_val, 1)[[1]], 1)
  cell <- switch(mycol,
    n = rcell(length(x), format = "xx"),
    mean = rcell(mean(x, na.rm = TRUE), format = "xx.x"),
    sd = rcell(sd(x, na.rm = TRUE), format = "xx.xx")
  )
  in_rows(.list = setNames(list(cell), .var))
}

combo_df <- tribble(
  ~valname, ~label, ~levelcombo, ~exargs,
  "n", "n", select_all_levels, list(),
  "mean", "mean", select_all_levels, list(),
  "sd", "sd", select_all_levels, list()
)


lyt_sem_cols <- basic_table() |>
  split_cols_by("ARM") |>
  split_cols_by("STUDYID", split_fun = add_combo_levels(combo_df, keep_levels = combo_df$valname)) |>
  split_rows_by("SEX", split_fun = keep_split_levels(c("F", "M"))) |>
  analyze(c("AGE", "BMRKR1"), afun = tpose_afun, show_labels = "hidden")

fixed_shell(build_table(lyt_sem_cols, ex_adsl))

## -----------------------------------------------------------------------------
my_combo_df <- tribble(
  ~valname, ~label, ~levelcombo, ~exargs,
  "n", "n", select_all_levels, list(),
  "mean", "mean", select_all_levels, list(),
  "sd", "sd", select_all_levels, list()
)

lyt_tpose_cols_only <- basic_table() |>
  split_cols_by("ARM", show_colcounts = TRUE) |>
  split_cols_by("STUDYID",
    split_fun = add_combo_levels(my_combo_df, keep_levels = combo_df$valname),
    show_colcounts = TRUE
  )

build_table(lyt_tpose_cols_only, ex_adsl)

## -----------------------------------------------------------------------------
lyt_tpose_full <- basic_table() |>
  split_cols_by("ARM", show_colcounts = TRUE) |>
  split_cols_by("STUDYID",
    split_fun = add_combo_levels(my_combo_df, keep_levels = combo_df$valname),
    show_colcounts = TRUE
  ) |>
  split_rows_by("SEX", split_fun = keep_split_levels(c("F", "M"))) |>
  analyze(c("AGE", "BMRKR1"), afun = tpose_afun, show_labels = "hidden")

build_table(lyt_tpose_full, ex_adsl)

