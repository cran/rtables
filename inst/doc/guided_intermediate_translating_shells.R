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

## ----init, echo = FALSE, results = "hidden"-----------------------------------
suppressPackageStartupMessages(library(rtables))
suppressPackageStartupMessages(library(dplyr))

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

## ----echo = FALSE, result = "asis"--------------------------------------------
adsl <- ex_adsl %>%
  filter(SEX %in% c("M", "F") & RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")) %>%
  mutate(
    BMEASIFL = factor(as.character(BMEASIFL),
      levels = c("Y", "N"),
      labels = c("Yes", "No")
    ),
    SEX = factor(as.character(SEX),
      levels = c("M", "F", "UNDIFFERENTIATED", "U"),
      labels = c("Male", "Female", "Undifferentiated", "Unknown")
    ),
    RACE = factor(as.character(RACE),
      levels = c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE"),
      labels = c("Asian", "Black", "White")
    )
  )


lyt <- basic_table(
  title = "Subject Response by Race and Sex; Treated Subjects",
  show_colcounts = TRUE
) %>%
  split_cols_by("STRATA1", split_fun = keep_split_levels(only = c("A", "B"))) %>%
  split_cols_by("ARM", split_fun = keep_split_levels(only = c("A: Drug X", "B: Placebo"))) %>%
  analyze("BMEASIFL", afun = counts_wpcts, var_labels = "All Patients", show_labels = "visible") %>%
  split_rows_by(
    var = "RACE",
    label_pos = "topleft",
    split_fun = keep_split_levels(only = c("Asian", "Black", "White"))
  ) %>%
  split_rows_by(
    var = "SEX",
    label_pos = "topleft",
    split_fun = keep_split_levels(only = c("Male", "Female"))
  ) %>%
  summarize_row_groups(
    var = "SEX",
    format = "xx"
  ) %>%
  analyze(
    vars = "BMEASIFL",
    afun = counts_wpcts
  )

result <- build_table(lyt, adsl)
fixed_shell(result)

## ----echo = FALSE, result="asis"----------------------------------------------
fixed_shell(result[0, ])

## -----------------------------------------------------------------------------
fixed_shell(result[0, c("A", "*")])

## -----------------------------------------------------------------------------
lyt_cols <- basic_table() |>
  split_cols_by("STRATA1", split_fun = keep_split_levels(only = c("A", "B"))) %>%
  split_cols_by("ARM", split_fun = keep_split_levels(only = c("A: Drug X", "B: Placebo")))

build_table(lyt_cols, adsl)

## -----------------------------------------------------------------------------
lyt_cols <- basic_table() |>
  split_cols_by("STRATA1", split_fun = keep_split_levels(only = c("A", "B"))) %>%
  split_cols_by("ARM",
    split_fun = keep_split_levels(only = c("A: Drug X", "B: Placebo")),
    show_colcounts = TRUE
  )

build_table(lyt_cols, adsl)

## ----echo = FALSE-------------------------------------------------------------
fixed_shell(tt_at_path(result, "BMEASIFL"))

## ----echo = FALSE-------------------------------------------------------------
fixed_shell(tt_at_path(result, "RACE"))

## -----------------------------------------------------------------------------
dummy_afun <- function(x, ...) in_rows("Analysis" = "-")
lyt_a <- basic_table() |>
  analyze("BMEASIFL",
    afun = dummy_afun,
    var_labels = "All Patients",
    show_labels = "visible"
  )

build_table(lyt_a, adsl)

## -----------------------------------------------------------------------------
lyt_b <- basic_table() |>
  split_rows_by("RACE") |>
  split_rows_by("SEX") |>
  analyze("BMEASIFL", afun = dummy_afun)

head(build_table(lyt_b, adsl), 30)

## -----------------------------------------------------------------------------
lyt_b2 <- basic_table() |>
  split_rows_by("RACE") |>
  split_rows_by("SEX", split_fun = keep_split_levels(only = c("Male", "Female"))) |>
  analyze("BMEASIFL", afun = dummy_afun)


build_table(lyt_b2, adsl)

## -----------------------------------------------------------------------------
lyt_b3 <- basic_table() |>
  analyze("BMEASIFL",
    afun = dummy_afun,
    var_labels = "All Patients",
    show_labels = "visible"
  ) |>
  split_rows_by("RACE") |>
  split_rows_by("SEX", split_fun = keep_split_levels(only = c("Male", "Female"))) |>
  analyze("BMEASIFL", afun = dummy_afun)

build_table(lyt_b3, adsl)

## -----------------------------------------------------------------------------
lyt_struct <- basic_table() |>
  split_cols_by("STRATA1", split_fun = keep_split_levels(only = c("A", "B"))) |>
  split_cols_by("ARM",
    split_fun = keep_split_levels(only = c("A: Drug X", "B: Placebo")),
    show_colcounts = TRUE
  ) |>
  analyze("BMEASIFL",
    afun = dummy_afun,
    var_labels = "All Patients",
    show_labels = "visible"
  ) |>
  split_rows_by("RACE") |>
  split_rows_by("SEX", split_fun = keep_split_levels(only = c("Male", "Female"))) |>
  analyze("BMEASIFL", afun = dummy_afun)

build_table(lyt_struct, adsl)

## -----------------------------------------------------------------------------
rw_lyt <- basic_table() |>
  analyze("BMEASIFL",
    var_labels = "All Patients",
    show_labels = "visible"
  )

build_table(rw_lyt, adsl)

## -----------------------------------------------------------------------------
rw_lyt_struct <- basic_table() |>
  split_cols_by("STRATA1", split_fun = keep_split_levels(only = c("A", "B"))) |>
  split_cols_by("ARM",
    split_fun = keep_split_levels(only = c("A: Drug X", "B: Placebo")),
    show_colcounts = TRUE
  ) |>
  analyze("BMEASIFL",
    var_labels = "All Patients",
    show_labels = "visible"
  ) |>
  split_rows_by("RACE") |>
  split_rows_by("SEX", split_fun = keep_split_levels(only = c("Male", "Female"))) |>
  analyze("BMEASIFL")

build_table(rw_lyt_struct, adsl)

## -----------------------------------------------------------------------------
rw_lyt_structb <- basic_table() |>
  split_cols_by("STRATA1", split_fun = keep_split_levels(only = c("A", "B"))) |>
  split_cols_by("ARM",
    split_fun = keep_split_levels(only = c("A: Drug X", "B: Placebo")),
    show_colcounts = TRUE
  ) |>
  analyze("BMEASIFL",
    afun = counts_wpcts,
    var_labels = "All Patients",
    show_labels = "visible"
  ) |>
  split_rows_by("RACE") |>
  split_rows_by("SEX", split_fun = keep_split_levels(only = c("Male", "Female"))) |>
  analyze("BMEASIFL", afun = counts_wpcts)

build_table(rw_lyt_structb, adsl)

## -----------------------------------------------------------------------------
lyt_final <- basic_table() |>
  split_cols_by("STRATA1", split_fun = keep_split_levels(only = c("A", "B"))) |>
  split_cols_by("ARM",
    split_fun = keep_split_levels(only = c("A: Drug X", "B: Placebo")),
    show_colcounts = TRUE
  ) |>
  analyze("BMEASIFL",
    afun = counts_wpcts,
    var_labels = "All Patients",
    show_labels = "visible"
  ) |>
  split_rows_by("RACE") |>
  split_rows_by("SEX", split_fun = keep_split_levels(only = c("Male", "Female"))) |>
  summarize_row_groups(format = "xx") |>
  analyze("BMEASIFL", afun = counts_wpcts)

build_table(lyt_final, adsl)

## ----echo = FALSE-------------------------------------------------------------
span_map <- data.frame(
  ARM = c("A: Drug X", "B: Placebo", "C: Combination"),
  span_label = c("Active Treatment", " ", "Active Treatment")
)

lyt_span <- basic_table() |>
  split_cols_by("span_label", split_fun = trim_levels_to_map(span_map)) |>
  split_cols_by("ARM", show_colcounts = TRUE)

adsl2 <- adsl
adsl2$span_label <- "Active Treatment"
adsl2$span_label[adsl2$ARM == "B: Placebo"] <- " "

tbl_colspans <- build_table(lyt_span, adsl2)
fixed_shell(tbl_colspans)

## -----------------------------------------------------------------------------
adsl_forspans <- adsl
adsl_forspans$span_label <- "Active Treatment"
adsl_forspans$span_label[adsl_forspans$ARM == "B: Placebo"] <- " "

qtable(adsl_forspans, "ARM", "span_label")

## -----------------------------------------------------------------------------
lyt_cspan <- basic_table() |>
  split_cols_by("span_label") |>
  split_cols_by("ARM", show_colcounts = TRUE)

build_table(lyt_cspan, adsl_forspans)

## -----------------------------------------------------------------------------
span_label_map <- tribble(
  ~span_label, ~ARM,
  "Active Treatment", "A: Drug X",
  "Active Treatment", "C: Combination",
  " ", "B: Placebo",
)

lyt_cspan_final <- basic_table() |>
  split_cols_by("span_label",
    split_fun = trim_levels_to_map(span_label_map)
  ) |>
  split_cols_by("ARM", show_colcounts = TRUE)

build_table(lyt_cspan_final, adsl_forspans)

## ----echo = FALSE-------------------------------------------------------------
adsl2$rr_header <- "Risk Differences"
adsl2$rr_label <- paste(adsl2$ARM, "vs B: Placebo")
lyt_rr <- basic_table() |>
  split_cols_by("span_label", split_fun = trim_levels_to_map(span_map)) |>
  split_cols_by("ARM", show_colcounts = TRUE) |>
  split_cols_by("rr_header", nested = FALSE) |>
  split_cols_by("ARM", labels_var = "rr_label", split_fun = remove_split_levels("B: Placebo"))


tbl_rr_shell <- build_table(lyt_rr, adsl2)
fixed_shell(tbl_rr_shell)

## -----------------------------------------------------------------------------
adsl_rr <- adsl_forspans
adsl_rr$rr_header <- "Risk Differences"

lyt_only_rr <- basic_table() |>
  split_cols_by("rr_header") |>
  split_cols_by("ARM")

build_table(lyt_only_rr, adsl_rr)

## -----------------------------------------------------------------------------
adsl_rr$rr_label <- paste(adsl_rr$ARM, "vs B: Placebo")

lyt_only_rr2 <- basic_table() |>
  split_cols_by("rr_header") |>
  split_cols_by("ARM",
    split_fun = remove_split_levels("B: Placebo"),
    labels_var = "rr_label"
  )

build_table(lyt_only_rr2, adsl_rr)

## -----------------------------------------------------------------------------
lyt_rr_cols <- basic_table() |>
  split_cols_by("span_label",
    split_fun = trim_levels_to_map(span_label_map)
  ) |>
  split_cols_by("ARM", show_colcounts = TRUE) |>
  split_cols_by("rr_header", nested = FALSE) |>
  split_cols_by("ARM",
    split_fun = remove_split_levels("B: Placebo"),
    labels_var = "rr_label"
  )

build_table(lyt_rr_cols, adsl_rr)

## -----------------------------------------------------------------------------
in_risk_diff <- function(spl_context) grepl("Risk Differences", spl_context$cur_col_id[1])

## -----------------------------------------------------------------------------
rr_afun <- function(x, .N_col, .spl_context) {
  xtbl <- table(x)
  if (in_risk_diff(.spl_context)) {
    armlabel <- tail(.spl_context$cur_col_split_val[[1]], 1) # last split value, ie arm
    armletter <- substr(armlabel, 1, 1)
    vals <- as.list(rep(paste(armletter, "vs B"), length(xtbl)))
    fmts <- rep("xx", length(xtbl))
  } else {
    vals <- lapply(xtbl, function(x) x * c(1, 1 / .N_col)) ## count and pct
    fmts <- rep("xx.x (xx.x%)", length(xtbl))
  }
  names(vals) <- names(xtbl)
  names(fmts) <- names(vals)
  in_rows(.list = vals, .formats = fmts)
}

## -----------------------------------------------------------------------------
lyt_rr_full <- basic_table() |>
  split_cols_by("span_label",
    split_fun = trim_levels_to_map(span_label_map)
  ) |>
  split_cols_by("ARM", show_colcounts = TRUE) |>
  split_cols_by("rr_header", nested = FALSE) |>
  split_cols_by("ARM",
    split_fun = remove_split_levels("B: Placebo"),
    labels_var = "rr_label"
  ) |>
  analyze("BMRKR2", afun = rr_afun)

build_table(lyt_rr_full, adsl_rr)

## -----------------------------------------------------------------------------
lyt_rr_full2 <- basic_table() |>
  split_cols_by("span_label",
    split_fun = trim_levels_to_map(span_label_map)
  ) |>
  split_cols_by("ARM", show_colcounts = TRUE) |>
  split_cols_by("rr_header", nested = FALSE) |>
  split_cols_by("ARM",
    split_fun = remove_split_levels("B: Placebo"),
    labels_var = "rr_label"
  ) |>
  split_rows_by("STRATA1") |>
  split_rows_by("SEX", split_fun = keep_split_levels(c("Female", "Male"))) |>
  analyze("BMRKR2", afun = rr_afun)

tbl <- build_table(lyt_rr_full2, adsl_rr)

cwidths <- propose_column_widths(tbl)
cwidths[cwidths > 15] <- 15
cat(export_as_txt(tbl, colwidths = cwidths)) ## for wrapping

## ----echo = FALSE-------------------------------------------------------------
simple_two_tier_init <- function(df, .var, .N_col, inner_var, drill_down_levs) {
  outer_tbl <- table(df[[.var]])

  cells <- lapply(
    names(outer_tbl),
    function(nm) {
      cont_cell <- rcell(outer_tbl[nm] * c(1, 1 / .N_col), format = "xx (xx.x%)")
      if (nm %in% drill_down_levs) {
        inner_tbl <- table(df[[inner_var]])
        detail_cells <- lapply(
          names(inner_tbl),
          function(innm) rcell(inner_tbl[innm] * c(1, 1 / .N_col), format = "xx (xx.x%)", indent_mod = 1L)
        )
        names(detail_cells) <- names(inner_tbl)
      } else {
        detail_cells <- NULL
      }
      c(setNames(list(cont_cell), nm), detail_cells)
    }
  )

  in_rows(.list = unlist(cells, recursive = FALSE))
}

two_tier_shell_lyt <- basic_table() |>
  split_cols_by("span_label",
    split_fun = trim_levels_to_map(span_label_map)
  ) |>
  split_cols_by("ARM", show_colcounts = TRUE) |>
  split_rows_by("RACE", split_fun = keep_split_levels(c("Asian", "Black"))) |>
  analyze("EOSSTT",
    afun = simple_two_tier_init,
    extra_args = list(
      inner_var = "DCSREAS",
      drill_down_levs = "DISCONTINUED"
    )
  )

two_tier_shell <- build_table(two_tier_shell_lyt, adsl_rr) ## don't need the rr bits
fixed_shell(two_tier_shell)

## -----------------------------------------------------------------------------
simple_two_tier <- function(df, .var, .N_col, inner_var, drill_down_levs) {
  ## group EOSSTT counts
  outer_tbl <- table(df[[.var]])

  cells <- lapply(
    names(outer_tbl),
    function(nm) {
      ## simulated group summary rows
      cont_cell <- rcell(outer_tbl[nm] * c(1, 1 / .N_col),
        format = "xx (xx.x%)"
      )
      if (nm %in% drill_down_levs) {
        ## detail (DCSREAS) counts
        inner_tbl <- table(df[[inner_var]])
        ## note indent_mod
        detail_cells <- lapply(
          names(inner_tbl),
          function(innm) {
            rcell(inner_tbl[innm] * c(1, 1 / .N_col),
              format = "xx (xx.x%)",
              ## appearance of "detail drill-down"
              indent_mod = 1L
            )
          }
        )
        names(detail_cells) <- names(inner_tbl)
      } else {
        detail_cells <- NULL
      }
      c(setNames(list(cont_cell), nm), detail_cells)
    }
  )

  in_rows(.list = unlist(cells, recursive = FALSE))
}

lyt_two_tier <- basic_table() |>
  analyze("EOSSTT",
    afun = simple_two_tier,
    extra_args = list(inner_var = "DCSREAS", drill_down_levs = "DISCONTINUED")
  )

build_table(lyt_two_tier, adsl_rr)

## -----------------------------------------------------------------------------
lyt_two_tier_full <- basic_table() |>
  split_cols_by("span_label",
    split_fun = trim_levels_to_map(span_label_map)
  ) |>
  split_cols_by("ARM", show_colcounts = TRUE) |>
  split_rows_by("RACE", split_fun = keep_split_levels(c("Asian", "Black"))) |>
  analyze("EOSSTT",
    afun = simple_two_tier,
    extra_args = list(inner_var = "DCSREAS", drill_down_levs = "DISCONTINUED")
  )

build_table(lyt_two_tier_full, adsl_rr)

