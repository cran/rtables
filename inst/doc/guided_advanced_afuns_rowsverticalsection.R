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
rvs1 <- in_rows(what = 17.123, .formats = c(what = "xx.x"))
rvs1

## -----------------------------------------------------------------------------
rvs2 <- in_rows(
  ok = "hi",
  nah = "bye",
  .indent_mods = c(ok = 1, nah = -1), .row_footnotes = list(nah = "I guess not ...")
)
rvs2

## -----------------------------------------------------------------------------
c(rvs1, rvs2)

## -----------------------------------------------------------------------------
library(rtables)
placeholder_rd_afun <- function(df, .var, .spl_context, ref_path) {
  val <- tail(.spl_context$cur_col_split_val[[1]], 1)
  levs <- levels(df[[.var]])
  len <- length(levs)

  lst <- setNames(rep(val, len), levs)
  in_rows(.list = lst, .formats = setNames(rep("xx", len), levs))
}

comb_afun <- function(df, .var, .spl_context, ref_path) {
  if (grepl("difference", .spl_context$cur_col_id[[1]], ignore.case = TRUE)) {
    ret <- placeholder_rd_afun(df, .var, .spl_context, ref_path)
  } else {
    ret <- simple_analysis(df[[.var]])
  }
  ret
}

adsl <- ex_adsl
adae <- ex_adae

adsl$trt_span <- ifelse(adsl$ARM == "B: Placebo", " ", "Active Treatment")
adae$trt_span <- ifelse(adae$ARM == "B: Placebo", " ", "Active Treatment")
adsl$rr_header <- "Risk Differences"
adae$rr_header <- "Risk Differences"
adsl$rr_label <- paste(adsl$ARM, "vs B: Placebo")
adae$rr_label <- paste(adae$ARM, "vs B: Placebo")

trtmap <- data.frame(
  rr_header = c("Active Treatment", "Active Treatment", " "),
  ARM = c("A: Drug X", "C: Combination", "B: Placebo")
)

lyt <- basic_table() |>
  split_cols_by("trt_span", split_fun = trim_levels_in_group("ARM")) |>
  split_cols_by("ARM") |>
  split_cols_by("rr_header", nested = FALSE) |>
  split_cols_by("rr_label", split_fun = remove_split_levels("B: Placebo vs B: Placebo")) |>
  analyze("AEBODSYS", afun = comb_afun, extra_args = list(ref_path = c("ARM", "B: Placebo")))

build_table(lyt, adae, adsl)

## -----------------------------------------------------------------------------
afun_1 <- function(df, .var) {
  dat_vec <- df[[.var]]
  in_rows("Total Events" = sum(!is.na(dat_vec)))
}

afun_2 <- function(df, .var, .N_col, id) {
  non_na <- !is.na(df[[.var]])
  count <- length(unique(df[[id]]))
  in_rows("Unique Patients" = count * c(1, 1 / .N_col), .formats = c("Unique Patients" = "xx (xx.x%)"))
}

stacked_afun <- function(df, .var, .N_col, id) {
  events_rvs <- afun_1(df, .var)
  pats_rvs <- afun_2(df, .var, .N_col, id)
  c(events_rvs, pats_rvs)
}

## -----------------------------------------------------------------------------
lyt <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("AEBODSYS", split_fun = trim_levels_in_group("AEDECOD")) |>
  split_rows_by("AEDECOD") |>
  analyze("STUDYID", afun = stacked_afun, extra_args = list(id = "USUBJID"))

build_table(lyt, ex_adae, ex_adsl)

## -----------------------------------------------------------------------------
afun_count_lbl <- function(df, .var, lbl) {
  in_rows(sum(!is.na(df[[.var]])), .names = lbl)
}
basic_two_tier <- function(df, .var, .spl_context, detail_var, detail_level) {
  values <- lapply(
    levels(df[[.var]]),
    function(lvl) {
      dat <- df[df[[.var]] == lvl, ]
      rvs_out <- afun_count_lbl(dat, .var, lvl)
      if (lvl %in% detail_level) {
        det_rvs <- simple_analysis(dat[[detail_var]])
        indent_mod(det_rvs) <- 1
        rvs_out <- c(rvs_out, det_rvs)
      }
      rvs_out
    }
  )
  ret <- do.call(c, values)
  ret
}

## -----------------------------------------------------------------------------
lyt <- basic_table() |>
  split_cols_by("ARM") |>
  analyze("EOSSTT", afun = basic_two_tier, extra_args = list(detail_var = "DCSREAS", detail_level = "DISCONTINUED"))

build_table(lyt, ex_adsl)

## ----eval = FALSE-------------------------------------------------------------
# c.RowsVerticalSection <- function(...) {
#   lst <- list(...)
#   if (!all(vapply(lst, function(x) inherits(x, "RowsVerticalSection"), TRUE))) {
#     stop("Cannot use c() to combine RowsVerticalSection objects with objects of other classes")
#   }
# 
#   out <- NextMethod(generic = "c")
#   out <- RowsVerticalSection(
#     out,
#     names = comb_attr_w_dflt(lst, "row_names"),
#     labels = comb_attr_w_dflt(lst, "row_labels"),
#     indent_mods = comb_attr_w_dflt(lst, "indent_mods", 0L),
#     formats = comb_attr_w_dflt(lst, "row_formats", "xx"),
#     footnotes = comb_attr_w_dflt(lst, "row_footnotes"),
#     format_na_strs = comb_attr_w_dflt(lst, "row_na_strs", NA_character_)
#   )
#   out
# }
# 
# comb_attr_w_dflt <- function(lst, attrname, dflt = NULL) {
#   unlist(
#     lapply(lst, function(x) {
#       attr(x, attrname, exact = TRUE) %||% rep(dflt, length(x))
#     }),
#     recursive = FALSE,
#     use.names = FALSE
#   )
# }

