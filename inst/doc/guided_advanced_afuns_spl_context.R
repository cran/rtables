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
library(rtables)

## -----------------------------------------------------------------------------
in_risk_diff <- function(spl_context) {
  any(grepl("Risk Differences", spl_context$cur_col_split_value[1]))
}

## ----eval = FALSE-------------------------------------------------------------
# col_condition <- function(spl_context) {
#   ## return TRUE or FALSE
# }
# 
# col_cond_afun_template1 <- function(df, .var, ..., .spl_context) {
#   ## shared processing
# 
#   if (col_condition(.spl_context)) {
#     ## alternate behavior
# 
#     ## data processing
# 
#     ## value calculation
# 
#     ## determine cell formats, etc
#   } else {
#     ## primary behavior
# 
#     ## data processing
# 
#     ## value calculation
# 
#     ## determine cell formats, etc
#   }
# 
#   ## label calculation, etc if necessary
# 
#   in_rows(val_list, .labels = lbl_vector, .formats = format_vector)
# }

## ----eval = FALSE-------------------------------------------------------------
# col_cond_afun_template2 <- function(df, .var, ..., .spl_context) {
#   if (col_condition(.spl_context)) {
#     alt_behavior_afun(df, .var, ..., .spl_context = .spl_context)
#   } else {
#     main_behavior_afun(df, .var, ..., .spl_context = .spl_context)
#   }
# }

## -----------------------------------------------------------------------------
basic_get_ref <- function(ref_path, spl_context) {
  facet_dat <- spl_context$full_parent_df[[NROW(spl_context)]]

  ref_col_id <- paste(ref_path[seq(2, length(ref_path), by = 2)])

  ref_subset_vec <- spl_context[[ref_col_id]][[NROW(spl_context)]]

  ref_dat <- facet_dat[ref_subset_vec, ]

  list(ref_group = ref_dat, in_ref_col = ref_col_id == spl_context$cur_col_id[[1]])
}

## -----------------------------------------------------------------------------
diag_afun <- function(df, .spl_context, ref_path) {
  ref_info <- basic_get_ref(ref_path, .spl_context)

  in_rows(
    data_dim = dim(df),
    ref_dim = dim(ref_info$ref_group),
    in_ref_col = ref_info$in_ref_col,
    .formats = c(
      data_dim = "xx, xx",
      ref_dim = "xx, xx",
      in_ref_col = "xx"
    )
  )
}


lyt <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("STRATA1") |>
  split_rows_by("SEX", split_fun = keep_split_levels(c("F", "M"))) |>
  analyze("AGE", diag_afun, extra_args = list(ref_path = c("ARM", "B: Placebo")))


build_table(lyt, ex_adsl)

