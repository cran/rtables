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

knitr::opts_chunk$set(comment = "")

## -----------------------------------------------------------------------------
double_count <- function(x) {
  in_rows(
    "Unique Patients" = length(unique(x)),
    "Total Events" = length(x)
  )
}

lyt_counts <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("AEBODSYS", split_fun = trim_levels_in_group("AEDECOD")) |>
  split_rows_by("AEDECOD") |>
  analyze("USUBJID", afun = double_count)

build_table(lyt_counts, ex_adae, alt_counts_df = ex_adsl)

## ----class.source="fold-hide"-------------------------------------------------
disp_denoms <- function(x, .var, .N_col, .df_row, .alt_df_full, .spl_context) {
  ## myfn <- RefFootnote("Patients observed in AE data", symbol = "*")
  parent_df <- .spl_context$full_parent_df[[3]]
  parent_df_onecol <- parent_df[eval(.spl_context$cur_col_expr[[1]], envir = parent_df), ]
  gparent_df <- .spl_context$full_parent_df[[2]]
  gparent_df_onecol <- gparent_df[eval(.spl_context$cur_col_expr[[1]], envir = gparent_df), ]
  cur_race <- .spl_context$value[2]
  cur_aebodsys <- .spl_context$value[3]
  cur_aedecod <- .spl_context$value[4]
  alt_df_race <- subset(.alt_df_full, RACE == cur_race)
  alt_df_race_col <- alt_df_race[eval(.spl_context$cur_col_expr[[1]], envir = alt_df_race), ]

  vals <- list(
    .N_col,
    length(x),
    length(unique(x)),
    NA,
    NROW(.df_row),
    length(unique(.df_row[[.var]])),
    NA,
    NROW(parent_df),
    length(unique(parent_df[[.var]])),
    NROW(parent_df_onecol),
    length(unique(parent_df_onecol[[.var]])),
    NROW(gparent_df),
    length(unique(gparent_df[[.var]])),
    NROW(alt_df_race),
    NROW(gparent_df_onecol),
    length(unique(gparent_df_onecol[[.var]])),
    NROW(alt_df_race_col)
  )
  names(vals) <- c(
    "Column N",
    "facet events",
    "facet patients",
    "facet patients - alt df",
    sprintf("%s events", cur_aedecod),
    sprintf("%s patients", cur_aedecod),
    sprintf("%s patients - alt df", cur_aedecod),
    sprintf("%s events (all arms)", cur_aebodsys),
    sprintf("%s patients (all arms)", cur_aebodsys),
    sprintf("%s events (this arm)", cur_aebodsys),
    sprintf("%s patients (this arm)", cur_aebodsys),
    sprintf("%s events (all arms)", cur_race),
    sprintf("%s patients (all arms)", cur_race),
    sprintf("%s patients (all arms) - alt df", cur_race),
    sprintf("%s events (this arms)", cur_race),
    sprintf("%s patients (this arms)", cur_race),
    sprintf("%s patients (this arms) - alt df", cur_race)
  )

  in_rows(.list = vals)
}

## -----------------------------------------------------------------------------
map <- tribble(
  ~RACE, ~AEBODSYS, ~AEDECOD,
  "ASIAN", "cl B.2", "dcd B.2.2.3.1",
  "WHITE", "cl A.1", "dcd A.1.1.1.1"
)

lyt_denoms <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("RACE", split_fun = trim_levels_to_map(map)) |> ## keep_split_levels(c("ASIAN", "WHITE"))) |>
  split_rows_by("AEBODSYS") |> # , split_fun = trim_levels_in_group("AEDECOD")) |>
  split_rows_by("AEDECOD") |>
  analyze("USUBJID", afun = disp_denoms)

build_table(lyt_denoms, ex_adae, alt_counts_df = ex_adsl)

## ----class.source = "fold-hide"-----------------------------------------------
avisit_afun <- function(df, .var, .spl_context) {
  cur_visit <- tail(.spl_context$value, 1)
  vals <- list("Mean Patient DIABP" = mean(df[[.var]]))
  pardf <- head(.spl_context$full_parent_df, 1)[[1]]

  if (!(as.character(cur_visit) %in% c("SCREENING", "BASELINE"))) {
    pardf <- subset(pardf, AVISIT %in% c("BASELINE", cur_visit))
    difs <- tapply(
      seq_len(nrow(pardf)), pardf$USUBJID,
      function(iis) {
        avalvec <- pardf$AVAL[iis]
        bl <- which(as.character(pardf[iis, ]$AVISIT) == "BASELINE")
        mean(avalvec[-bl] - avalvec[bl])
      }
    )
    vals <- c(vals, list("Mean Diff From Patient's Baseline DIABP" = mean(difs)))
  }

  in_rows(.list = vals)
}

## -----------------------------------------------------------------------------
lyt_rowcond <- basic_table() |>
  split_rows_by("AVISIT") |>
  analyze("AVAL", avisit_afun, format = "xx.xx")

build_table(lyt_rowcond, subset(ex_advs, PARAMCD == "DIABP"))

## -----------------------------------------------------------------------------
advs <- ex_advs
advs$span_label <- "Active Treatment"
advs$span_label[advs$ARM == "B: Placebo"] <- " "

span_label_map <- tribble(
  ~span_label, ~ARM,
  "Active Treatment", "A: Drug X",
  "Active Treatment", "C: Combination",
  " ", "B: Placebo",
)

advs$rr_header <- "Risk Differences"
advs$rr_label <- paste(substr(advs$ARM, 1, 1), "vs B")

## ----class.source = "fold-hide"-----------------------------------------------
in_risk_diff <- function(spl_context) grepl("Risk Differences", spl_context$cur_col_id[1])

avisit_afun2 <- function(df, .var, .spl_context) {
  in_rd <- in_risk_diff(.spl_context)

  cur_visit <- tail(.spl_context$value, 1)
  is_followup <- !(as.character(cur_visit) %in% c("SCREENING", "BASELINE"))
  if (!in_rd) {
    vals <- list("Mean Patient DIABP" = mean(df[[.var]]))
  } else if (!is_followup) {
    vals <- list("Mean Patient DIABP" = NULL)
  } else {
    vals <- list("Mean Patient DIABP" = rcell("-", format = "xx"))
  }
  pardf <- head(.spl_context$full_parent_df, 1)[[1]]

  if (is_followup) {
    if (!in_rd) {
      pardf <- subset(pardf, AVISIT %in% c("BASELINE", cur_visit))
      difs <- tapply(
        seq_len(nrow(pardf)), pardf$USUBJID,
        function(iis) {
          avalvec <- pardf$AVAL[iis]
          bl <- which(as.character(pardf[iis, ]$AVISIT) == "BASELINE")
          mean(avalvec[-bl] - avalvec[bl])
        }
      )
      vals <- c(vals, list("Mean Diff From Baseline" = mean(difs)))
    } else {
      armlabel <- tail(.spl_context$cur_col_split_val[[1]], 1) # last split value, ie arm
      armletter <- substr(armlabel, 1, 1)
      vals <- c(vals, list("Mean Diff From Baseline" = rcell(paste(armletter, "vs B"), format = "xx")))
    }
  }

  in_rows(.list = vals)
}

## -----------------------------------------------------------------------------
lyt_fullcond <- basic_table() |>
  split_cols_by("span_label", split_fun = trim_levels_to_map(span_label_map)) |>
  split_cols_by("ARM", show_colcounts = TRUE) |>
  split_cols_by("rr_header", nested = FALSE) |>
  split_cols_by("ARM", labels_var = "rr_label", split_fun = remove_split_levels("B: Placebo")) |>
  split_rows_by("AVISIT") |>
  analyze("AVAL", avisit_afun2, format = "xx.xx")


build_table(lyt_fullcond, subset(advs, PARAMCD == "DIABP"))

