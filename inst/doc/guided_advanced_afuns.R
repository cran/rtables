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

template_acfun <- function(x,
                           labelstr = NULL,
                           ## <optional special args>,
                           ## <additional args>,
                           ...) {
  if (is.null(labelstr)) {
    ## 'calculate' label(s) for afun-usage case
    lbl <- "cool label, bro"
  } else {
    ## calculate label(s) from labelstr for cfun-usage case
    lbl <- labelstr
  }

  ## whatever calculations we want
  out <- rcell(sample(c("what?", "huh?", "eh?"), 1), format = "xx")

  ## return our value(s) via in_rows
  in_rows(.list = list(ok = out), .labels = c(ok = lbl))
}

## -----------------------------------------------------------------------------
lyt <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("STRATA1", split_fun = keep_split_levels(c("A", "B"))) |>
  summarize_row_groups("STRATA1", cfun = template_acfun) |>
  split_rows_by("SEX", split_fun = keep_split_levels(c("F", "M"))) |>
  summarize_row_groups("SEX", cfun = template_acfun) |>
  analyze("AGE", afun = template_acfun)

build_table(lyt, ex_adsl)

