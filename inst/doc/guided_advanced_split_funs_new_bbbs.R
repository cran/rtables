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

put_facets_first_last <- function(first = NULL, last = NULL) {
  if (is.null(first) && is.null(last)) {
    stop("must speficify at least one facet to be placed first or last")
  }
  function(ret, spl, fulldf) {
    fac_names <- names(ret$values)
    all_speced <- c(first, last)

    if (!all(all_speced %in% fac_names)) {
      stop(
        "Facet(s) []",
        paste(setdiff(all_speced, fac_names), collapse = ", "),
        "] not found in incoming split result."
      )
    }
    tmpfun <- restrict_facets(c(first, setdiff(fac_names, all_speced), last), op = "keep", reorder = TRUE)
    tmpfun(ret, spl, fulldf)
  }
}

## -----------------------------------------------------------------------------
fl_splfun <- make_split_fun(
  post = list(
    put_facets_first_last(first = "U", last = "UNDIFFERENTIATED")
  )
)

## -----------------------------------------------------------------------------
lyt_basic <- basic_table() |>
  split_cols_by("SEX")

build_table(lyt_basic, ex_adsl)

## -----------------------------------------------------------------------------
lyt_fl <- basic_table() |>
  split_cols_by("SEX", split_fun = fl_splfun)

build_table(lyt_fl, ex_adsl)

## -----------------------------------------------------------------------------
presort_facets <- function(ret, spl, fulldf) {
  fac_names <- names(ret$values)
  fac_ns <- vapply(ret$datasplit, NROW, 1L)
  ord <- order(fac_ns, decreasing = TRUE)
  tmpfun <- restrict_facets(fac_names[ord], op = "keep", reorder = TRUE)
  tmpfun(ret, spl, fulldf)
}

## -----------------------------------------------------------------------------
presort_splfun <- make_split_fun(post = list(presort_facets))

lyt_presort <- basic_table(show_colcounts = TRUE) |>
  split_cols_by("STRATA1", split_fun = presort_splfun)

build_table(lyt_presort, ex_adsl)

## -----------------------------------------------------------------------------
drop_sparse_facets <- function(ncutoff = 5) {
  function(ret, spl, fulldf) {
    fac_names <- names(ret$values)
    fac_ns <- vapply(ret$datasplit, NROW, 1L)
    keep_inds <- which(fac_ns >= ncutoff)
    tmpfun <- restrict_facets(fac_names[keep_inds], op = "keep", reorder = FALSE)
    tmpfun(ret, spl, fulldf)
  }
}

lyt_preprune1 <- basic_table(show_colcounts = TRUE) |>
  split_cols_by("SEX")

build_table(lyt_preprune1, ex_adsl)

preprune_splfun2 <- make_split_fun(post = list(drop_sparse_facets()))
lyt_preprune2 <- basic_table(show_colcounts = TRUE) |>
  split_cols_by("SEX", split_fun = preprune_splfun2)

build_table(lyt_preprune2, ex_adsl)

preprune_splfun3 <- make_split_fun(post = list(drop_sparse_facets(10)))
lyt_preprune3 <- basic_table(show_colcounts = TRUE) |>
  split_cols_by("SEX", split_fun = preprune_splfun3)

build_table(lyt_preprune3, ex_adsl)

## -----------------------------------------------------------------------------
trim_facets_to_map <- function(map = NULL) {
  function(df, spl, vals, labels, .spl_context) {
    if (is.null(map)) {
      return(df)
    } # do nothing
    cur_outer_val <- tail(.spl_context$value, 1)
    inner_var <- names(map)[2]
    inner_vec <- df[[inner_var]]
    inner_keep <- map[map[[1]] == cur_outer_val, inner_var, drop = TRUE]
    df_out <- df[inner_vec %in% inner_keep, ]
    df_out[[inner_var]] <- factor(df_out[[inner_var]], levels = intersect(levels(inner_vec), inner_keep))
    df_out
  }
}

## -----------------------------------------------------------------------------
map <- data.frame(
  ARM = c("A: Drug X", "B: Placebo"),
  STRATA1 = c("B", "A")
)

map_splfun <- make_split_fun(pre = list(trim_facets_to_map(map)))

outer_splfun <- make_split_fun(post = list(restrict_facets("C: Combination", op = "exclude")))

lyt <- basic_table() |>
  split_cols_by("ARM", split_fun = outer_splfun) |>
  split_cols_by("STRATA1", split_fun = map_splfun)

build_table(lyt, ex_adsl)

## -----------------------------------------------------------------------------
lyt <- basic_table() |>
  split_cols_by("ARM", split_fun = trim_levels_to_map(map)) |>
  split_cols_by("STRATA1")

build_table(lyt, ex_adsl)

