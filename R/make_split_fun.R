#' Variable associated with a split
#'
#' This function is intended for use when writing custom splitting logic. In cases where the split is associated with
#' a single variable, the name of that variable will be returned. At time of writing this includes splits generated
#' via the [split_rows_by()], [split_cols_by()], [split_rows_by_cuts()], [split_cols_by_cuts()],
#' [split_rows_by_cutfun()], and [split_cols_by_cutfun()] layout directives.
#'
#' @param spl (`VarLevelSplit`)\cr the split object.
#'
#' @return For splits with a single variable associated with them, returns the split. Otherwise, an error is raised.
#'
#' @export
#' @seealso \code{\link{make_split_fun}}
setGeneric("spl_variable", function(spl) standardGeneric("spl_variable"))

#' @rdname spl_variable
#' @export
setMethod("spl_variable", "VarLevelSplit", function(spl) spl_payload(spl))

#' @rdname spl_variable
#' @export
setMethod("spl_variable", "VarDynCutSplit", function(spl) spl_payload(spl))

#' @rdname spl_variable
#' @export
setMethod("spl_variable", "VarStaticCutSplit", function(spl) spl_payload(spl))

#' @rdname spl_variable
#' @export
setMethod(
  "spl_variable", "Split",
  function(spl) stop("Split class ", class(spl), " not associated with a single variable.")
)

in_col_split <- function(spl_ctx) {
  identical(
    names(spl_ctx),
    names(context_df_row(cinfo = NULL))
  )
}

assert_splres_element <- function(pinfo, nm, len = NULL, component = NULL) {
  msg_2_append <- ""
  if (!is.null(component)) {
    msg_2_append <- paste0(
      "Invalid split function constructed by upstream call to ",
      "make_split_fun. Problem source: ",
      component, " argument."
    )
  }
  if (!(nm %in% names(pinfo))) {
    stop(
      "Split result does not have required element: ", nm, ".",
      msg_2_append
    )
  }
  if (!is.null(len) && length(pinfo[[nm]]) != len) {
    stop(
      "Split result element ", nm, " does not have required length ", len, ".",
      msg_2_append
    )
  }
  TRUE
}

validate_split_result <- function(pinfo, component = NULL) {
  assert_splres_element(pinfo, "datasplit", component = component)
  len <- length(pinfo$datasplit)
  assert_splres_element(pinfo, "values", len, component = component)
  assert_splres_element(pinfo, "labels", len, component = component)
  TRUE
}

#' Construct split result object
#'
#' These functions can be used to create or add to a split result in functions which implement core splitting or
#' post-processing within a custom split function.
#'
#' @param values (`character` or `list(SplitValue)`)\cr the values associated with each facet.
#' @param datasplit (`list(data.frame)`)\cr the facet data for each facet generated in the split.
#' @param labels (`character`)\cr the labels associated with each facet.
#' @param extras (`list` or `NULL`)\cr extra values associated with each of the facets which will be passed to
#'   analysis functions applied within the facet.
#' @param subset_exprs (`list`)\cr A list of subsetting expressions (e.g.,
#'     created with `quote()`) to be used during column subsetting.
#'
#' @return A named list representing the facets generated by the split with elements `values`, `datasplit`, and
#'   `labels`, which are the same length and correspond to each other element-wise.
#'
#' @details
#' These functions performs various housekeeping tasks to ensure that the split result list is as the rtables
#' internals expect it, most of which are not relevant to end users.
#'
#' @examples
#' splres <- make_split_result(
#'   values = c("hi", "lo"),
#'   datasplit = list(hi = mtcars, lo = mtcars[1:10, ]),
#'   labels = c("more data", "less data"),
#'   subset_exprs = list(expression(TRUE), expression(seq_along(wt) <= 10))
#' )
#'
#' splres2 <- add_to_split_result(splres,
#'   values = "med",
#'   datasplit = list(med = mtcars[1:20, ]),
#'   labels = "kinda some data",
#'   subset_exprs = quote(seq_along(wt) <= 20)
#' )
#'
#' @family make_custom_split
#' @rdname make_split_result
#' @export
#' @family make_custom_split
make_split_result <- function(values, datasplit, labels, extras = NULL, subset_exprs = vector("list", length(values))) {
  if (length(values) == 1 && is(datasplit, "data.frame")) {
    datasplit <- list(datasplit)
  }
  ret <- list(values = values, datasplit = datasplit, labels = labels, subset_exprs = subset_exprs)
  if (!is.null(extras)) {
    ret$extras <- extras
  }
  .fixupvals(ret)
}

#' @param splres (`list`)\cr a list representing the result of splitting.
#'
#' @rdname make_split_result
#' @export
add_to_split_result <- function(splres, values, datasplit, labels, extras = NULL, subset_exprs = NULL) {
  validate_split_result(splres)
  newstuff <- make_split_result(values, datasplit, labels, extras, subset_exprs = list(subset_exprs))
  ret <- lapply(
    names(splres),
    function(nm) c(splres[[nm]], newstuff[[nm]])
  )
  names(ret) <- names(splres)
  .fixupvals(ret)
}


.can_take_spl_context <- function(f) any(c(".spl_context", "...") %in% names(formals(f)))

#' Create a custom splitting function
#'
#' @param pre (`list`)\cr zero or more functions which operate on the incoming data and return a new data frame that
#'   should split via `core_split`. They will be called on the data in the order they appear in the list.
#' @param core_split (`function` or `NULL`)\cr if non-`NULL`, a function which accepts the same arguments that
#'   `do_base_split` does, and returns the same type of named list. Custom functions which override this behavior
#'   cannot be used in column splits.
#' @param post (`list`)\cr zero or more functions which should be called on the list output by splitting.
#'
#' @details
#' Custom split functions can be thought of as (up to) 3 different types of manipulations of the splitting process:
#'
#' 1. Pre-processing of the incoming data to be split.
#' 2. (Row-splitting only) Customization of the core mapping of incoming data to facets.
#' 3. Post-processing operations on the set of facets (groups) generated by the split.
#'
#' This function provides an interface to create custom split functions by implementing and specifying sets of
#' operations in each of those classes of customization independently.
#'
#' Pre-processing functions (1), must accept: `df`, `spl`, `vals`, and `labels`, and can optionally accept
#' `.spl_context`. They then manipulate `df` (the incoming data for the split) and return a modified data frame.
#' This modified data frame *must* contain all columns present in the incoming data frame, but can add columns if
#' necessary (though we note that these  new columns cannot be used in the layout as split or analysis variables,
#' because they will not be present when validity checking is done).
#'
#' The preprocessing component is useful for things such as manipulating factor levels, e.g., to trim unobserved ones
#' or to reorder levels based on observed counts, etc.
#'
#' Core  splitting functions  override  the  fundamental
#' splitting procedure,  and are only  necessary in rare  cases. These
#' must  accept  `spl`, `df`,  `vals`,  `labels`,  and can  optionally
#' accept `.spl_context`. They should return a split result object
#' constructed via `make_split_result()`.
#'
#' In particular, if the custom split function will be used in
#' column space, subsetting expressions (e.g., as returned by
#' `quote()` or `bquote` must be provided, while they are
#' optional (and largely ignored, currently) in row space.
#'
#'
#' Post-processing functions (3) must accept the result of the core split as their first argument (which can be
#' anything), in addition to `spl`, and `fulldf`, and can optionally accept `.spl_context`. They must each return a
#' modified version of the same structure specified above for core splitting.
#'
#' In both the pre- and post-processing cases, multiple functions can be specified. When this happens, they are applied
#' sequentially, in the order they appear in the list passed to the relevant argument (`pre` and `post`, respectively).
#'
#' @return A custom function that can be used as a split function.
#'
#' @seealso [custom_split_funs] for a more detailed discussion on what custom split functions do.
#'
#' @examples
#' mysplitfun <- make_split_fun(
#'   pre = list(drop_facet_levels),
#'   post = list(add_overall_facet("ALL", "All Arms"))
#' )
#'
#' basic_table(show_colcounts = TRUE) %>%
#'   split_cols_by("ARM", split_fun = mysplitfun) %>%
#'   analyze("AGE") %>%
#'   build_table(subset(DM, ARM %in% c("B: Placebo", "C: Combination")))
#'
#' ## post (and pre) arguments can take multiple functions, here
#' ## we add an overall facet and the reorder the facets
#' reorder_facets <- function(splret, spl, fulldf, ...) {
#'   ord <- order(names(splret$values))
#'   make_split_result(
#'     splret$values[ord],
#'     splret$datasplit[ord],
#'     splret$labels[ord]
#'   )
#' }
#'
#' mysplitfun2 <- make_split_fun(
#'   pre = list(drop_facet_levels),
#'   post = list(
#'     add_overall_facet("ALL", "All Arms"),
#'     reorder_facets
#'   )
#' )
#' basic_table(show_colcounts = TRUE) %>%
#'   split_cols_by("ARM", split_fun = mysplitfun2) %>%
#'   analyze("AGE") %>%
#'   build_table(subset(DM, ARM %in% c("B: Placebo", "C: Combination")))
#'
#' very_stupid_core <- function(spl, df, vals, labels, .spl_context) {
#'   make_split_result(c("stupid", "silly"),
#'     datasplit = list(df[1:10, ], df[11:30, ]),
#'     labels = c("first 10", "second 20")
#'   )
#' }
#'
#' dumb_30_facet <- add_combo_facet("dumb",
#'   label = "thirty patients",
#'   levels = c("stupid", "silly")
#' )
#' nonsense_splfun <- make_split_fun(
#'   core_split = very_stupid_core,
#'   post = list(dumb_30_facet)
#' )
#'
#' ## recall core split overriding is not supported in column space
#' ## currently, but we can see it in action in row space
#'
#' lyt_silly <- basic_table() %>%
#'   split_rows_by("ARM", split_fun = nonsense_splfun) %>%
#'   summarize_row_groups() %>%
#'   analyze("AGE")
#' silly_table <- build_table(lyt_silly, DM)
#' silly_table
#'
#' @family make_custom_split
#' @export
make_split_fun <- function(pre = list(), core_split = NULL, post = list()) {
  function(df,
           spl,
           vals = NULL,
           labels = NULL,
           trim = FALSE,
           .spl_context) {
    orig_columns <- names(df)
    for (pre_fn in pre) {
      if (.can_take_spl_context(pre_fn)) {
        df <- pre_fn(df = df, spl = spl, vals = vals, labels = labels, .spl_context = .spl_context)
      } else {
        df <- pre_fn(df = df, spl = spl, vals = vals, labels = labels)
      }
      if (!is(df, "data.frame")) {
        stop(
          "Error in custom split function, pre-split step did not return a data.frame. ",
          "See upstream call to make_split_fun for original source of error."
        )
      }
    }

    if (!all(orig_columns %in% names(df))) {
      stop(
        "Preprocessing functions(s) in custom split function removed a column from the incoming data.",
        " This is not supported. See upstread make_split_fun call (pre argument) for original source of error."
      )
    }

    if (is.null(core_split)) {
      ret <- do_base_split(spl = spl, df = df, vals = vals, labels = labels)
    } else {
      ret <- core_split(spl = spl, df = df, vals = vals, labels = labels, .spl_context)
      validate_split_result(ret, component = "core_split")
    }

    for (post_fn in post) {
      if (.can_take_spl_context(post_fn)) {
        ret <- post_fn(ret, spl = spl, .spl_context = .spl_context, fulldf = df)
      } else {
        ret <- post_fn(ret, spl = spl, fulldf = df)
      }
    }
    validate_split_result(ret, "post")
    ret
  }
}

#' Add a combination facet in post-processing
#'
#' Add a combination facet during the post-processing stage in a custom split fun.
#'
#' @param name (`string`)\cr name for the resulting facet (for use in pathing, etc.).
#' @param label (`string`)\cr label for the resulting facet.
#' @param levels (`character`)\cr vector of levels to combine within the resulting facet.
#' @param extra (`list`)\cr extra arguments to be passed to analysis functions applied within the resulting facet.
#'
#' @details
#' For `add_combo_facet`, the data associated with the resulting facet will be the data associated with the facets for
#' each level in `levels`, row-bound together. In particular, this means that if those levels are overlapping, data
#' that appears in both will be duplicated.
#'
#' @return A function which can be used within the `post` argument in [make_split_fun()].
#'
#' @seealso [make_split_fun()]
#'
#' @examples
#' mysplfun <- make_split_fun(post = list(
#'   add_combo_facet("A_B",
#'     label = "Arms A+B",
#'     levels = c("A: Drug X", "B: Placebo")
#'   ),
#'   add_overall_facet("ALL", label = "All Arms")
#' ))
#'
#' lyt <- basic_table(show_colcounts = TRUE) %>%
#'   split_cols_by("ARM", split_fun = mysplfun) %>%
#'   analyze("AGE")
#'
#' tbl <- build_table(lyt, DM)
#'
#' @family make_custom_split
#' @export
add_combo_facet <- function(name, label = name, levels, extra = list()) {
  function(ret, spl, .spl_context, fulldf) {
    if (is(levels, "AllLevelsSentinel")) {
      subexpr <- expression(TRUE)
      datpart <- list(fulldf)
    } else {
      subexpr <- .combine_value_exprs(ret$values[levels])
      datpart <- list(do.call(rbind, ret$datasplit[levels]))
    }


    val <- LevelComboSplitValue(
      val = name, extr = extra, combolevels = levels, label = label,
      sub_expr = subexpr
    )
    add_to_split_result(ret,
      values = list(val), labels = label,
      datasplit = datpart
    )
  }
}

.combine_value_exprs <- function(val_lst, spl) {
  exprs <- lapply(val_lst, value_expr)
  nulls <- vapply(exprs, is.null, TRUE)
  if (all(nulls)) {
    return(NULL) # default behavior all the way down the line, no need to do anything.
  } else if (any(nulls)) {
    exprs[nulls] <- lapply(val_lst[nulls], function(vali) make_subset_expr(spl, vali))
  }
  Reduce(.or_combine_exprs, exprs)
}

## no NULLS coming in here, everything has been populated
## by either custom subsetting expressions or the result of make_subset_expr(spl, val)
.or_combine_exprs <- function(ex1, ex2) {
  if (identical(ex1, expression(FALSE))) {
    return(ex2)
  } else if (identical(ex2, expression(FALSE))) {
    return(ex1)
  } else if (identical(ex1, expression(TRUE)) || identical(ex2, expression(TRUE))) {
    return(TRUE)
  }
  as.expression(bquote((.(a)) | .(b), list(a = ex1[[1]], b = ex2[[1]])))
}

#' @rdname add_combo_facet
#' @export
add_overall_facet <- function(name, label, extra = list()) {
  add_combo_facet(
    name = name, label = label, levels = select_all_levels,
    extra = extra
  )
}

#' Trim levels of another variable from each facet (post-processing split step)
#'
#' @param innervar (`character`)\cr the variable(s) to trim (remove unobserved levels) independently within each facet.
#'
#' @return A function suitable for use in the `pre` (list) argument of `make_split_fun`.
#'
#' @seealso [make_split_fun()]
#'
#' @family make_custom_split
#' @export
trim_levels_in_facets <- function(innervar) {
  function(ret, ...) {
    for (var in innervar) {
      ret$datasplit <- lapply(ret$datasplit, function(df) {
        df[[var]] <- factor(df[[var]])
        df
      })
    }
    ret
  }
}

#' Pre-processing function for use in `make_split_fun`
#'
#' This function is intended for use as a pre-processing component in `make_split_fun`, and should not be called
#' directly by end users.
#'
#' @param df (`data.frame`)\cr the incoming data corresponding with the parent facet.
#' @param spl (`VarLevelSplit`)\cr the split.
#' @param ... additional parameters passed internally.
#'
#' @seealso [make_split_fun()]
#'
#' @family make_custom_split
#' @export
drop_facet_levels <- function(df, spl, ...) {
  if (!is(spl, "VarLevelSplit") || is.na(spl_payload(spl))) {
    stop("Unable to determine faceting variable in drop_facet_levels application.")
  }
  var <- spl_payload(spl)
  df[[var]] <- factor(df[[var]])
  df
}
