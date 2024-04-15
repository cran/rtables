## ----message=FALSE------------------------------------------------------------
library(rtables)

## -----------------------------------------------------------------------------
d1 <- subset(ex_adsl, AGE < 25)
d1$AGE <- as.factor(d1$AGE)
lyt1 <- basic_table() %>%
  split_cols_by("AGE") %>%
  analyze("SEX")

build_table(lyt1, d1)

## -----------------------------------------------------------------------------
sd_cutfun <- function(x) {
  cutpoints <- c(
    min(x),
    mean(x) - sd(x),
    mean(x) + sd(x),
    max(x)
  )

  names(cutpoints) <- c("", "Low", "Medium", "High")
  cutpoints
}

lyt1 <- basic_table() %>%
  split_cols_by_cutfun("AGE", cutfun = sd_cutfun) %>%
  analyze("SEX")

build_table(lyt1, ex_adsl)

## -----------------------------------------------------------------------------
lyt1 <- basic_table() %>%
  split_cols_by_cuts(
    "AGE",
    cuts = c(0, 30, 60, 100),
    cutlabels = c("0-30 y.o.", "30-60 y.o.", "60-100 y.o.")
  ) %>%
  analyze("SEX")

build_table(lyt1, ex_adsl)

## -----------------------------------------------------------------------------
picky_splitter <- function(var) {
  function(df, spl, vals, labels, trim) {
    orig_vals <- vals
    if (is.null(vals)) {
      vec <- df[[var]]
      vals <- if (is.factor(vec)) levels(vec) else unique(vec)
    }
    if (length(vals) == 1) {
      do_base_split(spl = spl, df = df, vals = vals, labels = labels, trim = trim)
    } else {
      add_overall_level(
        "Overall",
        label = "All Obs", first = FALSE
      )(df = df, spl = spl, vals = orig_vals, trim = trim)
    }
  }
}


d1 <- subset(ex_adsl, ARM == "A: Drug X")
d1$ARM <- factor(d1$ARM)

lyt1 <- basic_table() %>%
  split_cols_by("ARM", split_fun = picky_splitter("ARM")) %>%
  analyze("AGE")

## -----------------------------------------------------------------------------
build_table(lyt1, d1)

## -----------------------------------------------------------------------------
build_table(lyt1, ex_adsl)

## -----------------------------------------------------------------------------
dta_test <- data.frame(
  USUBJID = rep(1:6, each = 3),
  PARAMCD = rep("lab", 6 * 3),
  AVISIT = rep(paste0("V", 1:3), 6),
  ARM = rep(LETTERS[1:3], rep(6, 3)),
  AVAL = c(9:1, rep(NA, 9)),
  CHG = c(1:9, rep(NA, 9))
)

my_afun <- function(x, .spl_context) {
  n <- sum(!is.na(x))
  meanval <- mean(x, na.rm = TRUE)
  sdval <- sd(x, na.rm = TRUE)

  ## get the split value of the most recent parent
  ## (row) split above this analyze
  val <- .spl_context[nrow(.spl_context), "value"]
  ## do a silly thing to decide the different format precisiosn
  ## your real logic would go here
  valnum <- min(2L, as.integer(gsub("[^[:digit:]]*", "", val)))
  fstringpt <- paste0("xx.", strrep("x", valnum))
  fmt_mnsd <- sprintf("%s (%s)", fstringpt, fstringpt)
  in_rows(
    n = n,
    "Mean, SD" = c(meanval, sdval),
    .formats = c(n = "xx", "Mean, SD" = fmt_mnsd)
  )
}

lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("AVISIT") %>%
  split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
  analyze_colvars(my_afun)

build_table(lyt, dta_test)

## -----------------------------------------------------------------------------
my_afun <- function(x, .var, .spl_context) {
  n <- sum(!is.na(x))
  meanval <- mean(x, na.rm = TRUE)
  sdval <- sd(x, na.rm = TRUE)

  ## get the split value of the most recent parent
  ## (row) split above this analyze
  val <- .spl_context[nrow(.spl_context), "value"]
  ## we show it if its not a CHG within V1
  show_it <- val != "V1" || .var != "CHG"
  ## do a silly thing to decide the different format precisiosn
  ## your real logic would go here
  valnum <- min(2L, as.integer(gsub("[^[:digit:]]*", "", val)))
  fstringpt <- paste0("xx.", strrep("x", valnum))
  fmt_mnsd <- if (show_it) sprintf("%s (%s)", fstringpt, fstringpt) else "xx"
  in_rows(
    n = if (show_it) n, ## NULL otherwise
    "Mean, SD" = if (show_it) c(meanval, sdval), ## NULL otherwise
    .formats = c(n = "xx", "Mean, SD" = fmt_mnsd)
  )
}

lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("AVISIT") %>%
  split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
  analyze_colvars(my_afun)

build_table(lyt, dta_test)

## -----------------------------------------------------------------------------
my_afun <- function(x, .var, ref_rowgroup, .spl_context) {
  n <- sum(!is.na(x))
  meanval <- mean(x, na.rm = TRUE)
  sdval <- sd(x, na.rm = TRUE)

  ## get the split value of the most recent parent
  ## (row) split above this analyze
  val <- .spl_context[nrow(.spl_context), "value"]
  ## we show it if its not a CHG within V1
  show_it <- val != ref_rowgroup || .var != "CHG"
  fmt_mnsd <- if (show_it) "xx.x (xx.x)" else "xx"
  in_rows(
    n = if (show_it) n, ## NULL otherwise
    "Mean, SD" = if (show_it) c(meanval, sdval), ## NULL otherwise
    .formats = c(n = "xx", "Mean, SD" = fmt_mnsd)
  )
}

lyt2 <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("AVISIT") %>%
  split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
  analyze_colvars(my_afun, extra_args = list(ref_rowgroup = "V1"))

build_table(lyt2, dta_test)

