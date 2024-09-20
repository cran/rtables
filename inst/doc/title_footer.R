## ----include = FALSE----------------------------------------------------------
suggested_dependent_pkgs <- c("dplyr", "tibble")
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

## ----echo=FALSE, output="hide"------------------------------------------------
knitr::opts_chunk$set(comment = "#")
suppressPackageStartupMessages(library(rtables))
suppressPackageStartupMessages(library(dplyr))

## -----------------------------------------------------------------------------
library(rtables)
library(dplyr)
lyt <- basic_table(
  title = "Study XXXXXXXX",
  subtitles = c("subtitle YYYYYYYYYY", "subtitle2 ZZZZZZZZZ"),
  main_footer = "Analysis was done using cool methods that are correct",
  prov_footer = "file: /path/to/stuff/that/lives/there HASH:1ac41b242a"
) %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX", split_fun = drop_split_levels) %>%
  split_rows_by("STRATA1") %>%
  analyze("AGE", mean, format = "xx.x")

tbl <- build_table(lyt, DM)
cat(export_as_txt(tbl, paginate = TRUE, page_break = "\n\n\n"))

## -----------------------------------------------------------------------------
lyt2 <- basic_table(
  title = "Study XXXXXXXX",
  subtitles = c("subtitle YYYYYYYYYY", "subtitle2 ZZZZZZZZZ"),
  main_footer = "Analysis was done using cool methods that are correct",
  prov_footer = "file: /path/to/stuff/that/lives/there HASH:1ac41b242a"
) %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX", page_by = TRUE, page_prefix = "Patient Subset - Gender", split_fun = drop_split_levels) %>%
  split_rows_by("STRATA1") %>%
  analyze("AGE", mean, format = "xx.x")

tbl2 <- build_table(lyt2, DM)
cat(export_as_txt(tbl2, paginate = TRUE, page_break = "\n\n~~~~ Page Break ~~~~\n\n"))

## -----------------------------------------------------------------------------
lyt3 <- basic_table(
  title = "Study XXXXXXXX",
  subtitles = c("subtitle YYYYYYYYYY", "subtitle2 ZZZZZZZZZ"),
  main_footer = "Analysis was done using cool methods that are correct",
  prov_footer = "file: /path/to/stuff/that/lives/there HASH:1ac41b242a"
) %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX", page_by = TRUE, page_prefix = "Patient Subset - Gender", split_fun = drop_split_levels) %>%
  split_rows_by("STRATA1", page_by = TRUE, page_prefix = "Stratification - Strata") %>%
  analyze("AGE", mean, format = "xx.x")

tbl3 <- build_table(lyt3, DM)
cat(export_as_txt(tbl3, paginate = TRUE, page_break = "\n\n~~~~ Page Break ~~~~\n\n"))

## -----------------------------------------------------------------------------
afun <- function(df, .var, .spl_context) {
  val <- .spl_context$value[NROW(.spl_context)]
  rw_fnotes <- if (val == "C") list("This is strata level C for these patients") else list()
  cl_fnotes <- if (val == "B" && df[1, "ARM", drop = TRUE] == "C: Combination") {
    list("these Strata B patients got the drug combination")
  } else {
    list()
  }

  in_rows(
    mean = mean(df[[.var]]),
    .row_footnotes = rw_fnotes,
    .cell_footnotes = cl_fnotes,
    .formats = c(mean = "xx.x")
  )
}

lyt <- basic_table(
  title = "Study XXXXXXXX",
  subtitles = c("subtitle YYYYYYYYYY", "subtitle2 ZZZZZZZZZ"),
  main_footer = "Analysis was done using cool methods that are correct",
  prov_footer = "file: /path/to/stuff/that/lives/there HASH:1ac41b242a"
) %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX", page_by = TRUE, page_prefix = "Patient Subset - Gender", split_fun = drop_split_levels) %>%
  split_rows_by("STRATA1") %>%
  analyze("AGE", afun, format = "xx.x")

tbl <- build_table(lyt, DM)
cat(export_as_txt(tbl, paginate = TRUE, page_break = "\n\n\n"))

## -----------------------------------------------------------------------------
## from ?tolower example slightly modified
.simpleCap <- function(x) {
  if (length(x) > 1) {
    return(sapply(x, .simpleCap))
  }
  s <- strsplit(tolower(x), " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
}

adsl2 <- ex_adsl %>%
  filter(SEX %in% c("M", "F") & RACE %in% (levels(RACE)[1:3])) %>%
  ## we trim the level names here solely due to space considerations
  mutate(ethnicity = .simpleCap(gsub("(.*)OR.*", "\\1", RACE)), RACE = factor(RACE))

lyt2 <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_cols_by("SEX", split_fun = drop_split_levels) %>%
  split_rows_by("RACE", labels_var = "ethnicity", split_fun = drop_split_levels) %>%
  summarize_row_groups() %>%
  analyze(c("AGE", "STRATA1"))

tbl2 <- build_table(lyt2, adsl2)
tbl2

## -----------------------------------------------------------------------------
fnotes_at_path(tbl2, c("RACE", "ASIAN")) <- c("hi", "there")
tbl2

## -----------------------------------------------------------------------------
fnotes_at_path(tbl2, rowpath = NULL, c("ARM", "B: Placebo")) <- c("this is a placebo")
tbl2

## -----------------------------------------------------------------------------
row_paths_summary(tbl2)

## -----------------------------------------------------------------------------
fnotes_at_path(
  tbl2,
  rowpath = c("RACE", "ASIAN", "@content", "Asian"),
  colpath = c("ARM", "B: Placebo", "SEX", "F")
) <- "These asian women got placebo treatments"
tbl2

