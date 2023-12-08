## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(comment = "#")

## ---- message=FALSE-----------------------------------------------------------
library(rtables)
library(dplyr)

## -----------------------------------------------------------------------------
# In rcell we use align.
lyt <- basic_table() %>%
  analyze("AGE", function(x) {
    in_rows(
      left = rcell("l", align = "left"),
      right = rcell("r", align = "right"),
      center = rcell("c", align = "center")
    )
  })

tbl <- build_table(lyt, DM)
tbl

## -----------------------------------------------------------------------------
# In in_rows, we use .aligns. This can either set the general value or the
#   single values (see NB).
lyt2 <- basic_table() %>%
  analyze("AGE", function(x) {
    in_rows(
      left = rcell("l"),
      right = rcell("r"),
      center = rcell("c"),
      .aligns = c("right")
    ) # NB: .aligns = c("right", "left", "center")
  })

tbl2 <- build_table(lyt2, DM)
tbl2

## -----------------------------------------------------------------------------
lyt3 <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  analyze(c("AGE", "STRATA1"), function(x) {
    if (is.numeric(x)) {
      in_rows(
        "mean" = rcell(mean(x)),
        "sd" = rcell(sd(x)),
        .formats = c("xx.x"), .aligns = "left"
      )
    } else if (is.factor(x)) {
      rcell(length(unique(x)), align = "right")
    } else {
      stop("Unsupported type")
    }
  }, show_labels = "visible", na_str = "NE")

tbl3 <- build_table(lyt3, ex_adsl)
tbl3

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("STRATA1") %>%
  analyze("AGE") %>%
  append_topleft("New top_left material here")

build_table(lyt, DM)

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("STRATA1", label_pos = "topleft") %>%
  split_rows_by("SEX", label_pos = "topleft") %>%
  analyze("AGE")

build_table(lyt, DM)

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  analyze(c("AGE", "STRATA1"), function(x) {
    if (is.numeric(x)) {
      in_rows(
        "mean" = rcell(mean(x)),
        "sd" = rcell(sd(x)),
        .formats = c("xx.x"), .aligns = "left"
      )
    } else if (is.factor(x)) {
      rcell(length(unique(x)), align = "right")
    } else {
      stop("Unsupported type")
    }
  }, show_labels = "visible", na_str = "NE") %>%
  build_table(ex_adsl)

# Adding top-left material
top_left(lyt) <- "New top-left material here"

lyt

## -----------------------------------------------------------------------------
lyt <- basic_table(inset = 5) %>%
  analyze("AGE")

build_table(lyt, DM)

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  analyze("AGE")

tbl <- build_table(lyt, DM)
tbl

## -----------------------------------------------------------------------------
table_inset(tbl) <- 5
tbl

## -----------------------------------------------------------------------------
analysisfun <- function(x, ...) {
  in_rows(
    row1 = 5,
    row2 = c(1, 2),
    .row_footnotes = list(row1 = "row 1 rfn"),
    .cell_footnotes = list(row2 = "row 2 cfn")
  )
}

lyt <- basic_table(
  title = "Title says Whaaaat", subtitles = "Oh, ok.",
  main_footer = "ha HA! Footer!", prov_footer = "provenaaaaance"
) %>%
  split_cols_by("ARM") %>%
  analyze("AGE", afun = analysisfun)

result <- build_table(lyt, ex_adsl)
result

## -----------------------------------------------------------------------------
table_inset(result) <- 5
result

## -----------------------------------------------------------------------------
tbl <- basic_table() %>%
  split_cols_by("Species") %>%
  add_colcounts() %>%
  analyze(c("Sepal.Length", "Petal.Width"), function(x) {
    in_rows(
      mean_sd = c(mean(x), sd(x)),
      var = var(x),
      min_max = range(x),
      .formats = c("xx.xx (xx.xx)", "xx.xxx", "xx.x - xx.x"),
      .labels = c("Mean (sd)", "Variance", "Min - Max")
    )
  }) %>%
  build_table(iris, hsep = "=")
tbl

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("Species") %>%
  analyze(head(names(iris), -1), afun = function(x) {
    list(
      "mean / sd" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
      "range" = rcell(diff(range(x)), format = "xx.xx")
    )
  }, section_div = "+")

build_table(lyt, iris)

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("Species") %>%
  analyze(head(names(iris), -1), afun = function(x) {
    list(
      "mean / sd" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
      "range" = rcell(diff(range(x)), format = "xx.xx")
    )
  }, section_div = " ")

build_table(lyt, iris)

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("RACE", section_div = "=") %>%
  split_rows_by("STRATA1", section_div = "~") %>%
  analyze("AGE", mean, var_labels = "Age", format = "xx.xx")

build_table(lyt, DM)

## -----------------------------------------------------------------------------
basic_table(
  title = "Study XXXXXXXX",
  subtitles = c("subtitle YYYYYYYYYY", "subtitle2 ZZZZZZZZZ"),
  main_footer = "Analysis was done using cool methods that are correct",
  prov_footer = "file: /path/to/stuff/that/lives/there HASH:1ac41b242a"
) %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  split_rows_by("STRATA1") %>%
  analyze("AGE", mean, format = "xx.x") %>%
  build_table(DM)

## -----------------------------------------------------------------------------
basic_table(
  title = "Study XXXXXXXX",
  subtitles = c("subtitle YYYYYYYYYY", "subtitle2 ZZZZZZZZZ"),
  main_footer = "Analysis was done using cool methods that are correct",
  prov_footer = "file: /path/to/stuff/that/lives/there HASH:1ac41b242a"
) %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX", indent_mod = 3) %>%
  split_rows_by("STRATA1", indent_mod = 5) %>%
  analyze("AGE", mean, format = "xx.x") %>%
  build_table(DM)

## -----------------------------------------------------------------------------
lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  split_rows_by("SEX", split_fun = drop_split_levels, child_labels = "visible") %>%
  split_rows_by("STRATA1") %>%
  analyze("AGE", mean, show_labels = "default")

build_table(lyt, DM)

## -----------------------------------------------------------------------------
lyt2 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  split_rows_by("SEX", split_fun = drop_split_levels, child_labels = "hidden") %>%
  split_rows_by("STRATA1") %>%
  analyze("AGE", mean, show_labels = "visible")

build_table(lyt2, DM)

## -----------------------------------------------------------------------------
basic_table(
  title = "Study XXXXXXXX",
  subtitles = c("subtitle YYYYYYYYYY", "subtitle2 ZZZZZZZZZ"),
  main_footer = "Analysis was done using cool methods that are correct",
  prov_footer = "file: /path/to/stuff/that/lives/there HASH:1ac41b242a"
) %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX", split_fun = drop_split_levels, label_pos = "visible") %>%
  split_rows_by("STRATA1", label_pos = "hidden") %>%
  analyze("AGE", mean, format = "xx.x") %>%
  build_table(DM)

## -----------------------------------------------------------------------------
basic_table(
  title = "Study XXXXXXXX",
  subtitles = c("subtitle YYYYYYYYYY", "subtitle2 ZZZZZZZZZ"),
  main_footer = "Analysis was done using cool methods that are correct",
  prov_footer = "file: /path/to/stuff/that/lives/there HASH:1ac41b242a"
) %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX", split_fun = drop_split_levels, label_pos = "topleft") %>%
  split_rows_by("STRATA1", label_pos = "hidden") %>%
  analyze("AGE", mean, format = "xx.x") %>%
  build_table(DM)

## -----------------------------------------------------------------------------
trimmed_data <- ex_adsl %>%
  filter(SEX %in% c("M", "F")) %>%
  filter(RACE %in% levels(RACE)[1:2])

levels(trimmed_data$ARM)[1] <- "Incredibly long column name to be wrapped"
levels(trimmed_data$ARM)[2] <- "This_column_name_should_be_split_somewhere"

wide_tbl <- basic_table(
  title = "Title that is too long and also needs to be wrapped to a smaller width",
  subtitles = "Subtitle that is also long and also needs to be wrapped to a smaller width",
  main_footer = "Footnote that is wider than expected for this table.",
  prov_footer = "Provenance footer material that is also wider than expected for this table."
) %>%
  split_cols_by("ARM") %>%
  split_rows_by("RACE", split_fun = drop_split_levels) %>%
  analyze(
    c("AGE", "EOSDY"),
    na_str = "Very long cell contents to_be_wrapped_and_splitted",
    inclNAs = TRUE
  ) %>%
  build_table(trimmed_data)

wide_tbl

## -----------------------------------------------------------------------------
result_wrap_cells <- toString(wide_tbl, widths = c(10, 8, 8, 8))
matrix_wrap_cells <- matrix(strsplit(result_wrap_cells, "\n")[[1]], ncol = 1)
matrix_wrap_cells

## -----------------------------------------------------------------------------
result_wrap_cells_tf <- toString(
  wide_tbl,
  widths = c(10, 8, 8, 8),
  tf_wrap = TRUE,
  max_width = 43
)
matrix_wrap_cells_tf <- matrix(strsplit(result_wrap_cells_tf, "\n")[[1]], ncol = 1)
matrix_wrap_cells_tf

