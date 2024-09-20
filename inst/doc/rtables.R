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

## ----message=FALSE------------------------------------------------------------
library(rtables)
library(dplyr)

## ----echo=FALSE, fig.align='center'-------------------------------------------
knitr::include_graphics("./images/rtables-basics.png")

## ----data---------------------------------------------------------------------
n <- 400

set.seed(1)

df <- tibble(
  arm = factor(sample(c("Arm A", "Arm B"), n, replace = TRUE), levels = c("Arm A", "Arm B")),
  country = factor(sample(c("CAN", "USA"), n, replace = TRUE, prob = c(.55, .45)), levels = c("CAN", "USA")),
  gender = factor(sample(c("Female", "Male"), n, replace = TRUE), levels = c("Female", "Male")),
  handed = factor(sample(c("Left", "Right"), n, prob = c(.6, .4), replace = TRUE), levels = c("Left", "Right")),
  age = rchisq(n, 30) + 10
) %>% mutate(
  weight = 35 * rnorm(n, sd = .5) + ifelse(gender == "Female", 140, 180)
)

head(df)

## ----echo=FALSE---------------------------------------------------------------
lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  split_rows_by("country") %>%
  summarize_row_groups() %>%
  split_rows_by("handed") %>%
  summarize_row_groups() %>%
  analyze("age", afun = mean, format = "xx.xx")

tbl <- build_table(lyt, df)
tbl

## -----------------------------------------------------------------------------
qtable(df,
  row_vars = c("country", "handed"),
  col_vars = c("arm", "gender"),
  avar = "age",
  afun = mean,
  summarize_groups = TRUE,
  row_labels = "mean"
)

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  analyze("age", mean, format = "xx.x")

tbl <- build_table(lyt, df)
tbl

## -----------------------------------------------------------------------------
lyt

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("arm") %>%
  analyze("age", afun = mean, format = "xx.x")

tbl <- build_table(lyt, df)
tbl

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  analyze("age", afun = mean, format = "xx.x")

tbl <- build_table(lyt, df)
tbl

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  split_rows_by("country") %>%
  analyze("age", afun = mean, format = "xx.x")

tbl <- build_table(lyt, df)
tbl

## -----------------------------------------------------------------------------
mean(df$age[df$country == "CAN" & df$arm == "Arm A" & df$gender == "Female"])

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  split_rows_by("country", page_by = TRUE) %>%
  split_rows_by("handed") %>%
  analyze("age", afun = mean, format = "xx.x")

tbl <- build_table(lyt, df)
cat(export_as_txt(tbl, page_type = "letter", page_break = "\n\n~~~~~~ Page Break ~~~~~~\n\n"))

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  split_rows_by("country") %>%
  summarize_row_groups() %>%
  analyze("age", afun = mean, format = "xx.x")

tbl <- build_table(lyt, df)
tbl

## -----------------------------------------------------------------------------
df_cell <- subset(df, df$country == "CAN" & df$arm == "Arm A" & df$gender == "Female")
df_col_1 <- subset(df, df$arm == "Arm A" & df$gender == "Female")

c(count = nrow(df_cell), percentage = nrow(df_cell) / nrow(df_col_1))

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  split_rows_by("country") %>%
  summarize_row_groups() %>%
  split_rows_by("handed") %>%
  analyze("age", afun = mean, format = "xx.x")

tbl <- build_table(lyt, df)
tbl

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  split_rows_by("country") %>%
  summarize_row_groups() %>%
  split_rows_by("handed") %>%
  summarize_row_groups() %>%
  analyze("age", afun = mean, format = "xx.x")

tbl <- build_table(lyt, df)
tbl

