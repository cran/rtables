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

## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(comment = "#")

## ----message=FALSE------------------------------------------------------------
library(rtables)
library(dplyr)

## -----------------------------------------------------------------------------
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

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  split_rows_by("country") %>%
  summarize_row_groups() %>%
  split_rows_by("handed") %>%
  summarize_row_groups() %>%
  analyze("age", afun = mean, format = "xx.x")

tbl <- build_table(lyt, df)
tbl

## -----------------------------------------------------------------------------
mean(df$age[df$country == "CAN" & df$arm == "Arm A" & df$gender == "Female" & df$handed == "Left"])

## -----------------------------------------------------------------------------
df %>%
  filter(country == "CAN", arm == "Arm A", gender == "Female", handed == "Left") %>%
  summarise(mean_age = mean(age))

## -----------------------------------------------------------------------------
df %>%
  group_by(arm, gender) %>%
  filter(country == "CAN", handed == "Left") %>%
  summarise(mean_age = mean(age))

## -----------------------------------------------------------------------------
average_age <- df %>%
  group_by(arm, gender, country, handed) %>%
  summarise(mean_age = mean(age))

average_age

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  split_rows_by("country") %>%
  split_rows_by("handed") %>%
  analyze("age", afun = mean, format = "xx.x")

tbl <- build_table(lyt, df)
tbl

## -----------------------------------------------------------------------------
c_h_df <- df %>%
  group_by(arm, gender, country, handed) %>%
  summarize(mean = mean(age), c_h_count = n()) %>%
  ## we need the sum below to *not* be by country, so that we're dividing by the column counts
  ungroup(country) %>%
  # now the `handed` grouping has been removed, therefore we can calculate percent now:
  mutate(n_col = sum(c_h_count), c_h_percent = c_h_count / n_col)
c_h_df

## -----------------------------------------------------------------------------
c_df <- df %>%
  group_by(arm, gender, country) %>%
  summarize(c_count = n()) %>%
  # now the `handed` grouping has been removed, therefore we can calculate percent now:
  mutate(n_col = sum(c_count), c_percent = c_count / n_col)
c_df

## -----------------------------------------------------------------------------
full_dplyr <- left_join(c_h_df, c_df) %>% ungroup()

## -----------------------------------------------------------------------------
lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  split_rows_by("country") %>%
  summarize_row_groups() %>%
  split_rows_by("handed") %>%
  summarize_row_groups() %>%
  analyze("age", afun = mean, format = "xx.x")

tbl <- build_table(lyt, df)
tbl

## -----------------------------------------------------------------------------
frm_rtables_h <- cell_values(
  tbl,
  rowpath = c("country", "CAN", "handed", "Right", "@content"),
  colpath = c("arm", "Arm B", "gender", "Female")
)[[1]]
frm_rtables_h

frm_dplyr_h <- full_dplyr %>%
  filter(country == "CAN" & handed == "Right" & arm == "Arm B" & gender == "Female") %>%
  select(c_h_count, c_h_percent)

frm_dplyr_h


frm_rtables_c <- cell_values(
  tbl,
  rowpath = c("country", "CAN", "@content"),
  colpath = c("arm", "Arm A", "gender", "Male")
)[[1]]

frm_rtables_c

frm_dplyr_c <- full_dplyr %>%
  filter(country == "CAN" & arm == "Arm A" & gender == "Male") %>%
  select(c_count, c_percent)

frm_dplyr_c

## ----echo = FALSE, result="hidden"--------------------------------------------
stopifnot(isTRUE(all.equal(frm_rtables_h, unname(unlist(frm_dplyr_h)))))
stopifnot(isTRUE(all.equal(frm_rtables_c, unname(unlist(frm_dplyr_c[1, ])))))

