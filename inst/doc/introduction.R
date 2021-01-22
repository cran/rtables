## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(comment = "")

## ---- message=FALSE-----------------------------------------------------------
library(rtables)
library(dplyr)

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

## ---- echo=FALSE--------------------------------------------------------------
basic_table() %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  add_colcounts() %>%
  split_rows_by("country") %>%
  summarize_row_groups() %>%
  split_rows_by("handed") %>%
  summarize_row_groups() %>%
  analyze("age", afun = mean, format = "xx.x") %>%
  build_table(df)

## -----------------------------------------------------------------------------
l <- basic_table() %>%
  analyze("age", mean, format = "xx.x")

build_table(l, df)

## -----------------------------------------------------------------------------
l

## -----------------------------------------------------------------------------
l <- basic_table() %>%
  split_cols_by("arm") %>%
  analyze("age", afun = mean, format = "xx.x")

build_table(l, df)

## -----------------------------------------------------------------------------
l <- basic_table() %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  analyze("age", afun = mean, format = "xx.x")

build_table(l, df)

## -----------------------------------------------------------------------------
l <- basic_table() %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  split_rows_by("country") %>%
  analyze("age", afun = mean, format = "xx.x")

build_table(l, df)

## -----------------------------------------------------------------------------
mean(df$age[df$country == "CAN" & df$arm == "Arm A" & df$gender == "Female"])

## -----------------------------------------------------------------------------
l <- basic_table() %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  split_rows_by("country") %>%
  summarize_row_groups() %>%
  analyze("age", afun = mean, format = "xx.x")

build_table(l, df)

## -----------------------------------------------------------------------------
df_cell <- subset(df, df$country == "CAN" & df$arm == "Arm A" & df$gender == "Female")
df_col_1 <- subset(df, df$arm == "Arm A" & df$gender == "Female")

c(count = nrow(df_cell), percentage = nrow(df_cell)/nrow(df_col_1))

## -----------------------------------------------------------------------------
l <- basic_table() %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  split_rows_by("country") %>%
  summarize_row_groups() %>%
  split_rows_by("handed") %>%
  analyze("age", afun = mean, format = "xx.x")

build_table(l, df)

## -----------------------------------------------------------------------------
l <- basic_table() %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  split_rows_by("country") %>%
  summarize_row_groups() %>%
  split_rows_by("handed") %>%
  summarize_row_groups() %>%
  analyze("age", afun = mean, format = "xx.x")

build_table(l, df)

