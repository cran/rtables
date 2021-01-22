## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(comment = "")

## ---- message=FALSE-----------------------------------------------------------
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
basic_table() %>%
  split_cols_by("arm") %>%
  split_cols_by("gender") %>%
  split_rows_by("country") %>%
  split_rows_by("handed") %>%
  analyze("age", afun = mean, format = "xx.x") %>%
  build_table(df)

## -----------------------------------------------------------------------------
df %>% 
  group_by(arm, gender, country, handed) %>%
  summarize(mean = mean(age), c_h_count = n()) %>%
  # now the `handed` grouping has been removed, therefore we can calculate percent now:
  mutate(c_h_percent = c_h_count / sum(c_h_count))

## -----------------------------------------------------------------------------
df %>% 
  group_by(arm, gender, country, handed) %>%
  summarize(mean = mean(age), c_h_count = n()) %>%
  mutate(c_h_percent = c_h_count / sum(c_h_count)) %>%
  mutate(c_count = sum(c_h_count)) %>%
  ungroup(country) %>%
  # note that we always use the finest level of counts to avoid duplicate counting
  mutate(c_percent = c_count / sum(c_h_count))

## -----------------------------------------------------------------------------
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

