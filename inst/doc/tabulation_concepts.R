## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(comment = "#")

## ----message=FALSE------------------------------------------------------------
library(dplyr)
library(tibble)
library(rtables)

## -----------------------------------------------------------------------------
add_subgroup <- function(x) paste0(tolower(x), sample(1:3, length(x), TRUE))

set.seed(1)

df <- tibble(
  x = rnorm(100),
  c1 = factor(sample(c("A", "B", "C"), 100, replace = TRUE), levels = c("A", "B", "C")),
  r1 = factor(sample(c("U", "V", "W"), 100, replace = TRUE), levels = c("U", "V", "W"))
) %>%
  mutate(
    c2 = add_subgroup(c1),
    r2 = add_subgroup(r1),
    y = as.numeric(2 * as.numeric(c1) - 3 * as.numeric(r1))
  ) %>%
  select(c1, c2, r1, r2, x, y)

df

## -----------------------------------------------------------------------------
df_A <- df %>% filter(c1 == "A")
df_B <- df %>% filter(c1 == "B")
df_C <- df %>% filter(c1 == "C")

## -----------------------------------------------------------------------------
foo <- prod
bar <- sum
zoo <- mean

lyt <- basic_table() %>%
  split_cols_by("c1") %>%
  analyze("x", function(df) foo(df$x), var_labels = "foo label", format = "xx.xx") %>%
  analyze("x", function(df) bar(df$x), var_labels = "bar label", format = "xx.xx") %>%
  analyze("x", function(df) zoo(df$x), var_labels = "zoo label", format = "xx.xx")

tbl <- build_table(lyt, df)
tbl

## -----------------------------------------------------------------------------
x_A <- df_A$x
x_B <- df_B$x
x_C <- df_C$x

## -----------------------------------------------------------------------------
lyt2 <- basic_table() %>%
  split_cols_by("c1") %>%
  analyze("x", foo, var_labels = "foo label", format = "xx.xx") %>%
  analyze("x", bar, var_labels = "bar label", format = "xx.xx") %>%
  analyze("x", zoo, var_labels = "zoo label", format = "xx.xx")

tbl2 <- build_table(lyt2, df)
tbl2

## -----------------------------------------------------------------------------
lyt3 <- basic_table() %>%
  split_cols_by("c1") %>%
  analyze("x", function(x) {
    in_rows(
      "row 1" = rcell(mean(x), format = "xx.xx"),
      "row 2" = rcell(sd(x), format = "xx.xxx")
    )
  }, var_labels = "foo label") %>%
  analyze("x", function(x) {
    in_rows(
      "more rows 1" = rcell(median(x), format = "xx.x"),
      "even more rows 1" = rcell(IQR(x), format = "xx.xx")
    )
  }, var_labels = "bar label", format = "xx.xx")

tbl3 <- build_table(lyt3, df)
tbl3

## -----------------------------------------------------------------------------
df_UA <- df %>% filter(r1 == "U", c1 == "A")
df_VA <- df %>% filter(r1 == "V", c1 == "A")
df_WA <- df %>% filter(r1 == "W", c1 == "A")
df_UB <- df %>% filter(r1 == "U", c1 == "B")
df_VB <- df %>% filter(r1 == "V", c1 == "B")
df_WB <- df %>% filter(r1 == "W", c1 == "C")
df_UC <- df %>% filter(r1 == "U", c1 == "C")
df_VC <- df %>% filter(r1 == "V", c1 == "C")
df_WC <- df %>% filter(r1 == "W", c1 == "C")

## -----------------------------------------------------------------------------
foo <- function(df, labelstr = "", ...) {
  paste(dim(df), collapse = " x ")
}

## -----------------------------------------------------------------------------
foo(df_UA)
foo(df_VA)
foo(df_WA)
foo(df_UB)
foo(df_VB)
foo(df_WB)
foo(df_UC)
foo(df_VC)
foo(df_WC)

## -----------------------------------------------------------------------------
matrix(
  list(
    foo(df_UA),
    foo(df_VA),
    foo(df_WA),
    foo(df_UB),
    foo(df_VB),
    foo(df_WB),
    foo(df_UC),
    foo(df_VC),
    foo(df_WC)
  ),
  byrow = FALSE, ncol = 3
)

## -----------------------------------------------------------------------------
lyt4 <- basic_table() %>%
  split_cols_by("c1") %>%
  split_rows_by("r1") %>%
  analyze("x", foo)

tbl4 <- build_table(lyt4, df)
tbl4

## -----------------------------------------------------------------------------
lyt5 <- basic_table() %>%
  split_cols_by("c1") %>%
  split_rows_by("r1") %>%
  summarize_row_groups(cfun = foo, format = "xx")

tbl5 <- build_table(lyt5, df)
tbl5

## -----------------------------------------------------------------------------
foo <- function(df, labelstr) {
  rcell(paste(dim(df), collapse = " x "), format = "xx", label = labelstr)
}

lyt6 <- basic_table() %>%
  split_cols_by("c1") %>%
  split_rows_by("r1") %>%
  summarize_row_groups(cfun = foo)

tbl6 <- build_table(lyt6, df)
tbl6

## -----------------------------------------------------------------------------
foo <- function(df, labelstr) {
  rcell(mean(df$y), label = labelstr, format = "xx.xx")
}

lyt7 <- basic_table() %>%
  split_cols_by("c1") %>%
  split_rows_by("r1") %>%
  summarize_row_groups(cfun = foo)

tbl7 <- build_table(lyt7, df)
tbl7

## -----------------------------------------------------------------------------
lyt8 <- basic_table() %>%
  split_cols_by("c1") %>%
  split_rows_by("r1") %>%
  analyze("y", afun = mean)

tbl8 <- build_table(lyt8, df)
tbl8

## -----------------------------------------------------------------------------
lyt9 <- basic_table() %>%
  split_cols_by("c1") %>%
  split_rows_by("r1") %>%
  analyze("y", afun = function(df) mean(df$y))

tbl9 <- build_table(lyt9, df)
tbl9

## -----------------------------------------------------------------------------
lyt10 <- basic_table() %>%
  split_cols_by("c1") %>%
  split_rows_by("r1") %>%
  analyze("y", afun = function(x) mean(x))

tbl10 <- build_table(lyt10, df)
tbl10

## -----------------------------------------------------------------------------
df %>%
  filter(r1 == "U", r2 == "u1", c1 == "A")

## -----------------------------------------------------------------------------
lyt11 <- basic_table() %>%
  split_cols_by("c1") %>%
  split_rows_by("r1") %>%
  split_rows_by("r2") %>%
  summarize_row_groups(cfun = function(df, labelstr) {
    rcell(mean(df$x), format = "xx.xx", label = paste("mean x for", labelstr))
  })

tbl11 <- build_table(lyt11, df)
tbl11

## -----------------------------------------------------------------------------
s_mean_sd <- function(x) {
  in_rows("mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"))
}

s_range <- function(x) {
  in_rows("range" = rcell(range(x), format = "xx.xx - xx.xx"))
}

lyt12 <- basic_table() %>%
  split_cols_by("c1") %>%
  split_rows_by("r1") %>%
  split_rows_by("r2") %>%
  analyze("x", s_mean_sd, show_labels = "hidden") %>%
  analyze("x", s_range, show_labels = "hidden")

tbl12 <- build_table(lyt12, df)
tbl12

## -----------------------------------------------------------------------------
s_mean_sd <- function(x) {
  in_rows("mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"))
}

s_range <- function(x) {
  in_rows("range" = rcell(range(x), format = "xx.xx - xx.xx"))
}

s_cfun_2 <- function(df, labelstr) {
  rcell(nrow(df), format = "xx", label = paste(labelstr, "(n)"))
}

lyt13 <- basic_table() %>%
  split_cols_by("c1") %>%
  split_rows_by("r1") %>%
  split_rows_by("r2") %>%
  summarize_row_groups(cfun = s_cfun_2) %>%
  analyze("x", s_mean_sd, show_labels = "hidden") %>%
  analyze("x", s_range, show_labels = "hidden")

tbl13 <- build_table(lyt13, df)
tbl13

## -----------------------------------------------------------------------------
lyt14 <- basic_table() %>%
  split_cols_by("c1") %>%
  split_rows_by("r1") %>%
  summarize_row_groups(cfun = s_cfun_2) %>%
  split_rows_by("r2") %>%
  summarize_row_groups(cfun = s_cfun_2) %>%
  analyze("x", s_mean_sd, show_labels = "hidden") %>%
  analyze("x", s_range, show_labels = "hidden")

tbl14 <- build_table(lyt14, df)
tbl14

