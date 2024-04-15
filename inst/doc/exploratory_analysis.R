## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(comment = "#")

## ----message=FALSE------------------------------------------------------------
library(rtables)
library(dplyr)

## -----------------------------------------------------------------------------
table(ex_adsl$ARM)
table(ex_adsl$SEX, ex_adsl$ARM)

## -----------------------------------------------------------------------------
qtable(ex_adsl, col_vars = "ARM")
qtable(ex_adsl, col_vars = "ARM", row_vars = "SEX")

## -----------------------------------------------------------------------------
qtable(ex_adsl, "ARM", show_colcounts = FALSE)

## ----eval = FALSE-------------------------------------------------------------
#  tmp_adsl <- ex_adsl
#  tmp_adsl$new <- rep_len(c("", "A", "B"), nrow(tmp_adsl))
#  
#  qtable(tmp_adsl, row_vars = "new")

## -----------------------------------------------------------------------------
qtable(ex_adsl, row_vars = c("SEX", "STRATA1"), col_vars = c("ARM", "STRATA2"))

## -----------------------------------------------------------------------------
qtable(
  ex_adsl,
  row_vars = c("SEX", "STRATA1"),
  col_vars = c("ARM", "STRATA2"),
  drop_levels = FALSE
)

## -----------------------------------------------------------------------------
table(ex_adsl$SEX, ex_adsl$STRATA1, ex_adsl$ARM, ex_adsl$STRATA2)

## -----------------------------------------------------------------------------
t1 <- ftable(ex_adsl[, c("SEX", "STRATA1", "ARM", "STRATA2")])
ftable(t1, row.vars = c("SEX", "STRATA1"))

## -----------------------------------------------------------------------------
tmp_adsl <- ex_adsl
tmp_adsl[[1]] <- NA_character_

qtable(tmp_adsl, row_vars = "ARM", col_vars = "SEX")

## -----------------------------------------------------------------------------
# Recode NA values
tmp_adsl[[1]] <- addNA(tmp_adsl[[1]])

qtable(tmp_adsl, row_vars = "ARM", col_vars = "SEX")

## -----------------------------------------------------------------------------
tmp_adsl$new1 <- factor(NA_character_, levels = c("X", "Y", "Z"))
qtable(tmp_adsl, row_vars = "ARM", col_vars = "new1")

## -----------------------------------------------------------------------------
tmp_adsl$new2 <- addNA(tmp_adsl$new1)
levels(tmp_adsl$new2)[4] <- "<NA>" # NA needs to be a recognizible string
qtable(tmp_adsl, row_vars = "ARM", col_vars = "new2")

## -----------------------------------------------------------------------------
qtable(ex_adsl, row_vars = "STRATA2", col_vars = "ARM", avar = "AGE", afun = mean)

## -----------------------------------------------------------------------------
qtable(ex_adsl, row_vars = "STRATA2", col_vars = "ARM", avar = "AGE", afun = range)

## -----------------------------------------------------------------------------
fivenum2 <- function(x) {
  setNames(as.list(fivenum(x)), c("min", "Q1", "MED", "Q3", "max"))
}
qtable(ex_adsl, row_vars = "STRATA2", col_vars = "ARM", avar = "AGE", afun = fivenum2)

## -----------------------------------------------------------------------------
meansd_range <- function(x) {
  in_rows(
    "Mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
    "Range" = rcell(range(x), format = "xx - xx")
  )
}

qtable(ex_adsl, row_vars = "STRATA2", col_vars = "ARM", avar = "AGE", afun = meansd_range)

## -----------------------------------------------------------------------------
qtable(
  ex_adsl,
  row_vars = c("STRATA1", "STRATA2"), col_vars = "ARM",
  avar = "AGE", afun = mean
)

qtable(
  ex_adsl,
  row_vars = c("STRATA1", "STRATA2"), col_vars = "ARM",
  summarize_groups = TRUE, avar = "AGE", afun = mean
)

## -----------------------------------------------------------------------------
qtable(
  ex_adsl,
  row_vars = "STRATA2", col_vars = "ARM",
  title = "Strata 2 Summary",
  subtitle = paste0("STUDY ", ex_adsl$STUDYID[1]),
  main_footer = paste0("Date: ", as.character(Sys.Date()))
)

