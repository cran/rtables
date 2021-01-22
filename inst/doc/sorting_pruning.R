## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(comment = "")

## ---- messages=FALSE----------------------------------------------------------
library(rtables)
library(dplyr)

## -----------------------------------------------------------------------------
rawtable <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX") %>%
    split_rows_by("RACE") %>%
    summarize_row_groups() %>%
    split_rows_by("STRATA1") %>%
    summarize_row_groups() %>%
    analyze("AGE") %>%
    build_table(DM)
rawtable

## -----------------------------------------------------------------------------
trim_rows(rawtable)

## -----------------------------------------------------------------------------
coltrimmed <- rawtable[,col_counts(rawtable) > 0]
head(coltrimmed)

## -----------------------------------------------------------------------------
pruned <- prune_table(coltrimmed)
pruned

## -----------------------------------------------------------------------------
pruned2 <- prune_table(coltrimmed, low_obs_pruner(10, "mean"))
pruned2

## -----------------------------------------------------------------------------
pruned3 <- prune_table(coltrimmed, low_obs_pruner(10, "sum"), stop_depth = 1)
pruned3

## -----------------------------------------------------------------------------
pruned4 <- prune_table(coltrimmed, low_obs_pruner(16, "sum"))
pruned4

## -----------------------------------------------------------------------------
sort_at_path(pruned, path = c("RACE", "ASIAN", "STRATA1"), scorefun = cont_n_allcols)

## -----------------------------------------------------------------------------
ethsort <- sort_at_path(pruned, path = c("RACE"), scorefun = cont_n_allcols, decreasing = FALSE)
ethsort

## -----------------------------------------------------------------------------
sort_at_path(pruned, path = c("RACE", "*", "STRATA1"), cont_n_onecol(5))


## -----------------------------------------------------------------------------
silly_afun = function(x) {
    in_rows(a = rcell(2),
            b = rcell(3),
            c = rcell(1))
}



sillytbl <- basic_table() %>% split_rows_by("cyl") %>%
    analyze("mpg", silly_afun) %>%
    build_table(mtcars)
sillytbl

## -----------------------------------------------------------------------------
scorefun <- function(tt) { mean(unlist(row_values(tt)))}
sort_at_path(sillytbl, c("cyl", "*", "mpg"), scorefun)

## -----------------------------------------------------------------------------
silly_name_scorer = function(tt) {
    nm = obj_name(tt)
    print(nm)
    nm
}

sort_at_path(ethsort, "RACE", silly_name_scorer)

## -----------------------------------------------------------------------------
silly_gender_diffcount = function(tt) {
    ctable = content_table(tt) ## get summary table at this location
    crow = tree_children(ctable)[[1]] ## get first row in summary table
    vals = row_values(crow)
    ## we need to have a better api for specificying location in column space but currently we don't
    mcount = vals[[6]][1]
    fcount = vals[[5]][1]
    (mcount - fcount)/fcount
}

sort_at_path(pruned, c("RACE", "*", "STRATA1"), silly_gender_diffcount)

