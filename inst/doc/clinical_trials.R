## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(comment = "")

## ---- message=FALSE-----------------------------------------------------------
library(rtables)
library(tibble)
library(dplyr)

## -----------------------------------------------------------------------------
ADSL <- ex_adsl  # Example ADSL dataset

basic_table() %>%
  split_cols_by("ARM") %>%
  analyze(vars = "AGE", afun = function(x) {
    in_rows(
      "Mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
      "Range" = rcell(range(x), format = "xx.xx - xx.xx")
    )
  }) %>%
  build_table(ADSL)

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by("ARM") %>%
  analyze(vars = c("AGE", "BMRKR1"), afun = function(x) {
    in_rows(
      "Mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
      "Range" = rcell(range(x), format = "xx.xx - xx.xx")
    )
  }) %>%
  build_table(ADSL)

## -----------------------------------------------------------------------------
s_summary <- function(x) {
  if (is.numeric(x)) {
    in_rows(
      "n" = rcell(sum(!is.na(x)), format = "xx"),
      "Mean (sd)" = rcell(c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)), format = "xx.xx (xx.xx)"),
      "IQR" = rcell(IQR(x, na.rm = TRUE), format = "xx.xx"),
      "min - max" = rcell(range(x, na.rm = TRUE), format = "xx.xx - xx.xx")
    )
  } else if (is.factor(x)) {
    
    vs <- as.list(table(x))
    do.call(in_rows, lapply(vs, rcell, format = "xx"))
    
  } else (
    stop("type not supported")
  )
}

## -----------------------------------------------------------------------------
s_summary(ADSL$AGE)

## -----------------------------------------------------------------------------
s_summary(ADSL$SEX)

## -----------------------------------------------------------------------------
lyt <- basic_table() %>% 
  split_cols_by(var = "ARM") %>%
  analyze(c("AGE", "SEX"), afun = s_summary) 

tbl <- build_table(lyt, ADSL)
tbl

## -----------------------------------------------------------------------------
tbl2 <- basic_table() %>% 
  split_cols_by(var = "ARM") %>%
  analyze("AGE", s_summary) %>%
  analyze("SEX", s_summary) %>%
  build_table(ADSL) 

tbl2

## -----------------------------------------------------------------------------
identical(tbl, tbl2)

## ---- echo=FALSE--------------------------------------------------------------
stopifnot(identical(tbl, tbl2))

## -----------------------------------------------------------------------------
basic_table() %>% 
  split_cols_by(var = "ARMCD") %>%
  add_colcounts() %>%
  analyze(c("AGE", "SEX"), s_summary) %>%
  build_table(ADSL) 

## -----------------------------------------------------------------------------
basic_table() %>% 
  split_cols_by(var = "ARM") %>%
  add_colcounts() %>%
  analyze(c("AGE", "BMRKR2"), s_summary) %>%
  build_table(ADSL) 

## ---- warning=FALSE-----------------------------------------------------------
basic_table() %>% 
  split_cols_by(var = "ARM") %>%
  add_colcounts() %>%
  split_rows_by("SEX") %>%
  analyze(c("AGE", "BMRKR2"), s_summary) %>%
  build_table(ADSL) 

## ---- warning=FALSE-----------------------------------------------------------
ADSL_M_F <- filter(ADSL, SEX %in% c("M", "F"))

basic_table() %>% 
  split_cols_by(var = "ARM") %>%
  add_colcounts() %>%
  split_rows_by("SEX") %>%
  analyze(c("AGE", "BMRKR2"), s_summary) %>%
  build_table(ADSL_M_F) 

## -----------------------------------------------------------------------------
basic_table() %>% 
  split_cols_by(var = "ARM") %>%
  add_colcounts() %>%
  split_rows_by("SEX", split_fun = drop_split_levels, child_labels = "visible") %>%
  analyze(c("AGE", "BMRKR2"), s_summary) %>%
  build_table(ADSL_M_F) 

## -----------------------------------------------------------------------------
ADSL_M_F_l <- ADSL_M_F %>% 
  mutate(lbl_sex = case_when(
    SEX == "M" ~ "Male",
    SEX == "F" ~ "Female",
    SEX == "U" ~ "Unknown",
    SEX == "UNDIFFERENTIATED" ~ "Undifferentiated"
  ))

basic_table() %>% 
  split_cols_by(var = "ARM") %>%
  add_colcounts() %>%
  split_rows_by("SEX", labels_var = "lbl_sex", split_fun = drop_split_levels, child_labels = "visible") %>%
  analyze(c("AGE", "BMRKR2"), s_summary) %>%
  build_table(ADSL_M_F_l)

## -----------------------------------------------------------------------------
basic_table() %>% 
  split_cols_by(var = "ARM") %>%
  add_colcounts() %>%
  split_rows_by("SEX", labels_var = "lbl_sex", split_fun = drop_split_levels, child_labels = "visible") %>%
  analyze("AGE", s_summary, show_labels = "visible") %>%
  analyze("BMRKR2", s_summary, nested = FALSE,  show_labels = "visible") %>%
  build_table(ADSL_M_F_l) 

## -----------------------------------------------------------------------------
insert_NAs <- function(x) {
  x[sample(c(TRUE, FALSE), length(x), TRUE, prob = c(0.2, 0.8))] <- NA
  x
}

set.seed(1)
ADSL_NA <- ADSL_M_F_l %>% 
  mutate(AGE = insert_NAs(AGE))

basic_table() %>% 
  split_cols_by(var = "ARM") %>%
  add_colcounts() %>%
  split_rows_by("SEX", labels_var = "lbl_sex", split_fun = drop_split_levels, child_labels = "visible") %>%
  analyze("AGE", s_summary) %>%
  analyze("BMRKR2", s_summary, nested = FALSE,  show_labels = "visible") %>%
  build_table(filter(ADSL_NA, SEX %in% c("M", "F"))) 

## -----------------------------------------------------------------------------
basic_table() %>% 
  split_cols_by(var = "ARM") %>%
  add_colcounts() %>%
  split_rows_by("SEX", labels_var = "lbl_sex", split_fun = drop_split_levels) %>%
  summarize_row_groups()  %>% 
  analyze("AGE", s_summary) %>%
  analyze("BMRKR2", afun = s_summary, nested = FALSE,  show_labels = "visible") %>%
  build_table(filter(ADSL_NA, SEX %in% c("M", "F"))) 

## -----------------------------------------------------------------------------
basic_table() %>% 
  split_cols_by(var = "ARM") %>%
  add_colcounts() %>%
  split_rows_by("SEX", labels_var = "lbl_sex", split_fun = drop_split_levels) %>%
  summarize_row_groups(cfun = function(df, labelstr, .N_col, ...) {
    in_rows(
      rcell(nrow(df) * c(1, 1/.N_col), format = "xx (xx.xx%)"),
      .labels = labelstr
    )
  })  %>% 
  analyze("AGE", s_summary) %>%
  analyze("BEP01FL", afun = s_summary, nested = FALSE,  show_labels = "visible") %>%
  build_table(filter(ADSL_NA, SEX %in% c("M", "F"))) 

## -----------------------------------------------------------------------------
basic_table() %>% 
  split_cols_by(var = "ARM") %>%
  split_rows_by("SEX", labels_var = "lbl_sex", split_fun = drop_split_levels, child_labels = "hidden") %>%
  summarize_row_groups(cfun = function(df, labelstr, .N_col, ...) {
    in_rows(
       rcell(nrow(df) * c(1, 1/.N_col), format = "xx (xx.xx%)"),
       .labels = paste0(labelstr, ": count (perc.)")
    )
  })  %>% 
  analyze("AGE", s_summary) %>%
  analyze("BEP01FL", s_summary, nested = FALSE, show_labels = "visible") %>%
  build_table(filter(ADSL_NA, SEX %in% c("M", "F"))) 

## -----------------------------------------------------------------------------
lyt <- basic_table() %>% 
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  analyze(c("AGE", "SEX"), afun = s_summary)

lyt

## -----------------------------------------------------------------------------
build_table(lyt, ADSL)

## -----------------------------------------------------------------------------
build_table(lyt, ADSL %>% filter(AGE > 18))

## -----------------------------------------------------------------------------
set.seed(1)

lookup <- tribble(
  ~AEDECOD,                          ~AEBODSYS,                                         ~AETOXGR,
  'HEADACHE',                        "NERVOUS SYSTEM DISORDERS",                        "5",
  'BACK PAIN',                       "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS", "2",
  'GINGIVAL BLEEDING',               "GASTROINTESTINAL DISORDERS",                      "1",
  'HYPOTENSION',                     "VASCULAR DISORDERS",                              "3",
  'FAECES SOFT',                     "GASTROINTESTINAL DISORDERS",                      "2",
  'ABDOMINAL DISCOMFORT',            "GASTROINTESTINAL DISORDERS",                      "1",
  'DIARRHEA',                        "GASTROINTESTINAL DISORDERS",                      "1",
  'ABDOMINAL FULLNESS DUE TO GAS',   "GASTROINTESTINAL DISORDERS",                      "1",
  'NAUSEA (INTERMITTENT)',           "GASTROINTESTINAL DISORDERS",                      "2",
  'WEAKNESS',                        "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS", "3",
  'ORTHOSTATIC HYPOTENSION',         "VASCULAR DISORDERS",                              "4"
)

normalize <- function(x) x/sum(x)
weightsA <- normalize(c(0.1, dlnorm(seq(0, 5, length.out = 25), meanlog = 3)))
weightsB <- normalize(c(0.2, dlnorm(seq(0, 5, length.out = 25))))

N_pop <- 300
ADSL2 <- data.frame(
  USUBJID = seq(1, N_pop, by = 1),
  ARM = sample(c('ARM A', 'ARM B'), N_pop, TRUE),
  SEX = sample(c('F', 'M'), N_pop, TRUE),
  AGE = 20 + rbinom(N_pop, size=40, prob=0.7)
)
                                      
l.adae <- mapply(ADSL2$USUBJID, ADSL2$ARM, ADSL2$SEX, ADSL2$AGE, FUN = function(id, arm, sex, age) {
  n_ae <- sample(0:25, 1, prob = if (arm == "ARM A") weightsA else weightsB)
  i <- sample(1:nrow(lookup), size = n_ae, replace = TRUE, prob = c(6, rep(1, 10))/16)
  lookup[i, ] %>% 
    mutate(
      AESEQ = seq_len(n()),
      USUBJID = id, ARM = arm, SEX = sex, AGE = age
    )
}, SIMPLIFY = FALSE)

ADAE2 <- do.call(rbind, l.adae)
ADAE2 <- ADAE2 %>% 
  mutate(
    ARM = factor(ARM, levels = c("ARM A", "ARM B")),
    AEDECOD = as.factor(AEDECOD),
    AEBODSYS = as.factor(AEBODSYS), 
    AETOXGR = factor(AETOXGR, levels = as.character(1:5))
  ) %>% 
  select(USUBJID, ARM, AGE, SEX, AESEQ, AEDECOD, AEBODSYS, AETOXGR)
  
ADAE2

## -----------------------------------------------------------------------------
s_events_patients <- function(x, labelstr, .N_col) {
  in_rows(
    "Total number of patients with at least one event" = 
      rcell(length(unique(x)) * c(1, 1/.N_col), format = "xx (xx.xx%)"),
    
    "Total number of events" = rcell(length(x), format = "xx")
  )
}

## -----------------------------------------------------------------------------
s_events_patients(x = c("id 1", "id 1", "id 2"), .N_col = 5)

## -----------------------------------------------------------------------------
basic_table() %>% 
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  analyze("USUBJID", s_events_patients) %>%
  build_table(ADAE2)

## -----------------------------------------------------------------------------
N_per_arm <- table(ADSL2$ARM)
N_per_arm

## -----------------------------------------------------------------------------
basic_table() %>% 
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  analyze("USUBJID", s_events_patients) %>%
  build_table(ADAE2, col_counts = N_per_arm)

## -----------------------------------------------------------------------------
l <- basic_table() %>% 
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  analyze("USUBJID", s_events_patients) %>%
  split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE)  %>%
  summarize_row_groups("USUBJID", cfun = s_events_patients)
  build_table(l, ADAE2, col_counts = N_per_arm)

## -----------------------------------------------------------------------------
tbl1 <- basic_table() %>% 
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  split_rows_by("AEBODSYS", child_labels = "visible", indent_mod = 1)  %>%
  summarize_row_groups("USUBJID", cfun = s_events_patients) %>%
  analyze("AEDECOD", indent_mod = -1) %>%
  build_table(ADAE2, col_counts = N_per_arm)

tbl1

## -----------------------------------------------------------------------------
table_count_once_per_id <- function(df, termvar = "AEDECOD", idvar = "USUBJID") {

  x <- df[[termvar]]
  id <- df[[idvar]]
 
  counts <- table(x[!duplicated(id)])
  
  in_rows(
    .list = as.vector(counts),
    .labels = names(counts)
  )
}

table_count_once_per_id(ADAE2)

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  split_rows_by("AEBODSYS", child_labels = "visible", indent_mod = 1)  %>%
  summarize_row_groups("USUBJID", cfun = s_events_patients) %>%
  analyze("AEDECOD", afun = table_count_once_per_id, show_labels = "hidden", indent_mod = -1) %>%
  build_table(ADAE2, col_counts = N_per_arm)

## -----------------------------------------------------------------------------
tbl <- basic_table() %>% 
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  analyze("USUBJID", afun = s_events_patients) %>% 
  split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE, indent_mod = 1)  %>%
  summarize_row_groups("USUBJID", cfun = s_events_patients) %>%
  analyze("AEDECOD", table_count_once_per_id, show_labels = "hidden", indent_mod = -1) %>%
  build_table(ADAE2, col_counts = N_per_arm)

tbl

## -----------------------------------------------------------------------------
trim_rows(tbl)

## -----------------------------------------------------------------------------
table_count_grade_once_per_id <- function(df, labelstr = "", gradevar = "AETOXGR", idvar = "USUBJID", grade_levels = NULL) {
  
  id <- df[[idvar]]
  grade <- df[[gradevar]]
  
  if (!is.null(grade_levels)) {
    stopifnot(all(grade %in% grade_levels))
    grade <- factor(grade, levels = grade_levels)
  }
  
  id_sel <- !duplicated(id)
  
  in_rows(
      "--Any Grade--" = sum(id_sel),
      .list =  as.list(table(grade[id_sel]))
    )
}

table_count_grade_once_per_id(ex_adae, grade_levels = 1:5)

## -----------------------------------------------------------------------------
basic_table() %>% 
  split_cols_by("ARM") %>%
  add_colcounts() %>%
  analyze("AETOXGR", 
          afun = table_count_grade_once_per_id, 
          extra_args = list(grade_levels = 1:5),
          var_labels = "- Any adverse events -", show_labels = "visible") %>%
  split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE, indent_mod = 1) %>%
  summarize_row_groups(cfun = table_count_grade_once_per_id, format = "xx", indent_mod = 1) %>%
  split_rows_by("AEDECOD", child_labels = "visible", indent_mod = -2)  %>%
  analyze("AETOXGR", 
          afun = table_count_grade_once_per_id, 
          extra_args = list(grade_levels = 1:5), show_labels = "hidden") %>%
  build_table(ADAE2, col_counts = N_per_arm)

## -----------------------------------------------------------------------------
ADRS_BESRSPI <- ex_adrs %>%
  filter(PARAMCD == "BESRSPI") %>%
  mutate(
    rsp = factor(AVALC %in% c("CR", "PR"), levels = c(TRUE, FALSE), labels = c("Responders", "Non-Responders")),
    is_rsp = (rsp == "Responders")
  )

s_proportion <- function(x, .N_col) {
   in_rows(.list = lapply(as.list(table(x)), function(xi) rcell(xi * c(1, 1/.N_col), format = "xx.xx (xx.xx%)")))
}

basic_table() %>%
  split_cols_by("ARMCD", ref_group = "ARM A") %>%
  add_colcounts() %>%
  analyze("rsp", s_proportion, show_labels = "hidden") %>%
  build_table(ADRS_BESRSPI)

## -----------------------------------------------------------------------------
s_unstratified_response_analysis <- function(x, .ref_group, .in_ref_col) {
  
  if (.in_ref_col) {
    return(in_rows(
        "Difference in Response Rates (%)" = rcell(numeric(0)),
        "95% CI (Wald, with correction)" = rcell(numeric(0)),
        "p-value (Chi-Squared Test)" = rcell(numeric(0)),
        "Odds Ratio (95% CI)" = rcell(numeric(0))
    ))
  }
  
  fit <- stats::prop.test(
    x = c(sum(x), sum(.ref_group)),
    n = c(length(x), length(.ref_group)),
    correct = FALSE
  )
  
  fit_glm <- stats::glm(
    formula = rsp ~ group,
    data = data.frame(
      rsp = c(.ref_group, x), 
      group = factor(rep(c("ref", "x"), times = c(length(.ref_group), length(x))), levels = c("ref", "x"))
    ),
    family = binomial(link = "logit")
  )

  in_rows(
      "Difference in Response Rates (%)" = non_ref_rcell((mean(x) - mean(.ref_group)) * 100,
                                                         .in_ref_col, format = "xx.xx") ,
      "95% CI (Wald, with correction)" = non_ref_rcell(fit$conf.int * 100,
                                                       .in_ref_col, format = "(xx.xx, xx.xx)"),
      "p-value (Chi-Squared Test)" = non_ref_rcell(fit$p.value,
                                                   .in_ref_col, format = "x.xxxx | (<0.0001)"),
      "Odds Ratio (95% CI)" = non_ref_rcell(c(
          exp(stats::coef(fit_glm)[-1]),
          exp(stats::confint.default(fit_glm, level = .95)[-1, , drop = FALSE])
      ),
      .in_ref_col, format = "xx.xx (xx.xx - xx.xx)")
  )
}

s_unstratified_response_analysis(
  x = ADRS_BESRSPI %>% filter(ARM == "A: Drug X") %>% pull(is_rsp), 
  .ref_group = ADRS_BESRSPI %>% filter(ARM == "B: Placebo") %>% pull(is_rsp),
  .in_ref_col = FALSE
)

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by("ARMCD", ref_group = "ARM A") %>%
  add_colcounts() %>%
  analyze("rsp", s_proportion, show_labels = "hidden") %>%
  analyze("is_rsp", s_unstratified_response_analysis, show_labels = "visible", var_labels = "Unstratified Response Analysis") %>%
  build_table(ADRS_BESRSPI)

## -----------------------------------------------------------------------------
s_prop <- function(df, .N_col) {
  in_rows(
    "95% CI (Wald, with correction)" = rcell(binom.test(nrow(df), .N_col)$conf.int * 100, format = "(xx.xx, xx.xx)")
  )
}

s_prop(
  df = ADRS_BESRSPI %>% filter(ARM == "A: Drug X", AVALC == "CR"), 
  .N_col = sum(ADRS_BESRSPI$ARM == "A: Drug X")
)

## -----------------------------------------------------------------------------
basic_table() %>%
  split_cols_by("ARMCD", ref_group = "ARM A") %>%
  add_colcounts() %>%
  analyze("rsp", s_proportion, show_labels = "hidden") %>%
  analyze("is_rsp", s_unstratified_response_analysis, 
          show_labels = "visible", var_labels = "Unstratified Response Analysis") %>%
  split_rows_by(
    var = "AVALC",
    split_fun = reorder_split_levels(neworder = c("CR", "PR", "SD", "NON CR/PD", "PD", "NE"), drlevels = TRUE), 
    nested = FALSE
  ) %>%
  summarize_row_groups() %>%
  analyze("AVALC", afun = s_prop) %>%
  build_table(ADRS_BESRSPI)

## -----------------------------------------------------------------------------
rsp_label <- function(x) {
  rsp_full_label <- c(
    CR          = "Complete Response (CR)",
    PR          = "Partial Response (PR)",
    SD          = "Stable Disease (SD)",
    `NON CR/PD` = "Non-CR or Non-PD (NON CR/PD)",
    PD          = "Progressive Disease (PD)",
    NE          = "Not Evaluable (NE)",
    Missing     = "Missing",
    `NE/Missing` = "Missing or unevaluable"
  )
  stopifnot(all(x %in% names(rsp_full_label)))
  rsp_full_label[x]
}


tbl <- basic_table() %>%
  split_cols_by("ARMCD", ref_group = "ARM A") %>%
  add_colcounts() %>%
  analyze("rsp", s_proportion, show_labels = "hidden") %>%
  analyze("is_rsp", s_unstratified_response_analysis, 
          show_labels = "visible", var_labels = "Unstratified Response Analysis") %>%
  split_rows_by(
    var = "AVALC",
    split_fun = keep_split_levels(c("CR", "PR", "SD", "PD"), reorder = TRUE), 
    nested = FALSE
  ) %>%
  summarize_row_groups(cfun = function(df, labelstr, .N_col) {
    in_rows(nrow(df) * c(1, 1/.N_col), .formats = "xx (xx.xx%)", .labels = rsp_label(labelstr))
  }) %>%
  analyze("AVALC", afun = s_prop) %>%
  analyze("AVALC", afun = function(x, .N_col) {
    in_rows(rcell(sum(x == "NE") * c(1, 1/.N_col), format = "xx.xx (xx.xx%)"), .labels = rsp_label("NE"))
  }, nested = FALSE) %>%
  build_table(ADRS_BESRSPI)

tbl

