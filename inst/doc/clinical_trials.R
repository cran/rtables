## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(comment = "#")

## ---- message=FALSE-----------------------------------------------------------
library(rtables)
library(tibble)
library(dplyr)

## -----------------------------------------------------------------------------
ADSL <- ex_adsl # Example ADSL dataset

lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  analyze(vars = "AGE", afun = function(x) {
    in_rows(
      "Mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
      "Range" = rcell(range(x), format = "xx.xx - xx.xx")
    )
  })

tbl <- build_table(lyt, ADSL)
tbl

## -----------------------------------------------------------------------------
lyt2 <- basic_table() %>%
  split_cols_by("ARM") %>%
  analyze(vars = c("AGE", "BMRKR1"), afun = function(x) {
    in_rows(
      "Mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
      "Range" = rcell(range(x), format = "xx.xx - xx.xx")
    )
  })

tbl2 <- build_table(lyt2, ADSL)
tbl2

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
  } else {
    stop("type not supported")
  }
}

## -----------------------------------------------------------------------------
s_summary(ADSL$AGE)

## -----------------------------------------------------------------------------
s_summary(ADSL$SEX)

## -----------------------------------------------------------------------------
summary_lyt <- basic_table() %>%
  split_cols_by(var = "ARM") %>%
  analyze(c("AGE", "SEX"), afun = s_summary)

summary_tbl <- build_table(summary_lyt, ADSL)
summary_tbl

## -----------------------------------------------------------------------------
summary_lyt2 <- basic_table() %>%
  split_cols_by(var = "ARM") %>%
  analyze("AGE", s_summary) %>%
  analyze("SEX", s_summary)

summary_tbl2 <- build_table(summary_lyt2, ADSL)
summary_tbl2

## -----------------------------------------------------------------------------
identical(summary_tbl, summary_tbl2)

## ---- echo=FALSE--------------------------------------------------------------
stopifnot(identical(summary_tbl, summary_tbl2))

## -----------------------------------------------------------------------------
summary_lyt3 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARMCD") %>%
  analyze(c("AGE", "SEX"), s_summary)

summary_tbl3 <- build_table(summary_lyt3, ADSL)
summary_tbl3

## -----------------------------------------------------------------------------
lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  analyze(c("AGE", "BMRKR2"), s_summary)

tbl <- build_table(lyt, ADSL)
tbl

## ---- warning=FALSE-----------------------------------------------------------
lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  split_rows_by("SEX") %>%
  analyze(c("AGE", "BMRKR2"), s_summary)

tbl <- build_table(lyt, ADSL)
tbl

## ---- warning=FALSE-----------------------------------------------------------
ADSL_M_F <- filter(ADSL, SEX %in% c("M", "F"))

lyt2 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  split_rows_by("SEX") %>%
  analyze(c("AGE", "BMRKR2"), s_summary)

tbl2 <- build_table(lyt2, ADSL_M_F)
tbl2

## -----------------------------------------------------------------------------
lyt3 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  split_rows_by("SEX", split_fun = drop_split_levels, child_labels = "visible") %>%
  analyze(c("AGE", "BMRKR2"), s_summary)

tbl3 <- build_table(lyt3, ADSL_M_F)
tbl3

## -----------------------------------------------------------------------------
ADSL_M_F_l <- ADSL_M_F %>%
  mutate(lbl_sex = case_when(
    SEX == "M" ~ "Male",
    SEX == "F" ~ "Female",
    SEX == "U" ~ "Unknown",
    SEX == "UNDIFFERENTIATED" ~ "Undifferentiated"
  ))

lyt4 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  split_rows_by("SEX", labels_var = "lbl_sex", split_fun = drop_split_levels, child_labels = "visible") %>%
  analyze(c("AGE", "BMRKR2"), s_summary)

tbl4 <- build_table(lyt4, ADSL_M_F_l)
tbl4

## -----------------------------------------------------------------------------
lyt5 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  split_rows_by("SEX", labels_var = "lbl_sex", split_fun = drop_split_levels, child_labels = "visible") %>%
  analyze("AGE", s_summary, show_labels = "visible") %>%
  analyze("BMRKR2", s_summary, nested = FALSE, show_labels = "visible")

tbl5 <- build_table(lyt5, ADSL_M_F_l)
tbl5

## -----------------------------------------------------------------------------
insert_NAs <- function(x) {
  x[sample(c(TRUE, FALSE), length(x), TRUE, prob = c(0.2, 0.8))] <- NA
  x
}

set.seed(1)
ADSL_NA <- ADSL_M_F_l %>%
  mutate(AGE = insert_NAs(AGE))

lyt6 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  split_rows_by(
    "SEX",
    labels_var = "lbl_sex",
    split_fun = drop_split_levels,
    child_labels = "visible"
  ) %>%
  analyze("AGE", s_summary) %>%
  analyze("BMRKR2", s_summary, nested = FALSE, show_labels = "visible")

tbl6 <- build_table(lyt6, filter(ADSL_NA, SEX %in% c("M", "F")))
tbl6

## -----------------------------------------------------------------------------
lyt7 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  split_rows_by("SEX", labels_var = "lbl_sex", split_fun = drop_split_levels) %>%
  summarize_row_groups() %>%
  analyze("AGE", s_summary) %>%
  analyze("BMRKR2", afun = s_summary, nested = FALSE, show_labels = "visible")

tbl7 <- build_table(lyt7, filter(ADSL_NA, SEX %in% c("M", "F")))
tbl7

## -----------------------------------------------------------------------------
lyt8 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM") %>%
  split_rows_by("SEX", labels_var = "lbl_sex", split_fun = drop_split_levels) %>%
  summarize_row_groups(cfun = function(df, labelstr, .N_col, ...) {
    in_rows(
      rcell(nrow(df) * c(1, 1 / .N_col), format = "xx (xx.xx%)"),
      .labels = labelstr
    )
  }) %>%
  analyze("AGE", s_summary) %>%
  analyze("BEP01FL", afun = s_summary, nested = FALSE, show_labels = "visible")

tbl8 <- build_table(lyt8, filter(ADSL_NA, SEX %in% c("M", "F")))
tbl8

## -----------------------------------------------------------------------------
lyt9 <- basic_table() %>%
  split_cols_by(var = "ARM") %>%
  split_rows_by("SEX", labels_var = "lbl_sex", split_fun = drop_split_levels, child_labels = "hidden") %>%
  summarize_row_groups(cfun = function(df, labelstr, .N_col, ...) {
    in_rows(
      rcell(nrow(df) * c(1, 1 / .N_col), format = "xx (xx.xx%)"),
      .labels = paste0(labelstr, ": count (perc.)")
    )
  }) %>%
  analyze("AGE", s_summary) %>%
  analyze("BEP01FL", s_summary, nested = FALSE, show_labels = "visible")

tbl9 <- build_table(lyt9, filter(ADSL_NA, SEX %in% c("M", "F")))
tbl9

## -----------------------------------------------------------------------------
adsl_lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  analyze(c("AGE", "SEX"), afun = s_summary)

adsl_lyt

## -----------------------------------------------------------------------------
adsl_tbl <- build_table(adsl_lyt, ADSL)
adsl_tbl

## -----------------------------------------------------------------------------
adsl_f_tbl <- build_table(lyt, ADSL %>% filter(AGE > 18))
adsl_f_tbl

## -----------------------------------------------------------------------------
set.seed(1)

lookup <- tribble(
  ~AEDECOD,                          ~AEBODSYS,                                         ~AETOXGR,
  "HEADACHE",                        "NERVOUS SYSTEM DISORDERS",                        "5",
  "BACK PAIN",                       "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS", "2",
  "GINGIVAL BLEEDING",               "GASTROINTESTINAL DISORDERS",                      "1",
  "HYPOTENSION",                     "VASCULAR DISORDERS",                              "3",
  "FAECES SOFT",                     "GASTROINTESTINAL DISORDERS",                      "2",
  "ABDOMINAL DISCOMFORT",            "GASTROINTESTINAL DISORDERS",                      "1",
  "DIARRHEA",                        "GASTROINTESTINAL DISORDERS",                      "1",
  "ABDOMINAL FULLNESS DUE TO GAS",   "GASTROINTESTINAL DISORDERS",                      "1",
  "NAUSEA (INTERMITTENT)",           "GASTROINTESTINAL DISORDERS",                      "2",
  "WEAKNESS",                        "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS", "3",
  "ORTHOSTATIC HYPOTENSION",         "VASCULAR DISORDERS",                              "4"
)

normalize <- function(x) x / sum(x)
weightsA <- normalize(c(0.1, dlnorm(seq(0, 5, length.out = 25), meanlog = 3)))
weightsB <- normalize(c(0.2, dlnorm(seq(0, 5, length.out = 25))))

N_pop <- 300
ADSL2 <- data.frame(
  USUBJID = seq(1, N_pop, by = 1),
  ARM = sample(c("ARM A", "ARM B"), N_pop, TRUE),
  SEX = sample(c("F", "M"), N_pop, TRUE),
  AGE = 20 + rbinom(N_pop, size = 40, prob = 0.7)
)

l.adae <- mapply(
  ADSL2$USUBJID,
  ADSL2$ARM,
  ADSL2$SEX,
  ADSL2$AGE,
  FUN = function(id, arm, sex, age) {
    n_ae <- sample(0:25, 1, prob = if (arm == "ARM A") weightsA else weightsB)
    i <- sample(seq_len(nrow(lookup)), size = n_ae, replace = TRUE, prob = c(6, rep(1, 10)) / 16)
    lookup[i, ] %>%
      mutate(
        AESEQ = seq_len(n()),
        USUBJID = id, ARM = arm, SEX = sex, AGE = age
      )
  },
  SIMPLIFY = FALSE
)

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
      rcell(length(unique(x)) * c(1, 1 / .N_col), format = "xx (xx.xx%)"),
    "Total number of events" = rcell(length(x), format = "xx")
  )
}

## -----------------------------------------------------------------------------
s_events_patients(x = c("id 1", "id 1", "id 2"), .N_col = 5)

## -----------------------------------------------------------------------------
adae_lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  analyze("USUBJID", s_events_patients)

adae_tbl <- build_table(adae_lyt, ADAE2)
adae_tbl

## -----------------------------------------------------------------------------
adae_adsl_tbl <- build_table(adae_lyt, ADAE2, alt_counts_df = ADSL2)
adae_adsl_tbl

## -----------------------------------------------------------------------------
adae_soc_lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  analyze("USUBJID", s_events_patients) %>%
  split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE) %>%
  summarize_row_groups("USUBJID", cfun = s_events_patients)

adae_soc_tbl <- build_table(adae_soc_lyt, ADAE2, alt_counts_df = ADSL2)
adae_soc_tbl

## -----------------------------------------------------------------------------
adae_soc_lyt2 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  split_rows_by("AEBODSYS", child_labels = "visible", indent_mod = 1) %>%
  summarize_row_groups("USUBJID", cfun = s_events_patients) %>%
  analyze("AEDECOD", indent_mod = -1)

adae_soc_tbl2 <- build_table(adae_soc_lyt2, ADAE2, alt_counts_df = ADSL2)
adae_soc_tbl2

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
adae_soc_lyt3 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  split_rows_by("AEBODSYS", child_labels = "visible", indent_mod = 1) %>%
  summarize_row_groups("USUBJID", cfun = s_events_patients) %>%
  analyze("AEDECOD", afun = table_count_once_per_id, show_labels = "hidden", indent_mod = -1)

adae_soc_tbl3 <- build_table(adae_soc_lyt3, ADAE2, alt_counts_df = ADSL2)
adae_soc_tbl3

## -----------------------------------------------------------------------------
adae_soc_lyt4 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  analyze("USUBJID", afun = s_events_patients) %>%
  split_rows_by("AEBODSYS", child_labels = "visible", indent_mod = 1, section_div = "") %>%
  summarize_row_groups("USUBJID", cfun = s_events_patients) %>%
  analyze("AEDECOD", table_count_once_per_id, show_labels = "hidden", indent_mod = -1)

adae_soc_tbl4 <- build_table(adae_soc_lyt4, ADAE2, alt_counts_df = ADSL2)
adae_soc_tbl4

## -----------------------------------------------------------------------------
trim_rows(adae_soc_tbl4)

## -----------------------------------------------------------------------------
table_count_grade_once_per_id <- function(df,
                                          labelstr = "",
                                          gradevar = "AETOXGR",
                                          idvar = "USUBJID",
                                          grade_levels = NULL) {
  id <- df[[idvar]]
  grade <- df[[gradevar]]

  if (!is.null(grade_levels)) {
    stopifnot(all(grade %in% grade_levels))
    grade <- factor(grade, levels = grade_levels)
  }

  id_sel <- !duplicated(id)

  in_rows(
    "--Any Grade--" = sum(id_sel),
    .list = as.list(table(grade[id_sel]))
  )
}

table_count_grade_once_per_id(ex_adae, grade_levels = 1:5)

## -----------------------------------------------------------------------------
adae_grade_lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  analyze(
    "AETOXGR",
    afun = table_count_grade_once_per_id,
    extra_args = list(grade_levels = 1:5),
    var_labels = "- Any adverse events -",
    show_labels = "visible"
  ) %>%
  split_rows_by("AEBODSYS", child_labels = "visible", indent_mod = 1) %>%
  summarize_row_groups(cfun = table_count_grade_once_per_id, format = "xx", indent_mod = 1) %>%
  split_rows_by("AEDECOD", child_labels = "visible", indent_mod = -2) %>%
  analyze(
    "AETOXGR",
    afun = table_count_grade_once_per_id,
    extra_args = list(grade_levels = 1:5),
    show_labels = "hidden"
  )

adae_grade_tbl <- build_table(adae_grade_lyt, ADAE2, alt_counts_df = ADSL2)
adae_grade_tbl

## -----------------------------------------------------------------------------
ADRS_BESRSPI <- ex_adrs %>%
  filter(PARAMCD == "BESRSPI") %>%
  mutate(
    rsp = factor(AVALC %in% c("CR", "PR"), levels = c(TRUE, FALSE), labels = c("Responders", "Non-Responders")),
    is_rsp = (rsp == "Responders")
  )

s_proportion <- function(x, .N_col) {
  in_rows(
    .list = lapply(
      as.list(table(x)),
      function(xi) rcell(xi * c(1, 1 / .N_col), format = "xx.xx (xx.xx%)")
    )
  )
}

rsp_lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARMCD", ref_group = "ARM A") %>%
  analyze("rsp", s_proportion, show_labels = "hidden")

rsp_tbl <- build_table(rsp_lyt, ADRS_BESRSPI)
rsp_tbl

## -----------------------------------------------------------------------------
s_unstrat_resp <- function(x, .ref_group, .in_ref_col) {
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
    "Difference in Response Rates (%)" = non_ref_rcell(
      (mean(x) - mean(.ref_group)) * 100,
      .in_ref_col,
      format = "xx.xx"
    ),
    "95% CI (Wald, with correction)" = non_ref_rcell(
      fit$conf.int * 100,
      .in_ref_col,
      format = "(xx.xx, xx.xx)"
    ),
    "p-value (Chi-Squared Test)" = non_ref_rcell(
      fit$p.value,
      .in_ref_col,
      format = "x.xxxx | (<0.0001)"
    ),
    "Odds Ratio (95% CI)" = non_ref_rcell(
      c(
        exp(stats::coef(fit_glm)[-1]),
        exp(stats::confint.default(fit_glm, level = .95)[-1, , drop = FALSE])
      ),
      .in_ref_col,
      format = "xx.xx (xx.xx - xx.xx)"
    )
  )
}

s_unstrat_resp(
  x = ADRS_BESRSPI %>% filter(ARM == "A: Drug X") %>% pull(is_rsp),
  .ref_group = ADRS_BESRSPI %>% filter(ARM == "B: Placebo") %>% pull(is_rsp),
  .in_ref_col = FALSE
)

## -----------------------------------------------------------------------------
rsp_lyt2 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARMCD", ref_group = "ARM A") %>%
  analyze("rsp", s_proportion, show_labels = "hidden") %>%
  analyze(
    "is_rsp", s_unstrat_resp,
    show_labels = "visible",
    var_labels = "Unstratified Response Analysis"
  )

rsp_tbl2 <- build_table(rsp_lyt2, ADRS_BESRSPI)
rsp_tbl2

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
rsp_lyt3 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARMCD", ref_group = "ARM A") %>%
  analyze("rsp", s_proportion, show_labels = "hidden") %>%
  analyze(
    "is_rsp", s_unstrat_resp,
    show_labels = "visible", var_labels = "Unstratified Response Analysis"
  ) %>%
  split_rows_by(
    var = "AVALC",
    split_fun = reorder_split_levels(neworder = c("CR", "PR", "SD", "NON CR/PD", "PD", "NE"), drlevels = TRUE),
    nested = FALSE
  ) %>%
  summarize_row_groups() %>%
  analyze("AVALC", afun = s_prop)

rsp_tbl3 <- build_table(rsp_lyt3, ADRS_BESRSPI)
rsp_tbl3

## -----------------------------------------------------------------------------
rsp_label <- function(x) {
  rsp_full_label <- c(
    CR = "Complete Response (CR)",
    PR = "Partial Response (PR)",
    SD = "Stable Disease (SD)",
    `NON CR/PD` = "Non-CR or Non-PD (NON CR/PD)",
    PD = "Progressive Disease (PD)",
    NE = "Not Evaluable (NE)",
    Missing = "Missing",
    `NE/Missing` = "Missing or unevaluable"
  )
  stopifnot(all(x %in% names(rsp_full_label)))
  rsp_full_label[x]
}


rsp_lyt4 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARMCD", ref_group = "ARM A") %>%
  analyze("rsp", s_proportion, show_labels = "hidden") %>%
  analyze(
    "is_rsp", s_unstrat_resp,
    show_labels = "visible", var_labels = "Unstratified Response Analysis"
  ) %>%
  split_rows_by(
    var = "AVALC",
    split_fun = keep_split_levels(c("CR", "PR", "SD", "PD"), reorder = TRUE),
    nested = FALSE
  ) %>%
  summarize_row_groups(cfun = function(df, labelstr, .N_col) {
    in_rows(nrow(df) * c(1, 1 / .N_col), .formats = "xx (xx.xx%)", .labels = rsp_label(labelstr))
  }) %>%
  analyze("AVALC", afun = s_prop) %>%
  analyze("AVALC", afun = function(x, .N_col) {
    in_rows(rcell(sum(x == "NE") * c(1, 1 / .N_col), format = "xx.xx (xx.xx%)"), .labels = rsp_label("NE"))
  }, nested = FALSE)

rsp_tbl4 <- build_table(rsp_lyt4, ADRS_BESRSPI)
rsp_tbl4

## -----------------------------------------------------------------------------
library(survival)

adtte <- ex_adaette %>%
  dplyr::filter(PARAMCD == "AETTE2", SAFFL == "Y")

# Add censoring to data for example
adtte[adtte$AVAL > 1.0, ] <- adtte[adtte$AVAL > 1.0, ] %>% mutate(AVAL = 1.0, CNSR = 1)

adtte2 <- adtte %>%
  mutate(CNSDTDSC = ifelse(CNSDTDSC == "", "__none__", CNSDTDSC))

## -----------------------------------------------------------------------------
a_count_subjs <- function(x, .N_col) {
  in_rows(
    "Subjects with Adverse Events n (%)" = rcell(length(unique(x)) * c(1, 1 / .N_col), format = "xx (xx.xx%)")
  )
}

## -----------------------------------------------------------------------------
cnsr_counter <- function(df, .var, .N_col) {
  x <- df[!duplicated(df$USUBJID), .var]
  x <- x[x != "__none__"]
  lapply(table(x), function(xi) rcell(xi * c(1, 1 / .N_col), format = "xx (xx.xx%)"))
}

## -----------------------------------------------------------------------------
cph <- coxph(Surv(AVAL, CNSR == 0) ~ ACTARM + STRATA1, ties = "exact", data = adtte)

a_cph <- function(df, .var, .in_ref_col, .ref_full, full_cox_fit) {
  if (.in_ref_col) {
    ret <- replicate(3, list(rcell(NULL)))
  } else {
    curtrt <- df[[.var]][1]
    coefs <- coef(full_cox_fit)
    sel_pos <- grep(curtrt, names(coefs), fixed = TRUE)
    hrval <- exp(coefs[sel_pos])
    sdf <- survdiff(Surv(AVAL, CNSR == 0) ~ ACTARM + STRATA1, data = rbind(df, .ref_full))
    pval <- (1 - pchisq(sdf$chisq, length(sdf$n) - 1)) / 2
    ci_val <- exp(unlist(confint(full_cox_fit)[sel_pos, ]))
    ret <- list(
      rcell(hrval, format = "xx.x"),
      rcell(ci_val, format = "(xx.x, xx.x)"),
      rcell(pval, format = "x.xxxx | (<0.0001)")
    )
  }
  in_rows(
    .list = ret,
    .names = c("Hazard ratio", "95% confidence interval", "p-value (one-sided stratified log rank)")
  )
}

## -----------------------------------------------------------------------------
surv_tbl <- as.data.frame(
  summary(survfit(Surv(AVAL, CNSR == 0) ~ ACTARM, data = adtte, conf.type = "log-log"))$table
) %>%
  dplyr::mutate(
    ACTARM = factor(gsub("ACTARM=", "", row.names(.)), levels = levels(adtte$ACTARM)),
    ind = FALSE
  )

a_tte <- function(df, .var, kp_table) {
  ind <- grep(df[[.var]][1], row.names(kp_table), fixed = TRUE)
  minmax <- range(df[["AVAL"]])
  mm_val_str <- format_value(minmax, format = "xx.x, xx.x")
  rowfn <- list()
  if (all(df$CNSR[df$AVAL == minmax[2]])) {
    mm_val_str <- paste0(mm_val_str, "*")
    rowfn <- "* indicates censoring"
  }
  in_rows(
    Median = kp_table[ind, "median", drop = TRUE],
    "95% confidence interval" = unlist(kp_table[ind, c("0.95LCL", "0.95UCL")]),
    "Min Max" = mm_val_str,
    .formats = c("xx.xx", "xx.xx - xx.xx", "xx"),
    .row_footnotes = list(NULL, NULL, rowfn)
  )
}

## -----------------------------------------------------------------------------
lyt <- basic_table(show_colcounts = TRUE) %>%
  ## Column faceting
  split_cols_by("ARM", ref_group = "A: Drug X") %>%
  ## Overall count
  analyze("USUBJID", a_count_subjs, show_labels = "hidden") %>%
  ## Censored subjects summary
  analyze("CNSDTDSC", cnsr_counter, var_labels = "Censored Subjects", show_labels = "visible") %>%
  ## Cox P-H analysis
  analyze("ARM", a_cph, extra_args = list(full_cox_fit = cph), show_labels = "hidden") %>%
  ## Time-to-event analysis
  analyze(
    "ARM", a_tte,
    var_labels = "Time to first adverse event", show_labels = "visible",
    extra_args = list(kp_table = surv_tbl),
    table_names = "kapmeier"
  )

tbl_tte <- build_table(lyt, adtte2)

## -----------------------------------------------------------------------------
fnotes_at_path(
  tbl_tte,
  c("ma_USUBJID_CNSDTDSC_ARM_kapmeier", "kapmeier")
) <- "Product-limit (Kaplan-Meier) estimates."

tbl_tte

