## ----message=FALSE, warning=FALSE---------------------------------------------
library(rtables)
library(dplyr)

## -----------------------------------------------------------------------------
adtte <- ex_adtte

anl <- adtte %>%
  dplyr::filter(PARAMCD == "OS") %>%
  dplyr::filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
  dplyr::filter(RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")) %>%
  dplyr::mutate(RACE = droplevels(RACE)) %>%
  dplyr::mutate(ARM = droplevels(stats::relevel(ARM, "B: Placebo"))) %>%
  dplyr::mutate(EVENT = 1 - CNSR)

## -----------------------------------------------------------------------------
tidy.summary.coxph <- function(x, ...) {
  is(x, "summary.coxph")
  pval <- x$coefficients
  confint <- x$conf.int
  levels <- rownames(pval)
  pval <- tibble::as_tibble(pval)
  confint <- tibble::as_tibble(confint)

  ret <- cbind(pval[, grepl("Pr", names(pval))], confint)
  ret$level <- levels
  ret$n <- x[["n"]]
  ret
}

## -----------------------------------------------------------------------------
h_coxreg_inter_effect <- function(x,
                                  effect,
                                  covar,
                                  mod,
                                  label,
                                  control,
                                  data) {
  if (is.numeric(x)) {
    betas <- stats::coef(mod)
    attrs <- attr(stats::terms(mod), "term.labels")
    term_indices <- grep(pattern = effect, x = attrs[!grepl("strata\\(", attrs)])
    betas <- betas[term_indices]
    betas_var <- diag(stats::vcov(mod))[term_indices]
    betas_cov <- stats::vcov(mod)[term_indices[1], term_indices[2]]
    xval <- stats::median(x)
    effect_index <- !grepl(covar, names(betas))
    coef_hat <- betas[effect_index] + xval * betas[!effect_index]
    coef_se <- sqrt(betas_var[effect_index] + xval^2 * betas_var[!effect_index] + 2 * xval * betas_cov)
    q_norm <- stats::qnorm((1 + control$conf_level) / 2)
  } else {
    var_lvl <- paste0(effect, levels(data[[effect]])[-1]) # [-1]: reference level
    giv_lvl <- paste0(covar, levels(data[[covar]]))
    design_mat <- expand.grid(effect = var_lvl, covar = giv_lvl)
    design_mat <- design_mat[order(design_mat$effect, design_mat$covar), ]
    design_mat <- within(data = design_mat, expr = {
      inter <- paste0(effect, ":", covar)
      rev_inter <- paste0(covar, ":", effect)
    })
    split_by_variable <- design_mat$effect
    interaction_names <- paste(design_mat$effect, design_mat$covar, sep = "/")
    mmat <- stats::model.matrix(mod)[1, ]
    mmat[!mmat == 0] <- 0
    design_mat <- apply(X = design_mat, MARGIN = 1, FUN = function(x) {
      mmat[names(mmat) %in% x[-which(names(x) == "covar")]] <- 1
      mmat
    })
    colnames(design_mat) <- interaction_names
    coef <- stats::coef(mod)
    vcov <- stats::vcov(mod)
    betas <- as.matrix(coef)
    coef_hat <- t(design_mat) %*% betas
    dimnames(coef_hat)[2] <- "coef"
    coef_se <- apply(design_mat, 2, function(x) {
      vcov_el <- as.logical(x)
      y <- vcov[vcov_el, vcov_el]
      y <- sum(y)
      y <- sqrt(y)
      y
    })
    q_norm <- stats::qnorm((1 + control$conf_level) / 2)
    y <- cbind(coef_hat, `se(coef)` = coef_se)
    y <- apply(y, 1, function(x) {
      x["hr"] <- exp(x["coef"])
      x["lcl"] <- exp(x["coef"] - q_norm * x["se(coef)"])
      x["ucl"] <- exp(x["coef"] + q_norm * x["se(coef)"])
      x
    })
    y <- t(y)
    y <- by(y, split_by_variable, identity)
    y <- lapply(y, as.matrix)
    attr(y, "details") <- paste0(
      "Estimations of ", effect, " hazard ratio given the level of ", covar, " compared to ",
      effect, " level ", levels(data[[effect]])[1], "."
    )
    xval <- levels(data[[covar]])
  }
  data.frame(
    effect = "Covariate:",
    term = rep(covar, length(xval)),
    term_label = as.character(paste0("  ", xval)),
    level = as.character(xval),
    n = NA,
    hr = if (is.numeric(x)) exp(coef_hat) else y[[1]][, "hr"],
    lcl = if (is.numeric(x)) exp(coef_hat - q_norm * coef_se) else y[[1]][, "lcl"],
    ucl = if (is.numeric(x)) exp(coef_hat + q_norm * coef_se) else y[[1]][, "ucl"],
    pval = NA,
    pval_inter = NA,
    stringsAsFactors = FALSE
  )
}

## -----------------------------------------------------------------------------
h_coxreg_extract_interaction <- function(effect, covar, mod, data) {
  control <- list(pval_method = "wald", ties = "exact", conf_level = 0.95, interaction = FALSE)
  test_statistic <- c(wald = "Wald", likelihood = "LR")[control$pval_method]
  mod_aov <- withCallingHandlers(
    expr = car::Anova(mod, test.statistic = test_statistic, type = "III"),
    message = function(m) invokeRestart("muffleMessage")
  )
  msum <- if (!any(attr(stats::terms(mod), "order") == 2)) summary(mod, conf.int = control$conf_level) else mod_aov
  sum_anova <- broom::tidy(msum)
  if (!any(attr(stats::terms(mod), "order") == 2)) {
    effect_aov <- mod_aov[effect, , drop = TRUE]
    pval <- effect_aov[[grep(pattern = "Pr", x = names(effect_aov)), drop = TRUE]]
    sum_main <- sum_anova[grepl(effect, sum_anova$level), ]
    term_label <- if (effect == covar) {
      paste0(levels(data[[covar]])[2], " vs control (", levels(data[[covar]])[1], ")")
    } else {
      unname(formatters::var_labels(data, fill = TRUE)[[covar]])
    }
    y <- data.frame(
      effect = ifelse(covar == effect, "Treatment:", "Covariate:"),
      term = covar, term_label = term_label,
      level = levels(data[[effect]])[2],
      n = mod[["n"]], hr = unname(sum_main["exp(coef)"]), lcl = unname(sum_main[grep("lower", names(sum_main))]),
      ucl = unname(sum_main[grep("upper", names(sum_main))]), pval = pval,
      stringsAsFactors = FALSE
    )
    y$pval_inter <- NA
    y
  } else {
    pval <- sum_anova[sum_anova$term == effect, ][["p.value"]]

    ## Test the interaction effect
    pval_inter <- sum_anova[grep(":", sum_anova$term), ][["p.value"]]
    covar_test <- data.frame(
      effect = "Covariate:",
      term = covar, term_label = unname(formatters::var_labels(data, fill = TRUE)[[covar]]),
      level = "",
      n = mod$n, hr = NA, lcl = NA, ucl = NA, pval = pval,
      pval_inter = pval_inter,
      stringsAsFactors = FALSE
    )
    ## Estimate the interaction
    y <- h_coxreg_inter_effect(
      data[[covar]],
      covar = covar,
      effect = effect,
      mod = mod,
      label = unname(formatters::var_labels(data, fill = TRUE)[[covar]]),
      control = control,
      data = data
    )
    rbind(covar_test, y)
  }
}

## -----------------------------------------------------------------------------
cached_model <- function(df, cov, cache_env) {
  ## Check if a model already exists for
  ## `cov` in the caching environment
  if (!is.null(cache_env[[cov]])) {
    ## If model already exists, retrieve it from cache_env
    model <- cache_env[[cov]]
  } else {
    ## Build model formula
    model_form <- paste0("survival::Surv(AVAL, EVENT) ~ ARM")
    if (length(cov) > 0) {
      model_form <- paste(c(model_form, cov), collapse = " * ")
    } else {
      cov <- "ARM"
    }
    ## Calculate Cox regression model
    model <- survival::coxph(
      formula = stats::as.formula(model_form),
      data = df,
      ties = "exact"
    )
    ## Store model in the caching environment
    cache_env[[cov]] <- model
  }
  model
}

## -----------------------------------------------------------------------------
a_cox_summary <- function(df,
                          labelstr = "",
                          .spl_context,
                          stat,
                          format,
                          cache_env,
                          cov_main = FALSE) {
  ## Get current covariate (variable used in latest row split)
  cov <- tail(.spl_context$value, 1)

  ## If currently analyzing treatment effect (ARM) replace empty
  ## value of cov with "ARM" so the correct model row is analyzed
  if (length(cov) == 0) cov <- "ARM"

  ## Use cached_model to get the fitted Cox regression
  ## model for the current covariate
  model <- cached_model(df = df, cov = cov, cache_env = cache_env)

  ## Extract levels of cov to be used as row labels for interaction effects.
  ## If cov is numeric, the median value of cov is used as a row label instead
  cov_lvls <- if (is.factor(df[[cov]])) levels(df[[cov]]) else as.character(median(df[[cov]]))

  ## Use function to calculate and extract information relevant to cov from the model
  cov_rows <- h_coxreg_extract_interaction(effect = "ARM", covar = cov, mod = model, data = df)
  ## Effect p-value is only printed for treatment effect row
  if (!cov == "ARM") cov_rows[, "pval"] <- NA_real_
  ## Extract rows containing statistics for cov from model information
  if (!cov_main) {
    ## Extract rows for main effect
    cov_rows <- cov_rows[cov_rows$level %in% cov_lvls, ]
  } else {
    ## Extract all non-main effect rows
    cov_rows <- cov_rows[nchar(cov_rows$level) == 0, ]
  }
  ## Extract value(s) of statistic for current column and variable/levels
  stat_vals <- as.list(apply(cov_rows[stat], 1, function(x) x, simplify = FALSE))
  ## Assign labels: covariate name for main effect (content) rows, ARM comparison description
  ## for treatment effect (content) row, cov_lvls for interaction effect (data) rows
  nms <- if (cov_main) labelstr else if (cov == "ARM") cov_rows$term_label else cov_lvls
  ## Return formatted/labelled row
  in_rows(
    .list = stat_vals,
    .names = nms,
    .labels = nms,
    .formats = setNames(rep(format, length(nms)), nms),
    .format_na_strs = setNames(rep("", length(nms)), nms)
  )
}

## -----------------------------------------------------------------------------
my_covs <- c("AGE", "RACE") ## Covariates
my_cov_labs <- c("Age", "Race") ## Covariate labels
my_stats <- list("n", "hr", c("lcl", "ucl"), "pval", "pval_inter") ## Statistics
my_stat_labs <- c("n", "Hazard Ratio", "95% CI", "p-value\n(effect)", "p-value\n(interaction)") ## Statistic labels
my_formats <- c(
  n = "xx", hr = "xx.xx", lcl = "(xx.xx, xx.xx)", pval = "xx.xxxx", pval_inter = "xx.xxxx" ## Statistic formats
)
my_env <- new.env()
ny_cache_env <- replicate(length(my_stats), list(my_env)) ## Caching environment

## -----------------------------------------------------------------------------
lyt <- basic_table() %>%
  ## Column split: one column for each statistic
  split_cols_by_multivar(
    vars = rep("STUDYID", length(my_stats)),
    varlabels = my_stat_labs,
    extra_args = list(
      stat = my_stats,
      format = my_formats,
      cache_env = ny_cache_env
    )
  ) %>%
  ## Create content row for treatment effect
  summarize_row_groups(cfun = a_cox_summary) %>%
  ## Row split: one content row for each covariate
  split_rows_by_multivar(
    vars = my_covs,
    varlabels = my_cov_labs,
    split_label = "Covariate:",
    indent_mod = -1 ## Align split label left
  ) %>%
  ## Create content rows for covariate main effects
  summarize_row_groups(
    cfun = a_cox_summary,
    extra_args = list(cov_main = TRUE)
  ) %>%
  ## Create data rows for covariate interaction effects
  analyze_colvars(afun = a_cox_summary)

## -----------------------------------------------------------------------------
cox_tbl <- build_table(lyt, anl)
cox_tbl

