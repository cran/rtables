## -----------------------------------------------------------------------------
set.seed(0)
levs_type <- c("car", "truck", "suv", "sailboat", "cruiseliner")

vclass <- sample(c("auto", "boat"), 1000, replace = TRUE)
auto_inds <- which(vclass == "auto")
vtype <- rep(NA_character_, 1000)
vtype[auto_inds] <- sample(
  c("car", "truck"), ## suv missing on purpose
  length(auto_inds),
  replace = TRUE
)
vtype[-auto_inds] <- sample(
  c("sailboat", "cruiseliner"),
  1000 - length(auto_inds),
  replace = TRUE
)

vehic_data <- data.frame(
  vehicle_class = factor(vclass),
  vehicle_type = factor(vtype, levels = levs_type),
  color = sample(
    c("white", "black", "red"), 1000,
    prob = c(1, 2, 1),
    replace = TRUE
  ),
  cost = ifelse(
    vclass == "boat",
    rnorm(1000, 100000, sd = 5000),
    rnorm(1000, 40000, sd = 5000)
  )
)
head(vehic_data)

## ----examples, message=FALSE--------------------------------------------------
library(rtables)

lyt <- basic_table() %>%
  split_cols_by("color") %>%
  split_rows_by("vehicle_class") %>%
  split_rows_by("vehicle_type") %>%
  analyze("cost")

build_table(lyt, vehic_data)

## -----------------------------------------------------------------------------
lyt2 <- basic_table() %>%
  split_cols_by("color") %>%
  split_rows_by("vehicle_class", split_fun = trim_levels_in_group("vehicle_type")) %>%
  split_rows_by("vehicle_type") %>%
  analyze("cost")

build_table(lyt2, vehic_data)

## ---- message = FALSE---------------------------------------------------------
library(tibble)
map <- tribble(
  ~vehicle_class, ~vehicle_type,
  "auto",         "truck",
  "auto",         "suv",
  "auto",         "car",
  "boat",         "sailboat",
  "boat",         "cruiseliner"
)

lyt3 <- basic_table() %>%
  split_cols_by("color") %>%
  split_rows_by("vehicle_class", split_fun = trim_levels_to_map(map)) %>%
  split_rows_by("vehicle_type") %>%
  analyze("cost")

build_table(lyt3, vehic_data)

## -----------------------------------------------------------------------------
lyt4 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("color", split_fun = add_overall_level("allcolors", label = "All Colors")) %>%
  split_rows_by("vehicle_class", split_fun = trim_levels_to_map(map)) %>%
  split_rows_by("vehicle_type") %>%
  analyze("cost")

build_table(lyt4, vehic_data)

## -----------------------------------------------------------------------------
combodf <- tribble(
  ~valname, ~label, ~levelcombo, ~exargs,
  "non-white", "Non-White", c("black", "red"), list(),
  "blackwhite", "Black or White", c("black", "white"), list()
)


lyt5 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("color", split_fun = add_combo_levels(combodf)) %>%
  split_rows_by("vehicle_class", split_fun = trim_levels_to_map(map)) %>%
  split_rows_by("vehicle_type") %>%
  analyze("cost")

build_table(lyt5, vehic_data)

## -----------------------------------------------------------------------------
## reverse order of levels

rev_lev <- function(df, spl, vals, labels, ...) {
  ## in the split_rows_by() and split_cols_by() cases,
  ## spl_variable() gives us the variable
  var <- spl_variable(spl)
  vec <- df[[var]]
  levs <- if (is.character(vec)) unique(vec) else levels(vec)
  df[[var]] <- factor(vec, levels = rev(levs))
  df
}

rem_lev_facet <- function(torem) {
  function(df, spl, vals, labels, ...) {
    var <- spl_variable(spl)
    vec <- df[[var]]
    bad <- vec == torem
    df <- df[!bad, ]
    levs <- if (is.character(vec)) unique(vec) else levels(vec)
    df[[var]] <- factor(as.character(vec[!bad]), levels = setdiff(levs, torem))
    df
  }
}

## -----------------------------------------------------------------------------
sort_them_facets <- function(splret, spl, fulldf, ...) {
  ord <- order(sapply(splret$datasplit, nrow))
  make_split_result(
    splret$values[ord],
    splret$datasplit[ord],
    splret$labels[ord]
  )
}

## -----------------------------------------------------------------------------
silly_splfun1 <- make_split_fun(
  pre = list(
    rev_lev,
    rem_lev_facet("white")
  ),
  post = list(sort_them_facets)
)

lyt6 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("color", split_fun = silly_splfun1) %>%
  split_rows_by("vehicle_class", split_fun = trim_levels_to_map(map)) %>%
  split_rows_by("vehicle_type") %>%
  analyze("cost")

build_table(lyt6, vehic_data)

## -----------------------------------------------------------------------------
silly_core_split <- function(spl, df, vals, labels, .spl_context) {
  make_split_result(
    c("first", "lowmid", "highmid", "last"),
    datasplit = list(
      df[1:100, ],
      df[101:500, ],
      df[501:900, ],
      df[901:1000, ]
    ),
    labels = c(
      "first 100",
      "obs 101-500",
      "obs 501-900",
      "last 100"
    )
  )
}

## -----------------------------------------------------------------------------
even_sillier_splfun <- make_split_fun(core_split = silly_core_split)

lyt7 <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("color") %>%
  split_rows_by("vehicle_class", split_fun = even_sillier_splfun) %>%
  split_rows_by("vehicle_type") %>%
  analyze("cost")

build_table(lyt7, vehic_data)

