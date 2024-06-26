context("Exporters")

test_that("export_as_txt works with and without pagination", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze(c("AGE", "BMRKR2", "COUNTRY"))

  tbl <- build_table(lyt, ex_adsl)

  tmptxtf <- tempfile()
  export_as_txt(tbl, file = tmptxtf, paginate = TRUE, lpp = 8, verbose = TRUE)
  txtlns <- readLines(tmptxtf)
  expect_identical(
    grep("\\\\s\\\\n", txtlns),
    c(9L, 17L)
  )

  expect_identical(
    toString(tbl),
    export_as_txt(tbl, file = NULL, paginate = FALSE)
  )
})

test_that("export_as_txt works with wrapping", {
  clw <- c(5, 7, 6, 6) + 12
  lpp_tmp <- 18

  ## no vert pagination because lpp is so big
  tmptxtf1 <- tempfile()
  export_as_txt(tt_for_wrap,
    file = tmptxtf1,
    paginate = TRUE,
    lpp = 150,
    colwidths = clw,
    tf_wrap = TRUE,
    max_width = 20, cpp = 80, verbose = TRUE
  )
  txtlns1 <- readLines(tmptxtf1)
  pagepos1 <- grep("\\\\s\\\\n", txtlns1)
  expect_identical(pagepos1, 30L) ## c(30L, 58L))

  ## explicitly no vertical pagination (lpp = NULL)
  tmptxtf1b <- tempfile()
  export_as_txt(tt_for_wrap,
    file = tmptxtf1b,
    paginate = TRUE,
    lpp = NULL,
    colwidths = clw,
    tf_wrap = TRUE,
    max_width = 20, cpp = 80
  )

  txtlns1b <- readLines(tmptxtf1b)
  expect_identical(txtlns1, txtlns1b)




  ## no horiz pagination, tf_wrap FALSE

  tmptxtf2 <- tempfile()
  expect_warning(export_as_txt(tt_for_wrap,
    file = tmptxtf2,
    paginate = TRUE,
    lpp = lpp_tmp,
    colwidths = clw,
    tf_wrap = FALSE,
    max_width = 20, verbose = TRUE
  ))
  txtlns2 <- readLines(tmptxtf2)
  pagepos2 <- grep("\\\\s\\\\n", txtlns2)
  expect_identical(pagepos2, 18L) ## c(26L, 50L))

  tmptxtf2b <- tempfile()
  expect_error(export_as_txt(tt_for_wrap,
    file = tmptxtf2b,
    paginate = TRUE,
    lpp = lpp_tmp,
    colwidths = clw,
    tf_wrap = TRUE,
    max_width = 20, verbose = TRUE
  ))
  export_as_txt(tt_for_wrap,
    file = tmptxtf2b,
    paginate = TRUE,
    lpp = lpp_tmp,
    colwidths = clw,
    tf_wrap = TRUE,
    max_width = 40, verbose = TRUE
  )
  txtlns2b <- readLines(tmptxtf2b)
  pagepos2b <- grep("\\\\s\\\\n", txtlns2b)
  expect_identical(pagepos2b, c(16L, 33L, 49L)) ## 16 because we dont' get our first pick of pagination spots anymore

  ## both vertical and horizontal pagination #458
  tmptxtf3 <- tempfile()
  ## this fails, no valid pagination after both heade rand footer
  ## are wrapped to 20
  expect_error(export_as_txt(tt_for_wrap,
    file = tmptxtf3,
    paginate = TRUE,
    lpp = lpp_tmp,
    colwidths = clw,
    tf_wrap = TRUE,
    max_width = 20,
    cpp = 80
  ))
  export_as_txt(tt_for_wrap,
    file = tmptxtf3,
    paginate = TRUE,
    lpp = lpp_tmp,
    colwidths = clw,
    tf_wrap = TRUE,
    max_width = 40,
    cpp = 80, verbose = TRUE
  )

  txtlns3 <- readLines(tmptxtf3)
  pagepos3 <- grep("\\\\s\\\\n", txtlns3)
  expect_identical(pagepos3[1], pagepos2b[1])
})

test_that("tsv roundtripping for path_enriched_df", {
  tbl2 <- tt_to_export()

  df <- path_enriched_df(tbl2)

  tmptsv <- tempfile(fileext = ".tsv")

  export_as_tsv(tbl2, file = tmptsv)

  newdf <- import_from_tsv(tmptsv)

  expect_true(all(sapply(newdf, is.list)))
  expect_equal(
    unclass(newdf[1, 2]), # AsIs "class"
    list(as.character(c(
      16,
      16 / sum(ex_adsl$ARM == "A: Drug X" & ex_adsl$SEX == "M")
    )))
  )
})

test_that("export_as_pdf works", {
  tbl <- tt_to_export()
  tmpf <- tempfile(fileext = ".pdf")

  expect_warning(
    export_as_pdf(tbl, file = tmpf, landscape = TRUE, height = 1000, width = 3, paginate = FALSE),
    "width of page 1 exceeds the available space"
  )
  expect_true(file.exists(tmpf))
  file.remove(tmpf)
  expect_warning(
    export_as_pdf(tbl, file = tmpf, height = 3, width = 1000, paginate = FALSE),
    "height of page 1 exceeds the available space"
  )

  res <- export_as_pdf(tbl, file = tmpf)
  expect_equal(res$npages, 3)

  ## non-monospace fonts work
  ## this tests the actual pagination behavior...
  fspec <- font_spec("Times", 20, 1.2)
  file.remove(tmpf)
  expect_error(export_as_pdf(tbl, file = tmpf, fontspec = fspec), "non-monospace")
  file.remove(tmpf) ## blank file created (currently, this could be better)
  res <- export_as_pdf(tbl, file = tmpf, fontspec = fspec, ttype_ok = TRUE)
})


# test_that("exporting pdfs gives the correct values", {
#     if (check_pdf) {
#         lyt <- basic_table(title = " ") %>%
#             split_rows_by("SEX", page_by = TRUE) %>%
#             analyze("AGE")
#
#         # Building the table
#         tbl <- build_table(lyt, DM)
#
#         tmpf <- tempfile(fileext = ".pdf")
#         res <- export_as_pdf(tbl, file = tmpf, hsep = "=", lpp = 20)
#         res_pdf <- pdf_text(tmpf)
#
#         # Removing spaces and replacing separators
#         res_pdf <- gsub(res_pdf, pattern = "==*", replacement = "+++")
#         res_pdf <- gsub(res_pdf, pattern = "  +", replacement = " ")
#         res_pdf <- gsub(res_pdf, pattern = " \n", replacement = "")
#
#         # Pagination is present as vector in pdf_text. Doing the same with tbl
#         expected <- sapply(paginate_table(tbl), function(x) toString(x, hsep = "="), USE.NAMES = FALSE)
#         names(expected) <- NULL
#
#         # Removing spaces and replacing separators
#         expected <- gsub(expected, pattern = "==*", replacement = "+++")
#         expected <- gsub(expected, pattern = "  +", replacement = " ")
#         expected <- gsub(expected, pattern = " \n", replacement = "\n")
#         expected <- gsub(expected, pattern = "^\n", replacement = "")
#         expect_identical(res_pdf, expected)
#         ## TODO understand better how to compare exactly these outputs
#     }
# })

test_that("exporting pdf does the inset", {
  tbl <- tt_to_export()
  table_inset(tbl) <- 100
  tmpf <- tempfile(fileext = ".pdf")

  expect_error(export_as_pdf(tbl, file = tmpf), "Width of row labels equal to or larger than")
})


test_that("as_html smoke test", {
  tmpf <- tempfile(fileext = ".html")

  tbl <- tt_to_export()
  oldo <- options(viewer = identity)
  expect_silent(fl <- Viewer(tbl))
  xml2::read_html(fl)
  expect_true(TRUE)
  options(oldo)
})

test_that("as_html Viewer with newline test", {
  tmpf <- tempfile(fileext = ".html")

  colfuns <- list(
    function(x) rcell(mean(x), format = "xx.x"),
    function(x) rcell(sd(x), format = "xx.x")
  )
  varlabs <- c("Mean Age", "SD\nLine Break!!! \nAge")

  lyt <- basic_table() %>%
    split_cols_by_multivar(c("AGE", "AGE"), varlabels = varlabs) %>%
    analyze_colvars(afun = colfuns)

  tbl_wrapping <- build_table(lyt, DM)

  tbl_normal <- rtable(
    header = c("Treatement\nN=100", "Comparison\nN=300"),
    format = "xx (xx.xx%)",
    rrow("A", c(104, .2), c(100, .4)),
    rrow("B", c(23, .4), c(43, .5)),
    rrow(),
    rrow("this is a very long section header"),
    rrow("estimate", rcell(55.23, "xx.xx", colspan = 2)),
    rrow("95% CI", indent = 1, rcell(c(44.8, 67.4), format = "(xx.x, xx.x)", colspan = 2))
  )
  oldo <- options(viewer = identity)
  expect_silent(fl <- Viewer(tbl_wrapping))
  expect_silent(fl <- Viewer(tbl_normal))
  xml2::read_html(fl)
  expect_true(TRUE)
  options(oldo)
})

test_that("as_html does not trim whitespace", {
  tbl <- rtable(
    header = LETTERS[1:3],
    format = "xx",
    rrow("  r1", 1, 2, 3),
    rrow(" r 2  ", 4, 3, 2, indent = 1),
    rrow("r3   ", indent = 2)
  )
  html_tbl <- as_html(tbl)
  html_parts <- html_tbl$children[[1]][[2]]$children
  expect_true(all(sapply(1:4, function(x) "white-space: pre;" %in% html_parts[[x]]$attribs)))
})

test_that("as_html bolding works", {
  tbl <- rtable(
    header = LETTERS[1:3],
    format = "xx",
    rrow("  r1", 1, 2, 3),
    rrow(" r 2  ", 4, 3, 2, indent = 1),
    rrow("r3   ", indent = 2)
  )
  html_tbl <- as_html(tbl, bold = "row_names")
  html_parts <- html_tbl$children[[1]][[2]]$children
  expect_true(all(sapply(2:4, function(x) "font-weight: bold;" %in% html_parts[[x]]$children[[1]][[1]]$attribs)))
})

test_that("as_html header line works", {
  tbl <- rtable(
    header = LETTERS[1:3],
    format = "xx",
    rrow("  r1", 1, 2, 3),
    rrow(" r 2  ", 4, 3, 2, indent = 1),
    rrow("r3   ", indent = 2)
  )
  html_tbl <- as_html(tbl, header_sep_line = TRUE)
  html_parts <- html_tbl$children[[1]][[2]]$children[[1]]$children[[1]]
  expect_true(all(sapply(1:4, function(x) "border-bottom: 1px solid black;" %in% html_parts[[x]]$attribs)))
})

# https://github.com/insightsengineering/rtables/issues/872
test_that("as_html indentation is translated to rows with linebreaks", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX") %>%
    analyze("AGE", afun = function(x) {
      mn <- round(mean(x), 2)
      if (!is.nan(mn) && mn > mean(DM$AGE)) {
        val <- paste(mn, "  ^  ", sep = "\n")
      } else {
        val <- paste(mn)
      }
      in_rows(my_row_label = rcell(val,
        format = "xx"
      ))
    })
  tbl <- build_table(lyt, DM)

  # Resolves correctly \n
  expect_silent(res <- as_html(tbl))
  expect_equal(
    as.character(res$children[[1]][[2]]$children[[7]]$children[[1]][[1]]),
    '<td style="text-align: left; padding-left: 3ch;"></td>'
  )
  expect_equal(
    as.character(res$children[[1]][[2]]$children[[7]]$children[[1]][[2]]),
    '<td style="text-align: center;">  ^  </td>'
  )
})

## https://github.com/insightsengineering/rtables/issues/308
test_that("path_enriched_df works for tables with a column that has all length 1 elements", {
  my_table <- basic_table() %>%
    split_rows_by("Species") %>%
    analyze("Petal.Length") %>%
    build_table(df = iris)
  mydf <- path_enriched_df(my_table)
  expect_identical(dim(mydf), c(3L, 2L))
})

test_that("export_as_rtf works", {
  testthat::skip_if_not_installed("r2rtf")
  tbl <- tt_to_export()
  tmpf <- tempfile(fileext = ".rtf")

  expect_error(
    export_as_rtf(tbl, file = tmpf, landscape = TRUE, margins = c(2, 2, 2, 2), colwidths = 2),
    "non-null colwidths argument"
  )

  res <- export_as_rtf(tbl, file = tmpf)
  expect_true(file.exists(tmpf))
})

# Flextable and docx support ---------------------------------------------------
test_that("Can create flextable object that works with different styles", {
  analysisfun <- function(x, ...) {
    in_rows(
      row1 = 5,
      row2 = c(1, 2),
      .row_footnotes = list(row1 = "row 1 - row footnote"),
      .cell_footnotes = list(row2 = "row 2 - cell footnote")
    )
  }

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX", split_fun = keep_split_levels(c("M", "F"))) %>%
    split_rows_by("STRATA1") %>%
    summarize_row_groups() %>%
    split_rows_by("RACE", split_fun = keep_split_levels(c("WHITE", "ASIAN"))) %>%
    analyze("AGE", afun = analysisfun)


  tbl <- build_table(lyt, ex_adsl)
  ft <- tt_to_flextable(tbl, total_width = 20)
  expect_equal(sum(unlist(nrow(ft))), 20)

  ft2 <- tt_to_flextable(tbl, paginate = TRUE, lpp = 20, verbose = TRUE)
  expect_equal(length(ft2), 2)

  expect_silent(ft3 <- tt_to_flextable(tbl, theme = NULL))

  # Custom theme
  special_bold <- list(
    "header" = list("i" = c(1, 2), "j" = c(1, 3)),
    "body" = list("i" = c(1, 2), "j" = 1)
  )
  custom_theme <- theme_docx_default(tbl,
    font_size = 10,
    font = "Brush Script MT",
    border = officer::fp_border(color = "pink", width = 2),
    bold = NULL,
    bold_manual = special_bold
  )
  expect_silent(tt_to_flextable(tbl, theme = custom_theme))

  # Custom theme error
  special_bold <- list(
    "header" = list("asdai" = c(1, 2), "j" = c(1, 3)),
    "body" = list("i" = c(1, 2), "j" = 1)
  )
  custom_theme <- theme_docx_default(tbl,
    font_size = 10,
    font = "Brush Script MT",
    bold = NULL,
    bold_manual = special_bold
  )
  expect_error(tt_to_flextable(tbl, theme = custom_theme), regexp = "header")


  # header colcounts not in a newline works
  topleft_t1 <- topleft_t2 <- basic_table(show_colcounts = TRUE) %>%
    split_rows_by("ARM", label_pos = "topleft") %>%
    split_cols_by("STRATA1")

  topleft_t1 <- topleft_t1 %>%
    analyze("BMRKR1") %>%
    build_table(DM)
  topleft_t1a <- tt_to_flextable(topleft_t1, counts_in_newline = FALSE)
  topleft_t1b <- tt_to_flextable(topleft_t1, counts_in_newline = TRUE)

  topleft_t2 <- topleft_t2 %>%
    split_rows_by("SEX", label_pos = "topleft") %>%
    analyze("BMRKR1") %>%
    build_table(DM) %>%
    tt_to_flextable(counts_in_newline = FALSE)

  expect_equal(flextable::nrow_part(topleft_t2, part = "header"), 2L)
  expect_equal(flextable::nrow_part(topleft_t1a, part = "header"), 1L)
  expect_equal(flextable::nrow_part(topleft_t1b, part = "header"), 2L)


  # internal package check
  not_a_pkg <- "bwrereloakdosirabttjtaeerr"
  expect_error(check_required_packages(c("flextable", not_a_pkg)), not_a_pkg)
})

test_that("export_as_doc works thanks to tt_to_flextable", {
  lyt <- make_big_lyt()
  tbl <- build_table(lyt, rawdat)
  top_left(tbl) <- "Ethnicity"
  main_title(tbl) <- "Main title"
  subtitles(tbl) <- c("Some Many", "Subtitles")
  main_footer(tbl) <- c("Some Footer", "Mehr")
  prov_footer(tbl) <- "Some prov Footer"
  fnotes_at_path(tbl, rowpath = c("RACE", "BLACK")) <- "factor 2"
  fnotes_at_path(tbl,
    rowpath = c("RACE", "BLACK"),
    colpath = c("ARM", "ARM1", "SEX", "F")
  ) <- "factor 3"

  # Get the flextable
  flex_tbl <- tt_to_flextable(tbl, titles_as_header = TRUE, footers_as_text = FALSE)

  doc_file <- tempfile(fileext = ".docx")

  expect_silent(export_as_docx(tbl,
    file = doc_file, doc_metadata = list("title" = "meh"),
    template_file = doc_file,
    section_properties = section_properties_portrait()
  ))
  # flx table in input
  expect_silent(export_as_docx(flex_tbl,
    file = doc_file, doc_metadata = list("title" = "meh"),
    template_file = doc_file,
    section_properties = section_properties_portrait()
  ))
  expect_silent(export_as_docx(tbl,
    file = doc_file, doc_metadata = list("title" = "meh"),
    template_file = doc_file,
    section_properties = section_properties_landscape()
  ))

  expect_true(file.exists(doc_file))
})
