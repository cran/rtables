% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_funs.R
\name{split_funcs}
\alias{split_funcs}
\alias{remove_split_levels}
\alias{keep_split_levels}
\alias{drop_split_levels}
\alias{drop_and_remove_levels}
\alias{reorder_split_levels}
\alias{trim_levels_in_group}
\title{Split functions}
\usage{
remove_split_levels(excl)

keep_split_levels(only, reorder = TRUE)

drop_split_levels(df, spl, vals = NULL, labels = NULL, trim = FALSE)

drop_and_remove_levels(excl)

reorder_split_levels(neworder, newlabels = neworder, drlevels = TRUE)

trim_levels_in_group(innervar, drop_outlevs = TRUE)
}
\arguments{
\item{excl}{(\code{character})\cr levels to be excluded (they will not be reflected in the resulting table structure
regardless of presence in the data).}

\item{only}{(\code{character})\cr levels to retain (all others will be dropped).}

\item{reorder}{(\code{flag})\cr whether the order of \code{only} should be used as the order of the children of the
split. Defaults to \code{TRUE}.}

\item{df}{(\code{data.frame} or \code{tibble})\cr dataset.}

\item{spl}{(\code{Split})\cr a \code{Split} object defining a partitioning or analysis/tabulation of the data.}

\item{vals}{(\code{ANY})\cr for internal use only.}

\item{labels}{(\code{character})\cr labels to use for the remaining levels instead of the existing ones.}

\item{trim}{(\code{flag})\cr whether splits corresponding with 0 observations should be kept when tabulating.}

\item{neworder}{(\code{character})\cr new order of factor levels.}

\item{newlabels}{(\code{character})\cr labels for (new order of) factor levels.}

\item{drlevels}{(\code{flag})\cr whether levels in the data which do not appear in \code{neworder} should be dropped.
Defaults to \code{TRUE}.}

\item{innervar}{(\code{string})\cr variable whose factor levels should be trimmed (e.g. empty levels dropped)
\emph{separately within each grouping defined at this point in the structure}.}

\item{drop_outlevs}{(\code{flag})\cr whether empty levels in the variable being split on (i.e. the "outer"
variable, not \code{innervar}) should be dropped. Defaults to \code{TRUE}.}
}
\value{
A closure suitable for use as a splitting function (\code{splfun}) when creating a table layout.
}
\description{
Split functions
}
\section{Custom Splitting Function Details}{


User-defined custom split functions can perform any type of computation on the incoming data provided that they
meet the requirements for generating "splits" of the incoming data based on the split object.

Split functions are functions that accept:
\describe{
\item{df}{a \code{data.frame} of incoming data to be split.}
\item{spl}{a Split object. This is largely an internal detail custom functions will not need to worry about,
but \code{obj_name(spl)}, for example, will give the name of the split as it will appear in paths in the resulting
table.}
\item{vals}{any pre-calculated values. If given non-\code{NULL} values, the values returned should match these.
Should be \code{NULL} in most cases and can usually be ignored.}
\item{labels}{any pre-calculated value labels. Same as above for \code{values}.}
\item{trim}{if \code{TRUE}, resulting splits that are empty are removed.}
\item{(optional) .spl_context}{a \code{data.frame} describing previously performed splits which collectively
arrived at \code{df}.}
}

The function must then output a named \code{list} with the following elements:

\describe{
\item{values}{the vector of all values corresponding to the splits of \code{df}.}
\item{datasplit}{a list of \code{data.frame}s representing the groupings of the actual observations from \code{df}.}
\item{labels}{a character vector giving a string label for each value listed in the \code{values} element above.}
\item{(optional) extras}{if present, extra arguments are to be passed to summary and analysis functions
whenever they are executed on the corresponding element of \code{datasplit} or a subset thereof.}
}

One way to generate custom splitting functions is to wrap existing split functions and modify either the incoming
data before they are called or their outputs.
}

\examples{
lyt <- basic_table() \%>\%
  split_cols_by("ARM") \%>\%
  split_rows_by("COUNTRY",
    split_fun = remove_split_levels(c(
      "USA", "CAN",
      "CHE", "BRA"
    ))
  ) \%>\%
  analyze("AGE")

tbl <- build_table(lyt, DM)
tbl

lyt <- basic_table() \%>\%
  split_cols_by("ARM") \%>\%
  split_rows_by("COUNTRY",
    split_fun = keep_split_levels(c("USA", "CAN", "BRA"))
  ) \%>\%
  analyze("AGE")

tbl <- build_table(lyt, DM)
tbl

lyt <- basic_table() \%>\%
  split_cols_by("ARM") \%>\%
  split_rows_by("SEX", split_fun = drop_split_levels) \%>\%
  analyze("AGE")

tbl <- build_table(lyt, DM)
tbl

lyt <- basic_table() \%>\%
  split_cols_by("ARM") \%>\%
  split_rows_by("SEX", split_fun = drop_and_remove_levels(c("M", "U"))) \%>\%
  analyze("AGE")

tbl <- build_table(lyt, DM)
tbl

}
