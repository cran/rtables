% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_split_fun.R
\name{make_split_result}
\alias{make_split_result}
\alias{add_to_split_result}
\title{Construct split result object}
\usage{
make_split_result(
  values,
  datasplit,
  labels,
  extras = NULL,
  subset_exprs = vector("list", length(values))
)

add_to_split_result(
  splres,
  values,
  datasplit,
  labels,
  extras = NULL,
  subset_exprs = NULL
)
}
\arguments{
\item{values}{(\code{character} or \code{list(SplitValue)})\cr the values associated with each facet.}

\item{datasplit}{(\code{list(data.frame)})\cr the facet data for each facet generated in the split.}

\item{labels}{(\code{character})\cr the labels associated with each facet.}

\item{extras}{(\code{list} or \code{NULL})\cr extra values associated with each of the facets which will be passed to
analysis functions applied within the facet.}

\item{subset_exprs}{(\code{list})\cr A list of subsetting expressions (e.g.,
created with \code{quote()}) to be used during column subsetting.}

\item{splres}{(\code{list})\cr a list representing the result of splitting.}
}
\value{
A named list representing the facets generated by the split with elements \code{values}, \code{datasplit}, and
\code{labels}, which are the same length and correspond to each other element-wise.
}
\description{
These functions can be used to create or add to a split result in functions which implement core splitting or
post-processing within a custom split function.
}
\details{
These functions performs various housekeeping tasks to ensure that the split result list is as the rtables
internals expect it, most of which are not relevant to end users.
}
\examples{
splres <- make_split_result(
  values = c("hi", "lo"),
  datasplit = list(hi = mtcars, lo = mtcars[1:10, ]),
  labels = c("more data", "less data"),
  subset_exprs = list(expression(TRUE), expression(seq_along(wt) <= 10))
)

splres2 <- add_to_split_result(splres,
  values = "med",
  datasplit = list(med = mtcars[1:20, ]),
  labels = "kinda some data",
  subset_exprs = quote(seq_along(wt) <= 20)
)

}
\seealso{
Other make_custom_split: 
\code{\link{add_combo_facet}()},
\code{\link{drop_facet_levels}()},
\code{\link{make_split_fun}()},
\code{\link{trim_levels_in_facets}()}

Other make_custom_split: 
\code{\link{add_combo_facet}()},
\code{\link{drop_facet_levels}()},
\code{\link{make_split_fun}()},
\code{\link{trim_levels_in_facets}()}
}
\concept{make_custom_split}
