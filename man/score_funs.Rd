% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tt_sort.R
\name{cont_n_allcols}
\alias{cont_n_allcols}
\alias{cont_n_onecol}
\title{Score functions for sorting \code{TableTrees}}
\usage{
cont_n_allcols(tt)

cont_n_onecol(j)
}
\arguments{
\item{tt}{(\code{TableTree} or related class)\cr a \code{TableTree} object representing a populated table.}

\item{j}{(\code{numeric(1)})\cr index of column used for scoring.}
}
\value{
A single numeric value indicating score according to the relevant metric for \code{tt}, to be used when sorting.
}
\description{
Score functions for sorting \code{TableTrees}
}
\seealso{
For examples and details, please read the documentation for \code{\link[=sort_at_path]{sort_at_path()}} and the
\href{https://insightsengineering.github.io/rtables/latest-tag/articles/sorting_pruning.html}{Sorting and Pruning}
vignette.
}
