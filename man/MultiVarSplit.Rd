% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00tabletrees.R
\name{MultiVarSplit}
\alias{MultiVarSplit}
\title{Split between two or more different variables}
\usage{
MultiVarSplit(
  vars,
  split_label = "",
  varlabels = NULL,
  varnames = NULL,
  cfun = NULL,
  cformat = NULL,
  split_format = NULL,
  split_name = "multivars",
  child_labels = c("default", "visible", "hidden"),
  extra_args = list(),
  indent_mod = 0L,
  cindent_mod = 0L,
  cvar = "",
  cextra_args = list(),
  label_pos = "visible"
)
}
\arguments{
\item{vars}{character vector. Multiple variable names.}

\item{split_label}{string. Label string to be associated with the table generated by the split. Not to be confused with labels assigned to each child (which are based on the data and type of split during tabulation).}

\item{varlabels}{character vector. Labels for \code{vars}}

\item{varnames}{character vector. Names for \code{vars} which will appear in pathing. When \code{vars} are all unique this will be the variable names. If not, these will be variable names with suffixes as necessary to enforce uniqueness.}

\item{cfun}{list/function/NULL. tabulation function(s) for creating content rows. Must accept \code{x} or \code{df} as first parameter. Must accept \code{labelstr} as the second argument. Can optionally accept all optional arguments accepted by analysis functions. See \code{\link{analyze}}.}

\item{cformat}{format spec. Format for content rows}

\item{split_format}{format spec. Format associated with this split.}

\item{split_name}{string. Name associiated with this split (for pathing, etc)}

\item{child_labels}{string. One of \code{"default"}, \code{"visible"}, \code{"hidden"}. What should the display behavior be for the  labels (ie label rows) of the children of this split. Defaults to \code{"default"} which flags the label row as visible only if the child has 0 content rows.}

\item{extra_args}{list. Extra arguments to be passed to the tabulation function. Element position in thte list corresponds to the children of this split. Named elements in the child-specific lists are ignored if they do not match a formal argument of the ttabulation function.}

\item{indent_mod}{numeric. Modifier for the default indent position for the structure created by this function(subtable, content table, or row) \emph{and all of that structure's children}. Defaults to 0, which corresponds to the unmodified default behavior.}

\item{cindent_mod}{numeric(1). The indent modifier for the content tables generated by this split.}

\item{cvar}{character(1). The variable, if any, which the content function should accept. Defaults to NA.}

\item{cextra_args}{list. Extra arguments to be passed to the content function when tabulating row group summaries.}

\item{label_pos}{character(1). Location the variable label should be displayed, Accepts  hidden (default for non-analyze row splits), visible, topleft, and - for analyze splits only - default.  For analyze calls, \code{default} indicates that the variable
should be visible if and only if multiple variables are analyzed at the same level of nesting.}
}
\value{
A \code{MultiVarSplit} object.
}
\description{
Split between two or more different variables
}
\author{
Gabriel Becker
}