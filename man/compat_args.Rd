% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/argument_conventions.R
\name{compat_args}
\alias{compat_args}
\title{Compatability Arg Conventions}
\usage{
compat_args(
  .lst,
  FUN,
  col_by,
  row_by,
  row.name,
  format,
  indent,
  col_wise_args,
  label
)
}
\arguments{
\item{.lst}{list. An already-collected list of arguments tot be used instead of the elements of \code{\dots}. Arguments passed via \code{\dots} will be ignored if this is specified.}

\item{FUN}{function. Tabulation fucntion. Will be passed subsets of \code{x} defined by the combination of \code{col_by} and \code{row_by} and returns corresponding cell value}

\item{col_by}{(\code{\link{factor}} or \code{\link{data.frame}}
if a \code{\link{factor}} of length \code{nrow(x)} that defines
which levels in \code{col_by} define a column.}

\item{row_by}{rows in \code{x} to take per row in the resulting table}

\item{row.name}{if \code{NULL} then the \code{FUN} argument is deparsed and
used as \code{row.name} of the \code{\link{rrow}}}

\item{format}{if \code{FUN} does not return a formatted \code{\link{rcell}}
then the \code{format} is applied}

\item{indent}{deprecated.}

\item{col_wise_args}{a named list containing collections (e.g. vectors or
lists) with data elements for each column of the resulting table. The data
elements are then passed to the named argument \code{FUN} corresponding to
the element name of the outer list. Hence, the length and order of each
collection must match the levels in \code{col_by}. See examples.}

\item{label}{character(1). A label (not to be confused with the name) for the object/structure.}
}
\value{
NULL (this is an argument template dummy function)
}
\description{
Compatability Arg Conventions
}
\seealso{
Other conventions: 
\code{\link{constr_args}()},
\code{\link{gen_args}()},
\code{\link{lyt_args}()},
\code{\link{sf_args}()}
}
\concept{conventions}
