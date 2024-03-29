% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_table_struct.R
\name{validate_table_struct}
\alias{validate_table_struct}
\alias{assert_valid_table}
\title{Validate and Assert valid table structure (Experimental).}
\usage{
validate_table_struct(tt)

assert_valid_table(tt, warn_only = FALSE)
}
\arguments{
\item{tt}{\code{TableTree}

A \code{TableTree} (\code{rtables}-built table) is considered degenerate if
\enumerate{
\item{it contains no subtables or data rows (content rows do not count)}
\item{it contains a subtable which is degenerate by the criterion above}
}

\code{validate_table_struct} assesses whether \code{tt} has a valid (non-degenerate)
structure.

\code{assert_valid_table} asserts a table must have a valid structure,
and throws an informative error (the default) or warning (if \code{warn_only}
is \code{TRUE}) if the table is degenerate (has invalid structure or
contains one or more invalid substructures}

\item{warn_only}{logical(1). Should a warning be thrown instead of an error?
Defaults to \code{FALSE}}
}
\value{
for \code{validate_table_struct} a logical value indicating valid structure;
\code{assert_valid_table} is called for its side-effect of throwing an error
or warning for degenerate tables.
}
\description{
Validate and Assert valid table structure (Experimental).
}
\note{
This function is experimental and the exact text of the warning/error
is subject to change in future releases.
}
\examples{
validate_table_struct(rtable("hahaha"))
\dontrun{
assert_valid_table(rtable("oops"))
}

}
