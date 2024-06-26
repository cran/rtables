% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colby_constructors.R
\name{rm_all_colcounts}
\alias{rm_all_colcounts}
\alias{rm_all_colcounts,VTableTree-method}
\alias{rm_all_colcounts,InstantiatedColumnInfo-method}
\alias{rm_all_colcounts,LayoutColTree-method}
\alias{rm_all_colcounts,LayoutColLeaf-method}
\title{Set all column counts at all levels of nesting to NA}
\usage{
rm_all_colcounts(obj)

\S4method{rm_all_colcounts}{VTableTree}(obj)

\S4method{rm_all_colcounts}{InstantiatedColumnInfo}(obj)

\S4method{rm_all_colcounts}{LayoutColTree}(obj)

\S4method{rm_all_colcounts}{LayoutColLeaf}(obj)
}
\arguments{
\item{obj}{(\code{ANY})\cr the object for the accessor to access or modify.}
}
\value{
\code{obj} with all column counts reset to missing
}
\description{
Set all column counts at all levels of nesting to NA
}
\examples{
lyt <- basic_table() \%>\%
  split_cols_by("ARM") \%>\%
  split_cols_by("SEX") \%>\%
  analyze("AGE")
tbl <- build_table(lyt, ex_adsl)

# before
col_counts(tbl)
tbl <- rm_all_colcounts(tbl)
col_counts(tbl)
}
