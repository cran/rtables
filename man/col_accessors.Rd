% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_accessors.R
\name{clayout}
\alias{clayout}
\alias{clayout,VTableNodeInfo-method}
\alias{clayout,PreDataTableLayouts-method}
\alias{clayout,ANY-method}
\alias{clayout<-}
\alias{clayout<-,PreDataTableLayouts-method}
\alias{col_info}
\alias{col_info,VTableNodeInfo-method}
\alias{col_info<-}
\alias{col_info<-,TableRow-method}
\alias{col_info<-,ElementaryTable-method}
\alias{col_info<-,TableTree-method}
\alias{coltree}
\alias{coltree,InstantiatedColumnInfo-method}
\alias{coltree,PreDataTableLayouts-method}
\alias{coltree,PreDataColLayout-method}
\alias{coltree,LayoutColTree-method}
\alias{coltree,VTableTree-method}
\alias{coltree,TableRow-method}
\alias{col_exprs}
\alias{col_exprs,PreDataTableLayouts-method}
\alias{col_exprs,PreDataColLayout-method}
\alias{col_exprs,InstantiatedColumnInfo-method}
\alias{col_counts}
\alias{col_counts,InstantiatedColumnInfo-method}
\alias{col_counts,VTableNodeInfo-method}
\alias{col_counts<-}
\alias{col_counts<-,InstantiatedColumnInfo-method}
\alias{col_counts<-,VTableNodeInfo-method}
\alias{col_total}
\alias{col_total,InstantiatedColumnInfo-method}
\alias{col_total,VTableNodeInfo-method}
\alias{col_total<-}
\alias{col_total<-,InstantiatedColumnInfo-method}
\alias{col_total<-,VTableNodeInfo-method}
\title{Column information/structure accessors}
\usage{
clayout(obj)

\S4method{clayout}{VTableNodeInfo}(obj)

\S4method{clayout}{PreDataTableLayouts}(obj)

\S4method{clayout}{ANY}(obj)

clayout(object) <- value

\S4method{clayout}{PreDataTableLayouts}(object) <- value

col_info(obj)

\S4method{col_info}{VTableNodeInfo}(obj)

col_info(obj) <- value

\S4method{col_info}{TableRow}(obj) <- value

\S4method{col_info}{ElementaryTable}(obj) <- value

\S4method{col_info}{TableTree}(obj) <- value

coltree(
  obj,
  df = NULL,
  rtpos = TreePos(),
  alt_counts_df = df,
  ccount_format = "(N=xx)"
)

\S4method{coltree}{InstantiatedColumnInfo}(
  obj,
  df = NULL,
  rtpos = TreePos(),
  alt_counts_df = df,
  ccount_format = "(N=xx)"
)

\S4method{coltree}{PreDataTableLayouts}(
  obj,
  df = NULL,
  rtpos = TreePos(),
  alt_counts_df = df,
  ccount_format = "(N=xx)"
)

\S4method{coltree}{PreDataColLayout}(
  obj,
  df = NULL,
  rtpos = TreePos(),
  alt_counts_df = df,
  ccount_format = "(N=xx)"
)

\S4method{coltree}{LayoutColTree}(
  obj,
  df = NULL,
  rtpos = TreePos(),
  alt_counts_df = df,
  ccount_format = "(N=xx)"
)

\S4method{coltree}{VTableTree}(
  obj,
  df = NULL,
  rtpos = TreePos(),
  alt_counts_df = df,
  ccount_format = "(N=xx)"
)

\S4method{coltree}{TableRow}(
  obj,
  df = NULL,
  rtpos = TreePos(),
  alt_counts_df = df,
  ccount_format = "(N=xx)"
)

col_exprs(obj, df = NULL)

\S4method{col_exprs}{PreDataTableLayouts}(obj, df = NULL)

\S4method{col_exprs}{PreDataColLayout}(obj, df = NULL)

\S4method{col_exprs}{InstantiatedColumnInfo}(obj, df = NULL)

col_counts(obj, path = NULL)

\S4method{col_counts}{InstantiatedColumnInfo}(obj, path = NULL)

\S4method{col_counts}{VTableNodeInfo}(obj, path = NULL)

col_counts(obj, path = NULL) <- value

\S4method{col_counts}{InstantiatedColumnInfo}(obj, path = NULL) <- value

\S4method{col_counts}{VTableNodeInfo}(obj, path = NULL) <- value

col_total(obj)

\S4method{col_total}{InstantiatedColumnInfo}(obj)

\S4method{col_total}{VTableNodeInfo}(obj)

col_total(obj) <- value

\S4method{col_total}{InstantiatedColumnInfo}(obj) <- value

\S4method{col_total}{VTableNodeInfo}(obj) <- value
}
\arguments{
\item{obj}{(\code{ANY})\cr the object for the accessor to access or modify.}

\item{object}{(\code{ANY})\cr the object to modify in place.}

\item{value}{(\code{ANY})\cr the new value.}

\item{df}{(\code{data.frame} or \code{NULL})\cr data to use if the column information is being
generated from a pre-data layout object.}

\item{rtpos}{(\code{TreePos})\cr root position.}

\item{alt_counts_df}{(\code{data.frame} or \code{tibble})\cr alternative full dataset the rtables framework will use
\emph{only} when calculating column counts.}

\item{ccount_format}{(\code{FormatSpec})\cr The format to be used by default for column
counts throughout this column tree (i.e. if not overridden by a more specific format
specification).}

\item{path}{(\code{character} or \code{NULL})\cr \code{col_counts} accessor and setter only.
Path (in column structure).}
}
\value{
A \code{LayoutColTree} object.

Returns various information about columns, depending on the accessor used.
}
\description{
Column information/structure accessors
}
\seealso{
\code{\link[=facet_colcount]{facet_colcount()}}
}
