% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00tabletrees.R
\docType{class}
\name{InstantiatedColumnInfo-class}
\alias{InstantiatedColumnInfo-class}
\alias{InstantiatedColumnInfo}
\title{InstantiatedColumnInfo}
\usage{
InstantiatedColumnInfo(
  treelyt = LayoutColTree(),
  csubs = list(expression(TRUE)),
  extras = list(list()),
  cnts = NA_integer_,
  total_cnt = NA_integer_,
  dispcounts = FALSE,
  countformat = "(N=xx)",
  topleft = character()
)
}
\arguments{
\item{treelyt}{LayoutColTree.}

\item{csubs}{list. List of subsetting expressions}

\item{extras}{list. Extra arguments associated with the columns}

\item{cnts}{integer. Counts.}

\item{total_cnt}{integer(1). Total observations represented across all columns.}

\item{dispcounts}{logical. Should the counts be displayed as header info when the associated table is printed.}

\item{countformat}{string. Format for the counts if thtey are displayed}

\item{topleft}{character. Override values for the "top left" material to be displayed during printing.}
}
\value{
an \code{InstantiateadColumnInfo} object.
}
\description{
InstantiatedColumnInfo
}