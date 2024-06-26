% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_accessors.R
\name{names,VTableNodeInfo-method}
\alias{names,VTableNodeInfo-method}
\alias{names,InstantiatedColumnInfo-method}
\alias{names,LayoutColTree-method}
\alias{row.names,VTableTree-method}
\title{Names of a \code{TableTree}}
\usage{
\S4method{names}{VTableNodeInfo}(x)

\S4method{names}{InstantiatedColumnInfo}(x)

\S4method{names}{LayoutColTree}(x)

\S4method{row.names}{VTableTree}(x)
}
\arguments{
\item{x}{(\code{TableTree})\cr the object.}
}
\value{
The column names of \code{x}, as defined in the details above.
}
\description{
Names of a \code{TableTree}
}
\details{
For \code{TableTree}s with more than one level of splitting in columns, the names are defined to be the top-level
split values repped out across the columns that they span.
}
