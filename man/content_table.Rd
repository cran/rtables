% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_accessors.R
\name{content_table}
\alias{content_table}
\alias{content_table<-}
\title{Retrieve or set content table from a \code{TableTree}}
\usage{
content_table(obj)

content_table(obj) <- value
}
\arguments{
\item{obj}{(\code{TableTree})\cr the table object.}

\item{value}{(\code{ElementaryTable})\cr the new content table for \code{obj}.}
}
\value{
the \code{ElementaryTable} containing the (top level) \emph{content rows} of \code{obj} (or \code{NULL} if \code{obj} is not
a formal table object).
}
\description{
Returns the content table of \code{obj} if it is a \code{TableTree} object, or \code{NULL} otherwise.
}
