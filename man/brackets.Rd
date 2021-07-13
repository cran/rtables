% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tt_pos_and_access.R
\name{[<-,VTableTree,ANY,ANY,list-method}
\alias{[<-,VTableTree,ANY,ANY,list-method}
\alias{[<-,VTableTree,ANY,ANY,CellValue-method}
\alias{[,VTableTree,logical,logical-method}
\alias{[,VTableTree,logical,ANY-method}
\alias{[,VTableTree,logical,missing-method}
\alias{[,VTableTree,ANY,logical-method}
\alias{[,VTableTree,ANY,missing-method}
\alias{[,VTableTree,missing,ANY-method}
\alias{[,VTableTree,ANY,character-method}
\alias{[,VTableTree,character,ANY-method}
\alias{[,VTableTree,character,character-method}
\alias{[,VTableTree,missing,numeric-method}
\alias{[,VTableTree,numeric,numeric-method}
\alias{[[,VTableTree,list-method}
\title{retrieve and assign elements of a TableTree}
\usage{
\S4method{[}{VTableTree,ANY,ANY,list}(x, i, j, ...) <- value

\S4method{[}{VTableTree,ANY,ANY,CellValue}(x, i, j, ...) <- value

\S4method{[}{VTableTree,logical,logical}(x, i, j, ..., drop = FALSE)

\S4method{[}{VTableTree,logical,ANY}(x, i, j, ..., drop = FALSE)

\S4method{[}{VTableTree,logical,missing}(x, i, j, ..., drop = FALSE)

\S4method{[}{VTableTree,ANY,logical}(x, i, j, ..., drop = FALSE)

\S4method{[}{VTableTree,ANY,missing}(x, i, j, ..., drop = FALSE)

\S4method{[}{VTableTree,missing,ANY}(x, i, j, ..., drop = FALSE)

\S4method{[}{VTableTree,ANY,character}(x, i, j, ..., drop = FALSE)

\S4method{[}{VTableTree,character,ANY}(x, i, j, ..., drop = FALSE)

\S4method{[}{VTableTree,character,character}(x, i, j, ..., drop = FALSE)

\S4method{[}{VTableTree,missing,numeric}(x, i, j, ..., drop = FALSE)

\S4method{[}{VTableTree,numeric,numeric}(x, i, j, ..., drop = FALSE)

\S4method{[[}{VTableTree,list}(x, i, j, ...)
}
\arguments{
\item{x}{TableTree}

\item{i}{index}

\item{j}{index}

\item{\dots}{Includes
\describe{
\item{\emph{keep_topleft}}{ logical(1) (\code{[} only) Should the 'top-left' material for the table be
retained after subsetting. Defaults to \code{NA}, which retains the material if all rows are included (ie
subsetting was by column), and drops it otherwise.}
\item{\emph{keep_titles}}{logical(1) Should title and non-referential footer information be retained. Defaults to \code{FALSE}}
\item{\emph{reindex_refs}}{logical(1). Should referential footnotes be re-indexed as if the resulting subset is the entire table. Defaults to \code{TRUE}}
}}

\item{value}{Replacement value (list, \code{TableRow}, or \code{TableTree})}

\item{drop}{logical(1). Should the value in the cell be returned if only one cell is selected by the combination of
\code{i} and \code{j}. Defaults to \code{FALSE}}
}
\value{
a \code{TableTree} (or \code{ElementaryTable}) object, unless a single cell was selected with \code{drop=TRUE}, in which case the (possibly multi-valued) fully stripped raw value of the selected cell.
}
\description{
retrieve and assign elements of a TableTree
}
\examples{
l <- basic_table() \%>\%
   split_cols_by("ARM") \%>\%
   analyze(c("SEX", "AGE"))

tbl <- build_table(l, DM)

tbl

tbl[1, ]
tbl[1:2, 2]

tbl[2, 1]
tbl[2, 1, drop = TRUE]

tbl[, 1]

tbl[-2, ]
tbl[, -1]

tbl[2, 1] <- rcell(999)
tbl[2, ] <- list(rrow("FFF", 888, 666, 777))
tbl[3, ] <- list(-111, -222, -333)
tbl
}