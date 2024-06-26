% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_accessors.R
\name{facet_colcount}
\alias{facet_colcount}
\alias{facet_colcount,LayoutColTree-method}
\alias{facet_colcount,LayoutColLeaf-method}
\alias{facet_colcount,VTableTree-method}
\alias{facet_colcount,InstantiatedColumnInfo-method}
\alias{facet_colcount<-}
\alias{facet_colcount<-,LayoutColTree-method}
\alias{facet_colcount<-,LayoutColLeaf-method}
\alias{facet_colcount<-,VTableTree-method}
\alias{facet_colcount<-,InstantiatedColumnInfo-method}
\title{Get or set column count for a facet in column space}
\usage{
facet_colcount(obj, path)

\S4method{facet_colcount}{LayoutColTree}(obj, path = NULL)

\S4method{facet_colcount}{LayoutColLeaf}(obj, path = NULL)

\S4method{facet_colcount}{VTableTree}(obj, path)

\S4method{facet_colcount}{InstantiatedColumnInfo}(obj, path)

facet_colcount(obj, path) <- value

\S4method{facet_colcount}{LayoutColTree}(obj, path) <- value

\S4method{facet_colcount}{LayoutColLeaf}(obj, path) <- value

\S4method{facet_colcount}{VTableTree}(obj, path) <- value

\S4method{facet_colcount}{InstantiatedColumnInfo}(obj, path) <- value
}
\arguments{
\item{obj}{(\code{ANY})\cr the object for the accessor to access or modify.}

\item{path}{character. This path must end on a
split value, e.g., the level of a categorical variable
that was split on in column space, but it need not
be the path to an individual column.}

\item{value}{(\code{ANY})\cr the new value.}
}
\value{
for \code{facet_colcount} the current count associated
with that facet in column space, for \verb{facet_colcount<-},
\code{obj} modified with the new column count for the specified
facet.
}
\description{
Get or set column count for a facet in column space
}
\note{
Updating a lower-level (more specific)
column count manually \strong{will not} update the
counts for its parent facets. This cannot be made
automatic because the rtables framework does not
require sibling facets to be mutually exclusive
(e.g., total "arm", faceting into cumulative
quantiles, etc) and thus the count of a parent facet
will not always be simply the sum of the counts for
all of its children.
}
\examples{
lyt <- basic_table() \%>\%
  split_cols_by("ARM", show_colcounts = TRUE) \%>\%
  split_cols_by("SEX",
    split_fun = keep_split_levels(c("F", "M")),
    show_colcounts = TRUE
  ) \%>\%
  split_cols_by("STRATA1", show_colcounts = TRUE) \%>\%
  analyze("AGE")

tbl <- build_table(lyt, ex_adsl)

facet_colcount(tbl, c("ARM", "A: Drug X"))
facet_colcount(tbl, c("ARM", "A: Drug X", "SEX", "F"))
facet_colcount(tbl, c("ARM", "A: Drug X", "SEX", "F", "STRATA1", "A"))

## modify specific count after table creation
facet_colcount(tbl, c("ARM", "A: Drug X", "SEX", "F", "STRATA1", "A")) <- 25

## show black space for certain counts by assign NA

facet_colcount(tbl, c("ARM", "A: Drug X", "SEX", "F", "STRATA1", "C")) <- NA

}
\seealso{
\code{\link[=col_counts]{col_counts()}}
}
