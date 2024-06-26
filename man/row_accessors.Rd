% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_accessors.R
\name{obj_avar}
\alias{obj_avar}
\alias{obj_avar,TableRow-method}
\alias{obj_avar,ElementaryTable-method}
\alias{row_cells}
\alias{row_cells,TableRow-method}
\alias{row_cells<-}
\alias{row_cells<-,TableRow-method}
\alias{row_values}
\alias{row_values,TableRow-method}
\alias{row_values<-}
\alias{row_values<-,TableRow-method}
\alias{row_values<-,LabelRow-method}
\title{Row attribute accessors}
\usage{
obj_avar(obj)

\S4method{obj_avar}{TableRow}(obj)

\S4method{obj_avar}{ElementaryTable}(obj)

row_cells(obj)

\S4method{row_cells}{TableRow}(obj)

row_cells(obj) <- value

\S4method{row_cells}{TableRow}(obj) <- value

row_values(obj)

\S4method{row_values}{TableRow}(obj)

row_values(obj) <- value

\S4method{row_values}{TableRow}(obj) <- value

\S4method{row_values}{LabelRow}(obj) <- value
}
\arguments{
\item{obj}{(\code{ANY})\cr the object for the accessor to access or modify.}

\item{value}{(\code{ANY})\cr the new value.}
}
\value{
Various return values depending on the accessor called.
}
\description{
Row attribute accessors
}
