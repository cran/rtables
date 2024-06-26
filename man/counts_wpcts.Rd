% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colby_constructors.R
\name{counts_wpcts}
\alias{counts_wpcts}
\title{Analysis function to count levels of a factor with percentage of the column total}
\usage{
counts_wpcts(x, .N_col)
}
\arguments{
\item{x}{(\code{factor})\cr a vector of data, provided by rtables pagination machinery.}

\item{.N_col}{(\code{integer(1)})\cr total count for the column, provided by rtables pagination machinery.}
}
\value{
A \code{RowsVerticalSection} object with counts (and percents) for each level of the factor.
}
\description{
Analysis function to count levels of a factor with percentage of the column total
}
\examples{
counts_wpcts(DM$SEX, 400)

}
