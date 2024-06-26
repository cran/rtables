% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colby_constructors.R
\name{list_wrap_x}
\alias{list_wrap_x}
\alias{list_wrap_df}
\title{Returns a function that coerces the return values of a function to a list}
\usage{
list_wrap_x(f)

list_wrap_df(f)
}
\arguments{
\item{f}{(\code{function})\cr the function to wrap.}
}
\value{
A function that returns a list of \code{CellValue} objects.
}
\description{
Returns a function that coerces the return values of a function to a list
}
\details{
\code{list_wrap_x} generates a wrapper which takes \code{x} as its first argument, while \code{list_wrap_df} generates an
otherwise identical wrapper function whose first argument is named \code{df}.

We provide both because when using the functions as tabulation in \code{\link[=analyze]{analyze()}}, functions which take \code{df} as
their first argument are passed the full subset data frame, while those which accept anything else notably
including \code{x} are passed only the relevant subset of the variable being analyzed.
}
\examples{
summary(iris$Sepal.Length)

f <- list_wrap_x(summary)
f(x = iris$Sepal.Length)

f2 <- list_wrap_df(summary)
f2(df = iris$Sepal.Length)

}
\author{
Gabriel Becker
}
