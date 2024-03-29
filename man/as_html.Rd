% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_html.R
\name{as_html}
\alias{as_html}
\title{Convert an \code{rtable} object to a \code{shiny.tag} html object}
\usage{
as_html(
  x,
  width = NULL,
  class_table = "table table-condensed table-hover",
  class_tr = NULL,
  class_th = NULL,
  link_label = NULL,
  bold = c("header"),
  header_sep_line = TRUE,
  no_spaces_between_cells = FALSE
)
}
\arguments{
\item{x}{\code{rtable} object}

\item{width}{a string to indicate the desired width of the table. Common input formats include a
percentage of the viewer window width (e.g. \code{"100\%"}) or a distance value (e.g. \code{"300px"}).
Defaults to \code{NULL}.}

\item{class_table}{class for \code{table} tag}

\item{class_tr}{class for \code{tr} tag}

\item{class_th}{class for \code{th} tag}

\item{link_label}{link anchor label (not including \verb{tab:} prefix) for the table.}

\item{bold}{elements in table output that should be bold. Options are \code{"main_title"}, \code{"subtitles"},
\code{"header"}, \code{"row_names"}, \code{"label_rows"}, and \code{"content_rows"} (which includes any non-label rows).
Defaults to \code{"header"}.}

\item{header_sep_line}{whether a black line should be printed to under the table header. Defaults to \code{TRUE}.}

\item{no_spaces_between_cells}{whether spaces between table cells should be collapsed. Defaults to \code{FALSE}.}
}
\value{
A \code{shiny.tag} object representing \code{x} in HTML.
}
\description{
The returned \code{html} object can be immediately used in \code{shiny} and \code{rmarkdown}.
}
\examples{

tbl <- rtable(
  header = LETTERS[1:3],
  format = "xx",
  rrow("r1", 1, 2, 3),
  rrow("r2", 4, 3, 2, indent = 1),
  rrow("r3", indent = 2)
)

as_html(tbl)

as_html(tbl, class_table = "table", class_tr = "row")

as_html(tbl, bold = c("header", "row_names"))

\dontrun{
Viewer(tbl)
}

}
