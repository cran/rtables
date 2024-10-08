% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_html.R
\name{as_html}
\alias{as_html}
\title{Convert an \code{rtable} object to a \code{shiny.tag} HTML object}
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
  no_spaces_between_cells = FALSE,
  expand_newlines = FALSE
)
}
\arguments{
\item{x}{(\code{VTableTree})\cr a \code{TableTree} object.}

\item{width}{(\code{character})\cr a string to indicate the desired width of the table. Common input formats include a
percentage of the viewer window width (e.g. \code{"100\%"}) or a distance value (e.g. \code{"300px"}). Defaults to \code{NULL}.}

\item{class_table}{(\code{character})\cr class for \code{table} tag.}

\item{class_tr}{(\code{character})\cr class for \code{tr} tag.}

\item{class_th}{(\code{character})\cr class for \code{th} tag.}

\item{link_label}{(\code{character})\cr link anchor label (not including \verb{tab:} prefix) for the table.}

\item{bold}{(\code{character})\cr elements in table output that should be bold. Options are \code{"main_title"},
\code{"subtitles"}, \code{"header"}, \code{"row_names"}, \code{"label_rows"}, and \code{"content_rows"} (which includes any non-label
rows). Defaults to \code{"header"}.}

\item{header_sep_line}{(\code{flag})\cr whether a black line should be printed to under the table header. Defaults
to \code{TRUE}.}

\item{no_spaces_between_cells}{(\code{flag})\cr whether spaces between table cells should be collapsed. Defaults
to \code{FALSE}.}

\item{expand_newlines}{(\code{flag})\cr Defaults to \code{FALSE}, relying on \code{html} output to solve newline characters (\verb{\\n}).
Doing this keeps the structure of the cells but may depend on the output device.}
}
\value{
A \code{shiny.tag} object representing \code{x} in HTML.
}
\description{
The returned HTML object can be immediately used in \code{shiny} and \code{rmarkdown}.
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
