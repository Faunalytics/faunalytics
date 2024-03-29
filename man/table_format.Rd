% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_format.R, R/table_format2.R
\name{table_format}
\alias{table_format}
\title{Format report tables}
\usage{
table_format(
  data,
  header_fill = "blue",
  header_color = "white",
  cell_fill = "white",
  text_color = "darkgray",
  border_color = "white",
  shade = TRUE,
  shade_color = "lightblue",
  shade_text = NULL,
  na.rm = TRUE,
  star = FALSE,
  star_source = NULL,
  star_dest = NULL,
  star_alpha = 0.05,
  h_aligns = NULL,
  col_widths = NULL,
  caption = NULL,
  return_html = FALSE,
  include_css = TRUE,
  write = FALSE,
  path = "table.txt",
  image_path = NULL,
  header_colour = NULL,
  text_colour = NULL,
  border_colour = NULL,
  shade_colour = NULL,
  ...
)

table_format(
  data,
  header_fill = "blue",
  header_color = "white",
  cell_fill = "white",
  text_color = "darkgray",
  border_color = "white",
  shade = TRUE,
  shade_color = "lightblue",
  shade_text = NULL,
  na.rm = TRUE,
  star = FALSE,
  star_source = NULL,
  star_dest = NULL,
  star_alpha = 0.05,
  h_aligns = NULL,
  col_widths = NULL,
  caption = NULL,
  return_html = FALSE,
  include_css = TRUE,
  write = FALSE,
  path = "table.txt",
  image_path = NULL,
  header_colour = NULL,
  text_colour = NULL,
  border_colour = NULL,
  shade_colour = NULL,
  ...
)
}
\arguments{
\item{data}{Required. A dataframe to be formatted as an html table}

\item{header_fill}{Color of header background. Blue by default}

\item{header_color}{Color of header text. White by default}

\item{cell_fill}{Color of table body cells. White by default}

\item{text_color}{Color of table body text. Dark gray by default}

\item{border_color}{Color of cell borders. White by default}

\item{shade}{Shade alternate rows. TRUE by default. Begins with the second row.}

\item{shade_color}{Color of alternate row shading. Light blue by default.
Currently, the only other option is "lightgray", which will also make text in those dark.}

\item{shade_text}{Color of text in shaded (or alternate if shaded = FALSE) rows.
Light and dark default colors are "white" and "darkgray".}

\item{na.rm}{Remove NA values from character columns and replace with blanks. TRUE by default.
If FALSE, NA will show up in any cells where it appears in the data you feed into this function.}

\item{star}{If TRUE, will add an asterisk to star_dest values where star_source is less than star_alpha (0.05  by default). Requires star_source and star_dest to be specified. FALSE by default.}

\item{star_source}{Source column from which use of asterisk is determined. For example, if your p-values are stored in a column called "p_vals", you would set this to p_vals}

\item{star_dest}{Destination column to apply asterisk to based on star_source. This column will be converted to a character.}

\item{star_alpha}{0.05 by default. When using star_source and star_dest, all star_dest values with a star_source value less than star_alpha will be given an asterisk.
Note that a value like 0.0497 that has been rounded to 0.05 will NOT receive an asterisk if you use the rounded column as your star_source}

\item{h_aligns}{Horizontal alignment of columns. If this is not specified, R will guess.
You can either specify a single string which will be applied to all columns,
or a vector of strings where that vector's length is equal to the number of columns in the data.
Options must be one of: "left", "center", "right"}

\item{col_widths}{Widths of columns. Must take the form of a list using list(). Uses expressions for the assignment of column widths for the table columns
in data. Two-sided formulas (e.g, <LHS> ~ <RHS>) can be used, where the left-hand side corresponds to selections
of columns and the right-hand side evaluates to single-length character values in the form {##}px (i.e., pixel dimensions);
the px() helper function is best used for this purpose. The pct() helper function is recommended for use in col_widths, which
will allow you to set the percentage of the table width each column should make up. The column-based select helpers starts_with(), ends_with(), contains(),
matches(), one_of(), and everything() can be used in the LHS. Subsequent expressions that operate on the columns assigned
previously will result in overwriting column width values (both in the same cols_width() call and across separate calls).
All other columns can be assigned a default width value by using everything() on the left-hand side. See examples.}

\item{caption}{A string to appear as a caption below the table. This is essentially functioning like a value in the additional row spanning the width of the table.
Because of that, captions longer than the width of the table will stretch the table.
A solution to this is to insert `\\n` in your text, which will create a linebreak. You may also insert <br> in the raw HTML.}

\item{return_html}{If TRUE, returns raw HTML of table. FALSE by default}

\item{include_css}{If TRUE, returns inline CSS for table formatting. TRUE by default. This is only returned if return_html is also TRUE}

\item{write}{If TRUE, write results to the file specified in the path argument. FALSE by default.}

\item{path}{File path to be written to if write is TRUE. "table.txt" in working directory by default.}

\item{image_path}{File path for saving table as image (PNG only). If unspecified, the table will not be saved as an image.
Include ".png" at the end of your file path. Requires phantomjs. If you have never installed phantomjs, run webshot::install_phantomjs()}

\item{header_colour}{See header_color}

\item{text_colour}{See text_color}

\item{border_colour}{See border_color}

\item{shade_colour}{See shade_color}

\item{...}{Other arguments}
}
\value{
An HTML table or raw HTML

An HTML table or raw HTML
}
\description{
Format a table using Faunalytics standards.
If you want to specify column widths, leave return_html as FALSE and pipe the results of table_format
into cols_width, where you will specify the column widths. To return html, pipe the result of that pipeline
into return_html. For more, see ?cols_width and ?return_html. An example is given below.

Format a table using Faunalytics standards.
If you want to specify column widths, leave return_html as FALSE and pipe the results of table_format
into cols_width, where you will specify the column widths. To return html, pipe the result of that pipeline
into return_html. For more, see ?cols_width and ?return_html. An example is given below.
}
\examples{
table_format(head(mtcars))
table_format(head(cars)) \%>\% return_html()

mtcars \%>\% head() \%>\% select(mpg, cyl, disp, hp) \%>\%
table_format(col_widths = list(
    starts_with("m") ~ pct(.2),
    cyl ~ pct(.5),
    everything() ~ pct(.15)
  )
)
table_format(head(mtcars))
table_format(head(cars)) \%>\% return_html()

mtcars \%>\% head() \%>\% select(mpg, cyl, disp, hp) \%>\%
table_format(col_widths = list(
    starts_with("m") ~ pct(.2),
    cyl ~ pct(.5),
    everything() ~ pct(.15)
  )
)
}
