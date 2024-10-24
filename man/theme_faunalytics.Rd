% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_faunalytics.R
\name{theme_faunalytics}
\alias{theme_faunalytics}
\title{Apply Faunalytics theme}
\usage{
theme_faunalytics(
  show_axis_title = TRUE,
  show_x_axis_title = TRUE,
  show_y_axis_title = TRUE,
  show_axis_text = TRUE,
  show_x_axis_text = TRUE,
  show_y_axis_text = TRUE,
  show_axis_ticks = TRUE,
  show_x_axis_ticks = TRUE,
  show_y_axis_ticks = TRUE,
  legend_location = "bottom",
  font = "Gotham Book",
  font_bold = "Gotham Bold",
  gotham = TRUE
)
}
\arguments{
\item{font}{Font for non-bold text. Gotham Book by default.}

\item{font_bold}{Font for bold text. Gotham Bold by default.}

\item{gotham}{Set to FALSE if you do not have the fonts Gotham Book and Gotham Bold installed and accessible to R. If FALSE, defaults to Helvetica.}
}
\value{
ggplot object
}
\description{
Custom Faunalytics theme styling for ggplot objects.
}
\examples{
ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_col() + theme_faunalytics()
}
