#' @title Return color(s) from the Faunalytics palette
#' @description Used to retrieve a single color or vector of colors from the Faunalytics color palette. In order, colors included are:
#' green, amber (or orange), red, dark blue, light blue, blue. The following are not part of the Faunalytics color palette, but will be returned
#' when requested: black, white, gray, light gray, dark gray. Variations of grays spelled with an "e" (e.g., dark grey) will also work.
#'
#' @param ... One of the following: color name(s) in quotes, a number of desired colors (no more than 6), or nothing.
#' Quoted colors must be a part of the Faunalytics color palette.
#' If no value is provided, this function will return the base color palette with names.
#' @return A named color hex code or named vector of color hex codes
#' @export
#'
#' @examples fauna_colors("green")
#' fauna_colors("orange", "lightblue")
#' fauna_colors(4)

fauna_colours <- function(...){
  fauna_colors(...)
}
