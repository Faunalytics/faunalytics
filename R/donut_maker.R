#' @title Make donut graph
#'
#' @param x Percentage to make donut graph of. Must be a numeric in decimal form.
#' @param color Fill color of graph. Green by default.
#' @param empty_color Color for remaining area. Light gray by default.
#' @param label Text for middle of graph. By default, x written as a percentage. Set to "" for blank.
#' @param label_color Color of text for middle of graph. By default, this is the same as color.
#' @param colour See colour
#' @param empty_colour See empty_color
#' @param label_colour See label_color
#' @return A ggplot graph
#' @export
#' @import ggplot2
#' @examples donut_maker(.25, "red")
donut_maker <- function(x, color = "green", empty_color = "lightgray",
                        label = NULL, label_color = NULL,
                        colour = NULL, empty_colour = NULL,
                        label_colour = NULL){

  if(!is.null(colour)){
    color <- colour
  }
  if(!is.null(empty_colour)){
    empty_color <- empty_colour
  }
  if(!is.null(label_colour)){
    label_color <- label_colour
  }

  data <- data.frame(value = c(x, 1-x)) %>%
    mutate(ymax = cumsum(value),
           ymin = c(0, x))

  use_color <- if(color %in% names(fauna_colors("force_return_full"))){
    fauna_colors(color)
  } else {
    color
  }

  use_empty_color <- if(empty_color %in% names(fauna_colors("force_return_full"))){
    fauna_colors(empty_color)
  } else {
    empty_color
  }

  use_label <- if(is.null(label)){
    paste0(round(100*x),"%")
  } else {
    label
  }

  use_label_color <- if(is.null(label_color)){
    use_color
  } else if (label_color %in% names(fauna_colors("force_return_full"))){
    fauna_colors(label_color)
  }
  else {
    label_color
  }

  ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = value)) +
    geom_rect(fill = c(use_color, use_empty_color), color = c(use_color, use_empty_color)) +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    annotate(geom = 'text', x = 2, y = 0, label = use_label, size = 5, fontface = "bold", color = use_label_color) +
    theme_void() +
    theme(legend.position = "none")
}
