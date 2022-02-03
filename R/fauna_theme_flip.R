#' Apply Faunalytics aesthetics to ggplot graphs that use coord_flip
#'
#'
#' @return A Faunalytics-themed ggplot object
#' @export
#' @import ggplot2
#' @examples library(ggplot2)
#' ggplot(mtcars, aes(x = cyl, y = mpg)) +
#'     geom_col() +
#'     fauna_theme_flip()
fauna_theme_flip <- function(){
  theme_classic() +
    theme(panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          axis.title = element_text(color = fauna_colors("darkgray"), size = 12, face = "bold"),
          axis.text.x = element_text(color = fauna_colors("darkgray"), size = 11, vjust = -.25),
          axis.text.y = element_text(color = fauna_colors("darkgray"), size = 11, hjust = -.25),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = fauna_colors("lightgray")),
          axis.line = element_line(color = fauna_colors("lightgray")),
          axis.ticks = element_line(color = fauna_colors("gray")),
          legend.position = "bottom"
    )
}

