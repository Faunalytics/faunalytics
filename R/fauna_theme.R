#' Apply Faunalytics aesthetics to ggplot graphs
#'
#' @param p A ggplot object
#'
#' @return A Faunalytics-themed ggplot object
#' @export
#' @import ggplot2
#' @examples ggplot(mtcars, aes(x = cyl, y = mpg)) +
#' geom_col() +
#' fauna_theme()
fauna_theme <- function(){
  theme_classic() +
    theme(panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          axis.title = element_text(color = fauna_colors("darkgray"), size = 12),
          axis.text.x = element_text(color = fauna_colors("darkgray"), size = 11, vjust = -.25),
          axis.text.y = element_text(color = fauna_colors("darkgray"), size = 11, hjust = -.25),
          panel.grid.major.x = element_line(color = fauna_colors("lightgray")),
          panel.grid.major.y = element_line(color = fauna_colors("lightgray")),
          axis.line = element_line(color = fauna_colors("lightgray")),
          axis.ticks = element_line(color = fauna_colors("gray"))
         )
}

