#' @title Apply Faunalytics theme
#' @description Custom Faunalytics theme styling for ggplot objects.
#' @param font Font for non-bold text. Gotham Book by default.
#' @param font_bold Font for bold text. Gotham Bold by default.
#' @param gotham Set to FALSE if you do not have the fonts Gotham Book and Gotham Bold installed and accessible to R. If FALSE, defaults to Helvetica.
#' @return ggplot object
#' @export
#' @import ggplot2 faunalytics
#' @examples 
#' ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_col() + theme_faunalytics()
theme_faunalytics <- function(show_axis_title = TRUE, show_x_axis_title = TRUE, show_y_axis_title = TRUE,
                              show_axis_text = TRUE, show_x_axis_text = TRUE, show_y_axis_text = TRUE, 
                              show_axis_ticks = TRUE, show_x_axis_ticks = TRUE, show_y_axis_ticks = TRUE, 
                              legend_location = "bottom", font = "Gotham Book", font_bold = "Gotham Bold",
                              gotham = TRUE){ 
  
  font <- "Gotham Book"   # font family (non-bold)
  font_bold <- "Gotham Bold" # font family (bold)
  
  if(gotham == FALSE){
    font <- "Helvetica"
    font_bold <- "Helvetica-Bold"
  }
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      # misc. elements
      text = element_text(family = font),
      
      # grid elements
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      
      # axis elements
      axis.title = element_text(family = font_bold,
                                size = 11, color = fauna_colors("darkgrey")),
      axis.ticks = element_line(color = fauna_colors("darkgrey")),
      axis.text = element_text(family = font_bold,
                               size = 10, color = fauna_colors("darkgrey")),
      
      # strip (facet) elements
      strip.text = element_text(family = font_bold, size = 10,
                                color = fauna_colors("lightgrey")),
      strip.background = element_rect(fill = fauna_colors("darkblue")),
      
      # legend elements
      legend.title = element_text(size = 10, family = font_bold,
                                  color = fauna_colors("darkgrey"),
                                  hjust = .5),
      legend.text = element_text(size = 10, family = font_bold,
                                 color = fauna_colors("darkgrey")),
      legend.position = "bottom"
    )
  
}
