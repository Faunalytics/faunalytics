#' @title Apply Faunalytics theme
#' @description Custom Faunalytics theme styling for ggplot objects.
#' @param font Font for non-bold text. Gotham Book by default.
#' @param font_bold Font for bold text. Gotham Bold by default.
#' @param gotham Set to FALSE if you do not have the fonts Gotham Book and Gotham Bold installed and accessible to R. If FALSE, defaults to Helvetica.
#' @return ggplot object
#' @import ggplot2
#' @importFrom sysfonts font_files
#' @export
#' 
theme_faunalytics <- function(font = "Gotham Book", font_bold = "Gotham Bold",
                              gotham = TRUE){ 
  
  if(gotham == TRUE | (font == "Gotham Book" & font_bold == "Gotham Bold")){
    font_db <- font_files() |> as_tibble()
    font_check <- font %in% font_db$family
    font_bold_check <- font_bold %in% font_db$family
    
    if(font_check == FALSE | font_bold_check == FALSE){
      warning(paste0("Warning: At least one of the following fonts is not loaded:\n",
                     font, ", ", font_bold, ".\n
                     Defaulting to Helvetica."))
      
      font <- "Helvetica"
      font_bold <- "Helvetica-Bold"
    }
  }
  
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
