#' Return shades of blue based on the Faunalytics color palette
#'
#' This function takes an integer input and returns a corresponding number of
#' hex codes based on the shades of blue in the Faunalytics color palette. In
#' other words, you give it a number of shades of blue you need it, it gives
#' you those shades.
#' When given 1, it returns Faunalytics blue. When given 2, it returns 
#' Faunalytics dark blue and light blue. When given 3, it returns Faunalytics 
#' dark blue, blue, and light blue. All values greater than 3 are interpolated 
#' between Faunalytics dark blue and light blue, which will be the darkest and 
#' lightest colors returned. 
#' @param x An integer greater than 0.
#' @return A vector of hex codes
#' @export
#'
#' @examples barplot(seq(1,5,1), col = fauna_blues(5))

fauna_blues <- function(x){
  
  if(x <= 0){
    warning("fauna_blues requires an integer greater than 0")
  } else if(!(x %% 1 == 0)){
    warning("fauna_blues requires an integer. For example, 1, 2, or 3, but not 1.5.")
  } else if(x == 1){
    blu <- fauna_colors("blue")
  }else if(x == 2){
    blu <- fauna_colors(c("darkblue", "lightblue"))
  } else if(x == 3){
    blu <- fauna_colors(c("darkblue", "blue", "lightblue"))
  } else if(x > 3){
    blu_f <- colorRampPalette(fauna_colors(c("darkblue", "lightblue")))
    blu <- blu_f(x)
  }
  
  return(blu)
}
