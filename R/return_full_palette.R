#' @title Return all colors recognized by fauna_colors function
#'
#' @return Named vector of all colors recognized by fauna_colors
#' @export
#'
#' @examples return_full_palette()
return_full_palette <- function(){
  fauna_colours("force_return_full")
}
