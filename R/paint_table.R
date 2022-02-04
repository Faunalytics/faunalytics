#' Paint table elements
#'
#' This function allows you to change the colors cells and text in the body of the table.
#' Header text should be changed with a table_format function.
#' @param tbl A table made with table_format, demo_table_format, or gt
#' @param rows Index numbers of rows to change. If left blank, every row will be affected.
#'     Ranges should be specified using colon notation (e.g., rows = 2:3)
#' @param columns Names or index numbers of rows to change. If left blank, every column will be affected.
#'     Ranges should be specified using colon notation (e.g., columns = mpg:cyl or columns = 1:2)
#' @param fill_color color to fill cells with. If blank, the fill won't be changed. You may also use fill_colour.
#' @param text_color color to change text colort o. If blank, the text color won't be changed. You may also use text_colour.
#' @param fill_colour See fill_color.
#' @param text_colour See text_color.
#' @return A gt table
#' @export
#'
#' @examples head(mtcars) %>% table_format() %>%
#'   paint_table(rows = 2:3, columns = 4, fill_color = "pink", text_color = "red")
paint_table <- function(tbl, rows = everything(), columns = everything(),
                        fill_color = NULL, text_color = NULL,
                        fill_colour = NULL, text_colour = NULL){

  if(!is.null(fill_colour)){
    fill_color <- fill_colour
  }
  if(!is.null(text_colour)){
    text_color <- text_colour
  }

  if((!is.null(fill_color) & !is.null(text_color))){
    tbl %>%
      tab_style(
        style = list(cell_fill(color = fill_color),
                     cell_text(color = text_color)),
        locations = cells_body(columns = all_of(columns), rows = all_of(rows))
      )
  } else if(!is.null(fill_color)){
    tbl %>%
      tab_style(
        style = list(cell_fill(color = fill_color)),
        locations = cells_body(columns = all_of(columns), rows = all_of(rows))
      )
  } else if(!is.null(text_color)){
    tbl %>%
      tab_style(
        style = list(cell_text(color = text_color)),
        locations = cells_body(columns = all_of(columns), rows = all_of(rows))
      )
  } else {
    tbl
  }
}
