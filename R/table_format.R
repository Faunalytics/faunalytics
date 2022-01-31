#' @title Format report tables
#' @description Format a table using Faunalytics standards.
#' If you want to specify column widths, leave return_html as FALSE and pipe the results of table_format
#' into cols_width, where you will specify the column widths. To return html, pipe the result of that pipeline
#' into return_html. For more, see ?cols_width and ?return_html. An example is given below.
#' @param data Required. A dataframe to be formatted as an html table
#' @param header_fill Color of header background. Blue by default
#' @param header_color Color of header text. White by default
#' @param cell_fill Color of table body cells. White by default
#' @param text_color Color of table body text. Dark gray by default
#' @param border_color Color of cell borders. White by default
#' @param shade_color Color of alternate row shading. Gray by default.
#' Currently, the only other option is "lightblue", which will make text in those rows white.
#' @param return_html If TRUE, returns raw HTML of table. FALSE by default
#' @param include_css If TRUE, returns inline CSS for table formatting. TRUE by default. This is only returned if return_html is also TRUE
#' @param ... Other arguments
#'
#'
#' @return An HTML table or raw HTML
#' @export
#' @import gt dplyr
#' @examples table_format(head(mtcars))
#' table_format(head(cars)) %>% return_html()
table_format <- function(data, header_fill = "blue", header_color = "white",
                         cell_fill = "white", text_color = "darkgray",
                         border_color = "white", shade_color = "lightgray",
                         return_html = FALSE, include_css = TRUE,
                         ...){

  # Create gt table 'foo' out of data
  foo <- data %>% gt()

  # Header fill
  header_fill <- gsub(" ", "", tolower(header_fill)) # Standardize
  header_fill <- if(header_fill %in% names(return_full_palette())){
    fauna_colors(header_fill)} else { header_fill } # If color matches a fauna_color, use that
  # Header color
  header_color <- gsub(" ", "", tolower(header_color)) # Standardize
  header_color <- if(header_color %in% names(return_full_palette())){
    fauna_colors(header_color)} else { header_color } # If color matches a fauna_color, use that
  # Cell fill
  cell_fill <- gsub(" ", "", tolower(cell_fill)) # Standardize
  cell_fill <- if(cell_fill %in% names(return_full_palette())){
    fauna_colors(cell_fill)} else { cell_fill } # If color matches a fauna_color, use that
  # Text color
  text_color <- gsub(" ", "", tolower(text_color)) # Standardize
  text_color <- if(text_color %in% names(return_full_palette())){
    fauna_colors(text_color)} else { text_color } # If color matches a fauna_color, use that
  # Border color
  border_color <- gsub(" ", "", tolower(border_color)) # Standardize
  border_color <- if(border_color %in% names(return_full_palette())){
    fauna_colors(border_color)} else { border_color } # If color matches a fauna_color, use that

  # Shading color
  shade_color <- gsub(" ", "", tolower(shade_color)) # Standardize

  alt_row_col <- if(shade_color == "lightblue"){
    fauna_colors("lightblue")
  } else {
    fauna_colors("lightgray")
  }

  # Set table characteristics
  foo <- foo %>%
    # Set table body characteristics
    tab_style(
      style = list(
        cell_fill(color = cell_fill),
        cell_text(color = text_color)
      ),
      locations = cells_body()
    ) %>%
    # Set header characteristics
    tab_style(
      style = list(
        cell_fill(color = header_fill),
        cell_text(color = header_color, weight = "bold")
      ),
      locations = list(
        cells_column_labels()
      )
    ) %>%
    # Set border characteristics
    tab_style(
      style = list(
        cell_borders(sides = "all", color = "white", weight = px(1))
      ),
      locations = list(cells_body(), cells_column_labels())
    ) %>%
    tab_options(
      table.border.top.style = "hidden",
      table.border.bottom.style = "hidden",
      column_labels.border.bottom.color = unname(border_color))

  # If data has more than 4 rows, alternate shade rows for readability
  if(nrow(data) > 4 & shade_color == "lightblue"){
    foo <- foo %>% tab_style(
      style = list(
        cell_fill(color = alt_row_col),
        cell_text(color = "white")
      ),
      # Apply shading to every other row beginning at row 2
      locations = cells_body(rows = seq(2,nrow(data),2))
    )
  } else if(nrow(data) > 4){
    foo <- foo %>% tab_style(
      style = list(
        cell_fill(color = alt_row_col)
      ),
      # Apply shading to every other row beginning at row 2
      locations = cells_body(rows = seq(2,nrow(data),2))
    )
  }

  if(return_html){
    foo <- foo %>% as_raw_html(inline_css = include_css)
  }

  return(foo)
}

