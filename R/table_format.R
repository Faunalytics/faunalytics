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
#' @param shade Shade alternate rows. TRUE by default. Begins with the second row.
#' @param shade_color Color of alternate row shading. Light blue by default.
#' Currently, the only other option is "lightgray", which will also make text in those dark.
#' @param shade_text Color of text in shaded (or alternate if shaded = FALSE) rows.
#' Light and dark default colors are "white" and "darkgray".
#' @param na.rm Remove NA values from character columns and replace with blanks. TRUE by default.
#' If FALSE, NA will show up in any cells where it appears in the data you feed into this function.
#' @param h_aligns Horizontal alignment of columns. If this is not specified, R will guess.
#' You can either specify a single string which will be applied to all columns,
#' or a vector of strings where that vector's length is equal to the number of columns in the data.
#' Options must be one of: "left", "center", "right"
#' @param col_widths Widths of columns. Must take the form of a list using list(). Uses expressions for the assignment of column widths for the table columns
#' in data. Two-sided formulas (e.g, <LHS> ~ <RHS>) can be used, where the left-hand side corresponds to selections
#' of columns and the right-hand side evaluates to single-length character values in the form {##}px (i.e., pixel dimensions);
#' the px() helper function is best used for this purpose. The pct() helper function is recommended for use in col_widths, which
#' will allow you to set the percentage of the table width each column should make up. The column-based select helpers starts_with(), ends_with(), contains(),
#' matches(), one_of(), and everything() can be used in the LHS. Subsequent expressions that operate on the columns assigned
#' previously will result in overwriting column width values (both in the same cols_width() call and across separate calls).
#' All other columns can be assigned a default width value by using everything() on the left-hand side. See examples.
#' @param caption A string to appear as a caption below the table. This is essentially functioning like a value in the additional row spanning the width of the table.
#' Because of that, captions longer than the width of the table will stretch the table.
#' A solution to this is to insert <br> in the raw HTML where you want line breaks in the caption.
#' @param return_html If TRUE, returns raw HTML of table. FALSE by default
#' @param include_css If TRUE, returns inline CSS for table formatting. TRUE by default. This is only returned if return_html is also TRUE
#' @param write If TRUE, write results to the file specified in the path argument. FALSE by default.
#' @param path File path to be written to if write is TRUE. "table.txt" in working directory by default.
#' @param header_colour See header_color
#' @param text_colour See text_color
#' @param border_colour See border_color
#' @param shade_colour See shade_color
#' @param ... Other arguments
#'
#'
#' @return An HTML table or raw HTML
#' @export
#' @import gt dplyr
#' @examples table_format(head(mtcars))
#' table_format(head(cars)) %>% return_html()
#'
#' mtcars %>% head() %>% select(mpg, cyl, disp, hp) %>%
#' table_format(col_widths = list(
#'     starts_with("m") ~ pct(.2),
#'     cyl ~ pct(.5),
#'     everything() ~ pct(.15)
#'   )
#' )
table_format <- function(data, header_fill = "blue", header_color = "white",
                         cell_fill = "white", text_color = "darkgray",
                         border_color = "white", shade = TRUE,
                         shade_color = "lightblue", shade_text = NULL,
                         na.rm = TRUE, h_aligns = NULL,
                         col_widths = NULL, caption = NULL,
                         return_html = FALSE, include_css = TRUE,
                         write = FALSE, path = "table.txt",
                         header_colour = NULL, text_colour = NULL,
                         border_colour = NULL, shade_colour = NULL,
                         ...){

  if(!is.null(header_colour)){
    header_color <- header_colour
  }
  if(!is.null(text_colour)){
    text_color <- text_colour
  }
  if(!is.null(border_colour)){
    border_color <- border_colour
  }
  if(!is.null(shade_colour)){
    shade_color <- shade_colour
  }

  # Set NA values to ""
  if(na.rm){
    data <- data %>%
      mutate(across(where(is.character), function(x){
        x = case_when(
          is.na(x) ~ "",
          TRUE ~ x
        )
      }))
  }

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

  alt_row_col <- if(shade_color == "lightgray"){
    fauna_colors("lightgray")
  } else if(shade_color == "lightblue"){
    fauna_colors("lightblue")
  } else if(shade_color %in% names(return_full_palette())){
    fauna_colors(shade_color)
  } else {
    shade_color
  }

  if(!(shade_color %in% names(return_full_palette()))){
    warning(paste0("shade_colo(u)r '", shade_color, "' is not in the Faunalytics color palette."))
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

  if(shade & shade_color == "lightblue"){
    foo <- foo %>% tab_style(
      style = list(
        cell_fill(color = alt_row_col),
        cell_text(color = "white")
      ),
      # Apply shading to every other row beginning at row 2
      locations = cells_body(rows = seq(2,nrow(data),2))
    )
  } else if(shade){
    foo <- foo %>% tab_style(
      style = list(
        cell_fill(color = alt_row_col)
      ),
      # Apply shading to every other row beginning at row 2
      locations = cells_body(rows = seq(2,nrow(data),2))
    )
  }

  if(!is.null(shade_text)){
    shade_text <- gsub(" ", "", tolower(shade_text))
    if(shade_text %in% names(return_full_palette())){
      shade_text <- fauna_colors(shade_text)
    }
    foo <- foo %>% tab_style(
      style = list(cell_text(color = shade_text)),
      locations = cells_body(rows = seq(2,nrow(data),2))
    )
  }

  # Column horizontal alignment
  if(!is.null(h_aligns)){
    if(length(h_aligns) == 1){
      foo <- foo %>% cols_align(align = h_aligns)
    } else {

      # To Do: See if there is a more efficient way of executing this instead of a loop,
      # possibly using {eval(parse(...))}

      for(i in 1:length(names(data))){
        foo <- foo %>%
          cols_align(align = h_aligns[i], columns = names(data)[i])
      }
    }
  }

  if(!is.null(col_widths)){
    foo <- foo %>%
      cols_width(
        .list = col_widths
      )
  }

  if(!is.null(caption)){
    foo <- foo %>%
      tab_source_note(source_note = caption) %>%
      tab_options(table_body.border.bottom.color  = "white")
  }

  if(return_html){
    foo <- return_html(foo, include_css = include_css, write = write, path = path)
  }

  if(((!return_html) & write)){
    foo <- return_html(foo, include_css = include_css, write = write, path = path)
    foo <- paste("Table saved to ", path)
  }

  return(foo)
}

