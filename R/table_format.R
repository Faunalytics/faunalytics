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
#' @param shade Shade alternate rows. By default, only shows when there are 4 or more rows. This can be replaced with
#' TRUE to apply regardless of the number of rows, or FALSE to prevent any row shading. 
#' @param shade_fill Color of alternate row shading. Light gray by default. Formerly called shade_color or shade_colour.
#' @param shade_text Color of text in shaded (or alternate if shaded = FALSE) rows.
#' Dark gray by default. 
#' @param na.rm Remove NA values from character columns and replace with blanks. TRUE by default.
#' If FALSE, NA will show up in any cells where it appears in the data you feed into this function.
#' @param star If TRUE, will add an asterisk to star_dest values where star_source is less than star_alpha (0.05  by default). Requires star_source and star_dest to be specified. FALSE by default.
#' @param star_source Source column from which use of asterisk is determined. For example, if your p-values are stored in a column called "p_vals", you would set this to p_vals
#' @param star_dest Destination column to apply asterisk to based on star_source. This column will be converted to a character.
#' @param star_alpha 0.05 by default. When using star_source and star_dest, all star_dest values with a star_source value less than star_alpha will be given an asterisk.
#' Note that a value like 0.0497 that has been rounded to 0.05 will NOT receive an asterisk if you use the rounded column as your star_source
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
#' A solution to this is to insert `\\n` in your text, which will create a linebreak. You may also insert <br> in the raw HTML.
#' @param return_html If TRUE, returns raw HTML of table. FALSE by default
#' @param include_css If TRUE, returns inline CSS for table formatting. TRUE by default. This is only returned if return_html is also TRUE
#' @param write If TRUE, write results to the file specified in the path argument. FALSE by default.
#' @param path File path to be written to if write is TRUE. "table.txt" in working directory by default.
#' @param image_path File path for saving table as image (PNG only). If unspecified, the table will not be saved as an image.
#' Include ".png" at the end of your file path. Requires phantomjs. If you have never installed phantomjs, run webshot::install_phantomjs()
#' @param header_colour See header_color
#' @param text_colour See text_color
#' @param border_colour See border_color
#' @param gotham Set to FALSE if you do not have the fonts Gotham Book and Gotham Bold installed and accessible to R. If FALSE, defaults to Helvetica.
#' @param ... Other arguments
#'
#'
#' @return An HTML table or raw HTML
#' @export
#' @import dplyr webshot stringr
#' @importFrom gt tab_style tab_options cols_align cols_width tab_source_note fmt_markdown
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
table_format <- function(data, header_fill = "darkblue", header_color = "white",
                         cell_fill = "white", text_color = "darkgray",
                         border_color = "white", shade = nrow(data) > 3,
                         shade_fill = "lightgrey", shade_text = NULL,
                         na.rm = TRUE, font_body = "Gotham Book", font_bold = "Gotham Bold",
                         star = FALSE, star_source = NULL, star_dest = NULL,
                         star_alpha = 0.05, h_aligns = NULL,
                         col_widths = NULL, caption = NULL,
                         return_html = FALSE, include_css = TRUE,
                         write = FALSE, path = "table.txt",
                         image_path = NULL,
                         header_colour = NULL, text_colour = NULL,
                         border_colour = NULL, shade_colour = NULL,
                         gotham = TRUE,
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
  
  if(gotham == FALSE){
    font_body <- "Helvetica"
    font_bold <- "Helvetica-Bold"
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

  if(star){
    data <- data %>%
      mutate( {{ star_dest }} := case_when(
        {{ star_source }} < {{ star_alpha }} ~ "*",
        TRUE ~ as.character( {{ star_dest }} )
      )
      )
  }


  # Create gt table 'foo' out of data after replacing \s\n\s with <br>
  foo <- data %>%
    mutate(across(where(is.character), \(x){
      x = gsub("\\s\\n\\s", "<br>", x)
    } )) %>%
  gt()

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
  shade_fill <- gsub(" ", "", tolower(shade_fill)) # Standardize

  alt_row_col <- if(shade_fill == "lightgray"){
    fauna_colors("lightgray")
  } else if(shade_fill == "lightblue"){
    fauna_colors("lightblue")
  } else if(shade_fill %in% names(return_full_palette())){
    fauna_colors(shade_fill)
  } else {
    shade_fill
  }

  if(!(shade_fill %in% names(return_full_palette()))){
    warning(paste0("shade_fill '", shade_fill, "' is not in the Faunalytics color palette."))
  }


  # Set table characteristics
  foo <- foo %>%
    # Set table body characteristics
    tab_style(
      style = list(
        cell_fill(color = cell_fill),
        cell_text(color = text_color, font = font_body)
      ),
      locations = cells_body()
    ) %>%
    # Set header characteristics
    tab_style(
      style = list(
        cell_fill(color = header_fill),
        cell_text(color = header_color, font = font_bold)
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

  if(nrow(data) > 1){
  if(shade & shade_fill == "lightblue"){
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

    caption <- caption |> stringr::str_split("\\<br\\>|\\s\\n\\s") |> unlist() |> trimws()


    foo <- foo %>%
      tab_source_note(source_note = caption) %>%
      tab_options(table_body.border.bottom.color  = "white")
  }

  foo <- foo %>%
    fmt_markdown(columns = everything(), rows = everything())

  if(return_html){
    foo <- return_html(foo, include_css = include_css, write = write, path = path)
  }

  if(((!return_html) & write)){
    foo <- return_html(foo, include_css = include_css, write = write, path = path)
    foo <- paste("Table saved to ", path)
  }

  if(!is.null(image_path)){
    if(webshot::is_phantomjs_installed()){
      gtsave(foo, image_path)
    } else {g
      warning("In order to save tables from this function, you must first install phantomjs using webshot::install_phantomjs()")
    }
  }

  return(foo)
}

