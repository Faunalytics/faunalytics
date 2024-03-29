#' @title Format demographic report tables
#' @description Characteristics can be reordered using the chars argument, but traits within those groups will be ordered as they are in the data.
#' The exception to this is for repeated options (e.g., "Yes" and "No"), which will be ordered in the way they first appear in the data.
#' @param data Required. A dataframe to be formatted as an html table. This dataframe should have the desired column names, with the exception of the main characteristic variable name, which can be overriden to blank.
#' @param char_var Required. Variable that contains characteristics. Set to Characteristic by default.
#' @param chars A vector of quoted characteristic groups. Table will be ordered in the same order these groups are listed.
#' These should NOT include specific traits (e.g., "Woman"), but instead, only the groups those traits fall into (e.g. "Gender").
#' These are the rows that will be fill in a different color.
#' @param char_header_blank If TRUE, the column specified in char_var will be set to blank. FALSE by default.
#' @param stripe_fill Color to fill characteristic row. "lightblue" by default.
#' @param stripe_color Color of text in characteristic row. "white" by default.
#' @param text_color Color of text in non-characteristic rows and header. "darkgray" by default.
#' @param na.rm Remove NA values from character columns and replace with blanks. TRUE by default.
#' If FALSE, NA will show up in any cells where it appears in the data you feed into this function.
#' @param caption A string to appear as a caption below the table. This is essentially functioning like a value in the additional row spanning the width of the table.
#' Because of that, captions longer than the width of the table will stretch the table.
#' A solution to this is to insert `\\n` in your text, which will create a linebreak. You may also insert <br> in the raw HTML.
#' @param return_html If TRUE, returns raw HTML of table. FALSE by default
#' @param include_css If TRUE, returns inline CSS for table formatting. TRUE by default. This is only returned if return_html is also TRUE
#' @param write If TRUE, write results to the file specified in the path argument. FALSE by default.
#' @param path File path to be written to if write is TRUE. "table.txt" in working directory by default.
#' @param stripe_colour See stripe_color
#' @param text_colour See text_color
#' @param ... Other arguments
#' @return An HTML table or raw HTML
#' @export
#' @importFrom zoo na.locf
#' @examples temp_data <- data.frame(
#' "Trait" = c("Gender", "Woman or Other", "Man",
#'     "Income", "Less than $25,000", "$25,000 to $49,999", "$50,000 to $74,999",
#'     "$75,000 to $99,999", "$100,000 or more",
#'     "Education", "Less than high school graduate",
#'     "High school graduate", "Some college or university, no degree",
#'     "Associate's degree or college diploma", "Bachelor's degree",
#'     "Master's degree", "Professional or doctoral degree"),
#' "Percent" = c("", "49%", "51%",
#'     "", "12%", "20%", "19%",
#'     "22%", "27%",
#'     "", "3%",
#'     "18%", "17%",
#'     "18%", "32%",
#'    "10%", "3%"))
#' demo_table_format(temp_data,
#' char_var = Trait,
#' chars = c("Gender", "Income", "Education"))
demo_table_format <- function(data, char_var = Characteristic,
                              chars = c("Gender", "Race", "Region", "Companion Animals", "Went Fishing", "Handled Chickens"),
                              char_header_blank = FALSE, stripe_fill = "lightblue",
                              stripe_color = "white", text_color = "darkgray",
                              na.rm = T, caption = NULL,
                              return_html = FALSE, include_css = TRUE,
                              write = FALSE, path = "table.txt",
                              stripe_colour = NULL, text_colour = NULL,
                              ...){

   # Set NA values to ""
   # if(na.rm){
   #    data <- data %>%
   #       mutate(across(where(is.character), function(x){
   #          x = case_when(
   #             is.na(x) ~ "",
   #             TRUE ~ x
   #          )
   #       }))
   # }


   if(!is.null(stripe_colour)){
      stripe_color <- stripe_colour
   }
   if(!is.null(text_colour)){
      text_color <- text_colour
   }

  # Stripe fill
  stripe_fill <- gsub(" ", "", tolower(stripe_fill)) # Standardize
  stripe_fill <- if(stripe_fill %in% names(return_full_palette())){
    fauna_colors(stripe_fill)} else { stripe_fill } # If color matches a fauna_color, use that

  # Stripe text color
  stripe_color <- gsub(" ", "", tolower(stripe_color)) # Standardize
  stripe_color <- if(stripe_color %in% names(return_full_palette())){
    fauna_colors(stripe_color)} else { stripe_color } # If color matches a fauna_color, use that

  # Main text color
  text_color <- gsub(" ", "", tolower(text_color)) # Standardize
  text_color <- if(text_color %in% names(return_full_palette())){
    fauna_colors(text_color)} else { text_color } # If color matches a fauna_color, use that


  data <- data %>%
    select({{char_var}}, everything()) %>%
    mutate(cat_indicator = case_when(
      {{char_var}} %in% chars ~ 1,
      TRUE ~ 0
    ))

  data <- data %>% mutate(groupr = case_when(
    cat_indicator == 1 ~ as.character({{char_var}}),
    TRUE ~ as.character(NA)
  ),
  traitr = case_when(
    cat_indicator == 0 ~ as.character({{char_var}}),
    TRUE ~ "category_name"
  )) %>%
    mutate(groupr = na.locf(groupr),
           groupr = factor(groupr, levels = chars),
           traitr_order = 1:n() #,
           # traitr = factor(traitr, levels = unique(append("category_name", traitr)))
    )

    data <- data %>%
    arrange(groupr, traitr_order) %>%
    select(-groupr, -traitr_order, -traitr)

 if(char_header_blank){
   data <- data %>% rename(" " = {{char_var}})
 }

 group_vec <- data %>% ungroup() %>% mutate(row_numr = 1:n()) %>%
   filter(cat_indicator == 1) %>% pull(row_numr)

 data <- data %>%
   mutate(across(where(is.character), \(x){
     x = gsub("\\s\\n\\s", "<br>", x)
   } ))

 foo <- data %>%
   select(-cat_indicator) %>%
   table_format() %>%
   tab_style(
     style = list(cell_text(align = "center", color = text_color),
                  cell_fill(color = "white")),
     locations = list(cells_body(),
                      cells_column_labels())
   ) %>%
   tab_style(
     style = list(
       cell_fill(color = stripe_fill),
       cell_text(color = stripe_color, weight = "bold")
     ),
     locations = list(cells_body(rows = group_vec))
   ) %>%
   tab_style(
     style = list(cell_text(align = "left", weight = "bold")),
     locations = list(cells_body(columns = 1),
                      cells_column_labels(columns = 1))
   )

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

 return(foo)

}


