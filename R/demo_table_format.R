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
#' @param return_html If TRUE, returns raw HTML of table. FALSE by default
#' @param include_css If TRUE, returns inline CSS for table formatting. TRUE by default. This is only returned if return_html is also TRUE
#' @param write If TRUE, write results to the file specified in the path argument. FALSE by default.
#' @param path File path to be written to if write is TRUE. "table.txt" in working directory by default.
#' @param ... Other arguments
#' @return An HTML table or raw HTML
#' @export
#' @importFrom zoo na.locf
#' @examples demo_table_format(df,
#' char_var = Trait,
#' chars = c("Income", "Education", "State", "Gender"))
demo_table_format <- function(data, char_var = Characteristic,
                              chars = c("Gender", "Race", "Region", "Companion Animals", "Went Fishing", "Handled Chickens"),
                              char_header_blank = FALSE, stripe_fill = "lightblue",
                              stripe_color = "white", text_color = "darkgray",
                              return_html = FALSE, include_css = TRUE,
                              write = FALSE, path = "table.txt",
                              ...){

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
          traitr = factor(traitr, levels = unique(append("category_name", traitr)))
          )

 data <- data %>%
   arrange(groupr, traitr) %>%
   select(-groupr, -traitr)

 if(char_header_blank){
   data <- data %>% rename(" " = {{char_var}})
 }

 group_vec <- data %>% ungroup() %>% mutate(row_numr = 1:n()) %>%
   filter(cat_indicator == 1) %>% pull(row_numr)

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
     style = list(cell_text(align = "left")),
     locations = list(cells_body(columns = 1),
                      cells_column_labels(columns = 1))
   )

 if(return_html){
    foo <- return_html(foo, include_css = include_css, write = write, path = path)
 }

 return(foo)

}


