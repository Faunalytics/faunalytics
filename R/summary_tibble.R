#' Make a summary tibble out of model or model summary object
#'
#' This function takes a lm or glm model or model summary object and returns a 
#' tibble with key measures of interest (e.g., coefficient estimate, p-values).
#' These measures may change depending on the type of model.
#' @param x Model or model summary object.
#' @param stars Create 'stars' column containing asterisks indicating significance.
#' @param clean Clean names using janitor::clean_names. FALSE by default.
#' @param p_name Optional. Name of p-value column. If p-values are not in a 
#' column named either "Pr(>|t|)" or "Pr(>|z|)", the `stars` argument will not 
#' work without this argument.
#' @return A tibble
#' @export
#'
#' @examples m1 <- lm(mpg ~ cyl + hp + wt, data = mtcars)
#' summary_tibble(m1)

summary_tibble <- function(x, stars = TRUE, clean = FALSE, p_name = NULL){
  
  # If x is a summary object, extract 'coefficients' matrix
  if(any(grepl("summary", class(x)))){
    foo <- x[['coefficients']]
  } else{ # If not, make a summary object and extract 'coefficients' matrix
    foo <- summary(x)[['coefficients']]
  }
  
  foo <- foo |> 
    as.data.frame() |> # Convert to data.frame
    rownames_to_column("coef") |> # Keep rownames (coefficient names)
    as_tibble() # Convert to tibble
  
  if(stars){
    if(is.null(p_name)){
      p_name <- ifelse("Pr(>|t|)" %in% names(foo), "Pr(>|t|)",
                       ifelse("Pr(>|z|)" %in% names(foo), "Pr(>|z|)",
                              "p_name"))
    }
    foo <- foo |> 
      rename(p_val = as.name(p_name)) |> 
      mutate(stars = case_when(
        p_val <= 0.001 ~ "***",
        p_val > 0.001 & p_val <= 0.01 ~ "**",
        p_val > 0.010 & p_val <= 0.05 ~ "*",
        p_val > 0.050 & p_val <= 0.10 ~ ".",
        TRUE ~ "")) |> 
      rename(!!quo_name(p_name) := p_val)
  }
  
  if(clean){
    foo <- janitor::clean_names(foo)
  }
  
  return(foo)
}
