#' Apply Benjamini-Hochberg adjustment to model summary object
#'
#' This function takes a model summary object (e.g., summary(model)) and
#' returns a tibble with the Benjamini-Hochberg adjustment for false discovery
#' rate applied. New columns include p-value rank (`rank`), adjusted alpha
#' (`adj_a`), adjusted p-values (`p_bh`), star indicators for significance level
#' (`stars_bh`), and a logical indicator of significance with the 
#' Benjamini-Hochberg adjustment applied (`sig_bh`).
#' @param x Model summary object. For example, summary(model). 
#' @param fdr False discovery rate. 0.05 by default.
#' @param p_name Name of p-value column in the summary object. "Pr(>|t|)" by
#' default.
#' @param coef_vec Optional. A vector of coefficient names as strings.
#' p-values associated with the coefficients in this vector will be included in 
#' the adjustment and all other coefficients will be excluded.
#' @return A tibble
#' @export
#'
#' @examples m1 <- lm(mpg ~ cyl + hp + wt, data = mtcars)
#' sum_m1 <- summary(m1)
#' bh_adj(sum_m1)
#'

bh_adj <- function(x, # summary object (e.g., summary(model))
                   fdr = 0.05, # false discovery rate
                   p_name = "Pr(>|t|)", # p-value column name
                   # optional vector of coefficient names to adjust
                   coef_vec = NULL ){ 
  
  
  
  foo <- x[['coefficients']] |> # Take coefficients object from summary object
    unclass() |> # Unclass to allow for conversion to data.frame
    as.data.frame() |> # Convert to data.frame
    rownames_to_column("coef") |> # Keep rownames (coefficient names)
    as_tibble()
  
  # Check for incorrect p_name specification. Warn if incorrect. If
  # Pr(>|z|) should have been used, make suggestion.
  if(!(p_name %in% names(foo))){
    warning(paste0(p_name, " does not exist.", 
                   ifelse("Pr(>|z|)" %in% names(foo), 
                          " Try setting p_name to 'Pr(>|z|)'", ""))
    )}

  foo_int <- foo |> filter(coef == "(Intercept)") # preserve intercept row
  foo <- foo |> filter(coef != "(Intercept)") # remove intercept row
  
  if(!is.null(coef_vec)){
    foo <- foo |> filter(coef %in% coef_vec)
  }
  
  ## Reference: https://real-statistics.com/hypothesis-testing/familywise-error/benjamini-hochberg-and-benjamini-yekutieli-tests/
  
  k <- nrow(foo) # Get k (number of p-values)
  
  foo <- foo |> 
    rename(p = as.name(p_name)) |> # Rename for ease, evaluating p_name as var
    clean_names() |> # Standardize column naming for ease of reference
    arrange(p) |> # Arrange p-values in ascending order
    mutate(rank = 1:n(), # Set ranks
           adj_a = (rank / k) * fdr)  # Calculate adjusted alpha
  
  # Add Benjamini-Hochberg adjusted p-values
  foo$p_bh <- p.adjust(foo$p, method = "BH", n = nrow(foo))
  
  foo <- foo |> 
    mutate(stars_bh = case_when(
      p_bh <= 0.001 ~ "***",
      p_bh > 0.001 & p_bh <= 0.01 ~ "**",
      p_bh > 0.010 & p_bh <= 0.05 ~ "*",
      p_bh > 0.050 & p_bh <= 0.10 ~ ".",
      TRUE ~ ""),
           sig_bh = p < adj_a) # Test for significance 
  
  # Add/rename intercept row's columns for binding
  foo_int <- foo_int |> 
    mutate(rank = 0, # set rank to 0 for easier sorting
           adj_a = as.numeric(NA),
           p_bh = as.numeric(NA),
           stars_bh = as.character(NA),
           sig_bh = as.logical(NA)) |> 
    set_names(names(foo))
  
  foo <- foo |> 
    bind_rows(foo_int) |> # Append intercept row
    arrange(rank) |> # Sort so intercept is first
    mutate(rank = case_when( # Set intercept rank to NA
      rank == 0 ~ as.integer(NA),
      TRUE ~ rank
    ))
  
  return(foo)
}