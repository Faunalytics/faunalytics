#' Apply Benjamini-Hochberg adjustment to model summary object
#'
#' This function takes a model summary object (e.g., summary(model)) and
#' returns a tibble with the Benjamini-Hochberg adjustment for false discovery
#' rate applied. New columns include p-value rank (`rank`), adjusted alpha
#' (`adj_a`), and a logical indicator of significance with the 
#' Benjamini-Hochberg adjustment applied (`bh_sig`).
#' @param x Model summary object. For example, summary(model). 
#' @param fdr False discovery rate. 0.05 by default.
#' @param p_name Name of p-value column in the summary object. "Pr(>|t|)" by
#' default.
#' @return A tibble
#' @export
#'
#' @examples m1 <- lm(mpg ~ cyl + hp + wt, data = mtcars)
#' sum_m1 <- summary(m1)
#' bh_adj(sum_m1)
#'

bh_adj <- function(x, # summary object (e.g., summary(model))
                   fdr = 0.05, # false discovery rate
                   p_name = "Pr(>|t|)"
                   ){ # Drop intercept row
  
  foo <- x[['coefficients']] |> # Take coefficients object from summary object
    unclass() |> # Unclass to allow for conversion to data.frame
    as.data.frame() |> # Convert to data.frame
    rownames_to_column("coef") |> # Keep rownames (coefficient names)
    as_tibble()
  
  foo_int <- foo |> filter(coef == "(Intercept)") # preserve intercept row
  foo <- foo |> filter(coef != "(Intercept)") # remove intercept row
  
  ## Reference: https://real-statistics.com/hypothesis-testing/familywise-error/benjamini-hochberg-and-benjamini-yekutieli-tests/
  
  k <- nrow(foo) # Get k (number of p-values)
  
  foo <- foo |> 
    rename(p = as.name(p_name)) |> # Rename for ease, evaluating p_name as var
    clean_names() |> # Standardize column naming for ease of reference
    arrange(p) |> # Arrange p-values in ascending order
    mutate(rank = 1:n(), # Set ranks
           adj_a = (rank / k) * fdr, # Calculate adjusted alpha
           bh_sig = p < adj_a) # Test for significance 
  
  # Add/rename intercept row's columns for binding
  foo_int <- foo_int |> 
    mutate(rank = 0, # set rank to 0 for easier sorting
           adj_a = as.numeric(NA),
           bh_sig = as.logical(NA)) |> 
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
