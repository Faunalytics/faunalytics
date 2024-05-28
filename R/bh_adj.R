#' Apply Benjamini-Hochberg adjustment to model summary object
#'
#' This function takes a lm or glm model object and returns a tibble with 
#' the Benjamini-Hochberg adjustment for false discovery
#' rate applied. New columns include p-value rank (`rank`), adjusted alpha
#' (`adj_a`), adjusted p-values (`p_bh`), star indicators for significance level
#' (`stars_bh`), and a logical indicator of significance with the 
#' Benjamini-Hochberg adjustment applied (`sig_bh`).
#' @param x Model summary object. For example, summary(model). 
#' @param fdr False discovery rate. 0.05 by default.
#' @param coef_vec Optional. A vector of coefficient names as strings.
#' p-values associated with the coefficients in this vector will be included in 
#' the adjustment and all other coefficients will be excluded.
#' @param p_name Optional. The name of the p-value column in the summary
#' object that results from running summary(x), where x is your model. By
#' default, the function will look for "Pr(>|t|)" and "Pr(>|z|)". If neither
#' of these is found and the correct `p_name` is not specified, an error will
#' result.
#' @return A tibble
#' @export
#'
#' @examples m1 <- lm(mpg ~ cyl + hp + wt, data = mtcars)
#' bh_adj(sum_m1)

bh_adj <- function(x, # model object (e.g., lm(mpg ~ cyl, data = mtcars))
                   fdr = 0.05, # false discovery rate
                   # optional vector of coefficient names to adjust
                   coef_vec = NULL,
                   # optional string of p-values column name in summary object
                   # (i.e., summary(x))
                   p_name = NULL){ 
  
  # Detect x class and throw warning if it's a summary object.
  if(any(grepl("summary", class(x)))){
    warning("x must be a model object, not a model summary object.")
  }
  
  foo <- summary(x)[['coefficients']] |> # Take coefficients matrix from summary
    as.data.frame() |> # Convert to data.frame
    rownames_to_column("coef") |> # Keep rownames (coefficient names)
    as_tibble() # Convert to tibble
  
  # Check for either "Pr(>|z|)" or "Pr(>|t|)" format p-values.
  # Store as p_foo_name
  
  if(is.null(p_name)){
    p_foo_name <- ifelse(any(grepl("Pr(>|z|)", names(foo), fixed = T)),
                  "Pr(>|z|)",
                  ifelse(any(grepl("Pr(>|t|)", names(foo), fixed = T)),
                         "Pr(>|t|)",
                         as.numeric(NA)))
    
  } else{ # Otherwise, use provided name
    p_foo_name <- p_name
  }
  
  # Warning message for incorrect p_name
  if(is.na(p_foo_name) | !(p_foo_name %in% names(foo))){
    warning("Incorrect p_name specification. Run the summary function on your
            model to find the proper p_name to use. See ?faunalytics::bh_adjust
            for more info.")
  }
  
  # Rename p-values column to "p"
  foo <- foo |> 
    rename(p = as.name(p_foo_name))
                       

  foo_int <- foo |> filter(coef == "(Intercept)") # preserve intercept row
  foo <- foo |> filter(coef != "(Intercept)") # remove intercept row
  
  # If specified, keep only rows for coefficients in coef_vec
  if(!is.null(coef_vec)){
    foo <- foo |> filter(coef %in% coef_vec)
  }
  
  ## Reference: https://real-statistics.com/hypothesis-testing/familywise-error/benjamini-hochberg-and-benjamini-yekutieli-tests/
  
  k <- nrow(foo) # Get k (number of p-values)
  
  foo <- foo |> 
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
