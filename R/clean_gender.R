#' Create recoded gender column
#'
#' This function takes a column of gender values and, in a new column,
#' returns the categorized values included in gender_walk.
#' @param x Data.frame containing gender data
#' @param var Original variable containing gender data. Must be specified
#' @param new_var Name of new gender variable. Defaults to gender_clean
#' @return A data.frame
#' @export
#'
#' @examples head(test_data) %>% clean_gender(gender) %>% select(gender, gender_clean)
#'
clean_gender <- function(x, var, new_var = gender_clean){
  res <- x |>
    rowwise() |>
    mutate(
      {{new_var}} := gender_walk$clean[match( toupper({{ var }}), toupper(gender_walk$original))]
    ) |>
    ungroup()

  no_match_full <- res |> select({{new_var}}, {{var}}) |>
    filter(is.na({{new_var}})) |>
    pull({{var}})

  na_count <- no_match_full |> is.na() |> sum()

  no_match_short <- no_match_full |> unique() |> na.omit() |> as.vector() |>
    paste(collapse = "\n")
  no_match_len <- no_match_full |> unique() |> na.omit() |> as.vector() |> length()

    if(no_match_len > 0){
    message(paste0("The following values are not in the gender walk dictionary and have been assigned a value of NA in the newly created variable:\n",
                           no_match_short, "\n\n"))
    }

  if(na_count > 0){
    warning(paste0("Note: Original gender variable contained ", na_count, " NA value(s)\n\n"))
  }

  return(res)
}
