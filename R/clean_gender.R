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
  x |>
    rowwise() |>
    mutate(
      {{new_var}} := gender_walk$clean[match( toupper({{ var }}), toupper(gender_walk$original))]
    ) |>
    ungroup()
}
