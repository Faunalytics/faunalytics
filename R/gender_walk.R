#' Gender recoding data
#'
#' A dataset containing open-ended responses to the question "What is your gender?"
#' and the category each falls into for consistency.
#'
#' @name gender_walk
#' @format A data frame with 20 rows and 2 variables:
#' \describe{
#'   \item{original}{open-ended response}
#'   \item{clean}{Faunalytics category}
#' }
#'


load("data/gender_walk.rda")

usethis::use_data(gender_walk, overwrite = T)
