#' Gender recoding data
#'
#' A dataset containing open-ended responses to the question "What is your gender?"
#' and the category each falls into for consistency.
#'
#' @name gender_walk
#' @format A data frame with 20 rows and 2 variables:
#' \describe{
#'   \item{original}{open-ended response}
#'   \item{bucket}{Faunalytics category}
#'   \item{clean}{Faunalytics category; Same as bucket, but preserved to allow deprecated clean_gender to work}
#' }
#'


load("data/gender_walk.rda")

usethis::use_data(gender_walk, overwrite = T)
