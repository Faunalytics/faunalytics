#' Fish beliefs data set, abridged
#'
#' A dataset containing survey results from Canadian participants who were asked
#' to report their beliefs about fishes and if they would pledge to reduce their fish consumption
#'
#' @name test_data
#' @format A data frame with 576 rows and variables:
#' \describe{
#'   \item{date}{date of survey completion}
#'   \item{speed}{number of seconds taken to complete survey}
#'   \item{gender}{gender selected by participant}
#'   \item{gender_write_in}{additional gender write-in information}
#'   \item{gender_group}{grouped gender data}
#'   \item{race_ethnicity}{race selected by participant}
#'   \item{race_ethnicity_write_in}{additional race write-in information}
#'   \item{age}{participant age}
#'   \item{income}{income range selected by participant}
#'   \item{education}{education selected by participant}
#'   \item{latinx}{Latinx ethnicity identifier }
#'   \item{belief_1}{level of agreement: "Fish can learn"}
#'   \item{belief_2}{level of agreement: "Fish don't mind being in a barren environment"}
#'   \item{belief_3}{level of agreement: "Fish don't care about being over-crowded"}
#'   \item{belief_4}{level of agreement: "Fish never find it stressful to be picked up or handled"}
#'   \item{ffq_1}{Frequency of consumption: Dairy}
#'   \item{ffq_2}{Frequency of consumption: Chicken}
#'   \item{ffq_3}{Frequency of consumption: Turkey}
#'   \item{ffq_4}{Frequency of consumption: Fish and seafood}
#'   \item{ffq_5}{Frequency of consumption: Pork}
#'   \item{ffq_6}{Frequency of consumption: Beef}
#'   \item{ffq_7}{Frequency of consumption: Other meat}
#'   \item{ffq_8}{Frequency of consumption: Eggs}
#'   \item{outcome}{pledged to reduce consumption of fish products}
#' }
#' @source \url{https://osf.io/n452j/}
#'


load("data/test_data.rda")

usethis::use_data(test_data, overwrite = T)
