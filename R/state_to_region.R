#' Create region variable from state input
#'
#' This function works with title case names of the 50 states and D.C., as well
#' as with all caps 2-letter abbreviations with and without periods (e.g., NJ and N.J.).
#' Default regions are defined by the U.S. Census.
#' @param data A data frame or data frame extension (e.g. a tibble)
#' @param state.var Variable containing state values. Defaults to state
#' @param region.var Variable to be created containing regions. Defaults to region
#' @param factorize A logical evaluation to TRUE or FALSE indicating whether to make the region.var a factor. Defaults to TRUE
#' @param region.levels An optional vector of the unique values (as character strings) that region.var might have taken. Defaults to c("Northeast", "Midwest", "South", "West")
#'
#' @return A data frame
#' @export
#' @import tibble
#'
#' @examples USArrests |> tibble::rownames_to_column("us_state") |>
#' state_to_region(us_state, us_region)
state_to_region <- function(data, state.var = state, region.var = region,
                            factorize = TRUE, region.levels = c(
                              "Northeast", "Midwest", "South", "West")){
  res <- mutate(data,
         {{ region.var }} := case_when(
           {{ state.var }} %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania",
                                  "CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA",
                                  "C.T.", "M.E.", "M.A.", "N.H.", "R.I.", "V.T.", "N.J.", "N.Y.", "P.A"
           ) ~ "Northeast",
           {{ state.var }} %in% c("Indiana", "Illinois", "Michigan", "Ohio", "Wisconsin", "Iowa", "Nebraska", "Kansas", "North Dakota", "Minnesota", "South Dakota", "Missouri",
                                  "IN", "IL", "MI", "OH", "WI", "IA", "NE", "KS", "ND", "MN", "SD", "MO",
                                  "I.N.", "I.L.", "M.I.", "O.H.", "W.I.", "I.A.", "N.E.", "K.S.", "N.D.", "M.N.", "S.D.", "M.O"
           ) ~ "Midwest",
           {{ state.var }} %in% c("Delaware", "District of Columbia", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia", "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas",
                                  "DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX",
                                  "D.E.", "D.C.", "F.L.", "G.A.", "M.D.", "N.C.", "S.C.", "V.A.", "W.V.", "A.L.", "K.Y.", "M.S.", "T.N.", "A.R.", "L.A.", "O.K.", "T.X."
           ) ~ "South",
           {{ state.var }} %in% c("Alaska", "California", "Hawaii", "Oregon", "Washington", "Arizona", "Colorado", "Idaho", "New Mexico", "Montana", "Utah", "Nevada", "Wyoming",
                                  "AK", "CA", "HI", "OR", "WA", "AZ", "CO", "ID", "NM", "MT", "UT", "NV", "WY",
                                  "A.K.", "C.A.", "H.I.", "O.R.", "W.A.", "A.Z.", "C.O.", "I.D.", "N.M.", "M.T.", "U.T.", "N.V.", "W.Y."
                                  ) ~ "West",
           TRUE ~ {{ state.var }}
         ))

  if(factorize){
    res <- mutate(res,
           {{ region.var }} := factor({{ region.var}}, levels = region.levels))
  }

  return(res)

}
