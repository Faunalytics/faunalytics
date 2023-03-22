#' @title Return color(s) from the Faunalytics palette
#' @description Used to retrieve a single color or vector of colors from the Faunalytics color palette. In order, colors included are:
#' green, amber (or orange), red, dark blue, light blue, blue. The following are not part of the Faunalytics color palette, but will be returned
#' when requested: black, white, gray, light gray, dark gray. Variations of grays spelled with an "e" (e.g., dark grey) will also work.
#'
#' @param ... One of the following: color name(s) in quotes, a number of desired colors (no more than 6), or nothing.
#' Quoted colors must be a part of the Faunalytics color palette.
#' If no value is provided, this function will return the base color palette with names.
#' @param nameless Return values without names when needed. TRUE by default.
#' @param cbf The current Faunalytics color palette is not colorblind-friendly. Setting this argument to TRUE will override
#' the default Faunalytics palette and replace it with a colorblind-friendly palette (viridis::turbo). FALSE by default.
#' @return A named color hex code or named vector of color hex codes
#' @import viridis
#' @export
#'
#' @examples fauna_colors("green")
#' fauna_colors("orange", "lightblue")
#' fauna_colors(4)

fauna_colors <- function(..., nameless = TRUE, cbf = FALSE){

  # Define main Faunalytics colors
  main_col_vec <- c(
    `green` = "#73A753",
    `amber` = "#FF8F2D",
    `red` = "#E64B3D",
    `darkblue` = "#254C59",
    `lightblue` = "#5FB7E5",
    `blue` = "#0092B2"
  )

  # Define additional colors for function
  alt_col_vec <- c(
    `orange` = "#FF8F2D", # Same as `amber`
    `white` = "#FFFFFF",
    `black` = "#000000",
    `gray` = "#808080",
    `grey` = "#808080",
    `lightgray` = "#eeeeee",
    `lightgrey` = "#eeeeee",
    `darkgray` = "#333333",
    `darkgrey` = "#333333"
  )

  # Combine all colors for easier lookup
  col_vec <- append(main_col_vec, alt_col_vec)

  # Determine type of input and return proper result
  col_getter <- function(...){
    cols <- c(...)

    # Standardizes character responses to be lowercase and without spaces
    cols <- if(is.character(cols)){
      gsub(" ", "", tolower(cols))
    }

    # If input is numeric, set value to look up as appropriate
    if(grepl("\\d{1,}", deparse(substitute(...)))){
      cols <- as.numeric(c(...))
    }

    # Define return object
    col_res <- if (is.null(cols)){ # If blank...
      main_col_vec # return the main palette
    } else if(is.numeric(cols)) { # If numeric...
      main_col_vec[1:cols] # return 1 through [input] from the main palette. Any number over the max will return the main palette plus NAs
    } else if(length(cols) == 1) {
      if(cols == "force_return_full"){ # Non-documented functionality to return all colors recognized by this function
        col_vec
      } else {
        col_vec[cols]
      }
    } else { # Otherwise...
      col_vec[cols] # look up the input in the full color list
    }

    if(nameless){
      if("force_return_full" %in% cols){
      col_res <- unname(col_res)
      }
    }

    if(cbf){
      col_res <- viridis::turbo(length(col_res))
    }

    return(col_res)
  }

  col_getter(...)

}
