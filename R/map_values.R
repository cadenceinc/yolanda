#' Map Values
#'
#' Given a vector of values this returns the matched counterpart efficiently.
#' Instead of applying a function to a vector that may have duplicates, you
#' can apply the function to the unique values only and then pair them
#' accordingly.  A function is not needed or required and instead you could
#' create a map manually.
#'
#' This function only works when there is a unique pairing between old and
#' new vectors.  Therefore the 'map' must be a named vector of values where
#' no value occurs more than once.  Otherwise, the pairing will be ambiguous.
#' That said, there can be more than one value that has the same mapped
#' counterpart (e.g. name).
#'
#' Values that do not appear in the map are return as-is and in the same
#' position as the original vector
#'
#' @param values character vector
#' @param map named character vector - with restrictions
#' @param replacement replacement value.  If NULL (default) the original value
#'  is returned.
#'
#' @return vector of equal length
#' @export
#'
#' @examples
#'
#'\dontrun{
#' # Duplicates cause failure
#' values <- 1:4
#' map    <- stats::setNames(c(1, 1:3), paste0("test", 1:4))
#' map_values(values, map)
#'
#' # Works
#' values <- 1:4
#' map    <- stats::setNames(1:4, letters[1:4])
#' map_values(values, map)
#'
#' # Works when length(map) > length(values)
#' values <- 1:3
#' map    <- stats::setNames(1:4, letters[1:4])
#' map_values(values, map)
#'
#' # Works when values are out of order
#' values <- c(1, 3, 4)
#' map    <- stats::setNames(1:4, letters[1:4])
#' map_values(values, map)
#'}
#'
map_values <- function(values, map, replacement = NULL) {
  replacement <- replacement[1]
  if (any(duplicated(map))) stop("There are duplicates in the map provided.")
  if (is.null(names(map))) stop("The map provided must be a named vector")

  ## coerce data to character.
  ## the matching function will return integers, and the results will be
  ## character no matter what.
  values <- as.character(values)
  map    <- stats::setNames(as.character(map), as.character(names(map)))

  ## find all matches and non-matches
  match_location    <- match(values, map, nomatch = 0)
  nomatch_positions <- which(match_location == 0)

  ## for all non-matches we would like to return the original values
  ## therefore, we modify the map to incorporate the necessary values
  ## and the re-match using the updated map
  map_additions <- unique(values[nomatch_positions])
  map_additions <- stats::setNames(map_additions, map_additions)
  if (!is.null(replacement)) {
    replacement <- as.character(replacement)
    r_len <- length(map_additions)
    map_additions <- stats::setNames(map_additions, rep(replacement, r_len))
  }
  map <- c(map, map_additions)

  match_location <- match(values, map, nomatch = 0)
  names(map)[match_location]
}

#' Map Function
#'
#' This is similar to mapping values except instead of providing two vectors
#' of equal length, you provide one vector and function.  The function
#' provided will be applied to the vector, and any values that cannot be
#' coerced will be returned as-is.
#'
#' @param values vector to be altered
#' @param .f function to apply to the provided vector
#'
#' @return vector of equal length
#' @export
#'
map_funct <- function(values, .f) {
  nms        <- unique(values)
  mapped     <- .f(nms)
  not_mapped <- which(is.na(mapped))
  mapped[not_mapped] <- nms[not_mapped]
  map_values(values, stats::setNames(nms, mapped))
}

#' Map Values - Snakecase
#'
#' Predefined version of 'map_funct' that uses snakecase::to_snake_case
#'
#' @param values character vector
#'
#' @return character vector of equal length that transforms the original
#'  vector to snakecase
#'
#' @export
#'
map_snake <- function(values) {
  map_funct(values, snakecase::to_snake_case)
}

#' Map Dates
#'
#' @param values character, usually a vector of dataframe names
#' @param style character, either 'monthly' or 'weekly'
#'
#' @return a vector of equal length as the ones provided with the values
#'  being transformed into a a readable format
#' @export
#'
map_date <- function(values, style = "monthly") {
  match.arg(style, c("monthly", "epiweek"))

  if (style == "monthly") {
    .f <- function(x) format(as.Date(x, "%Y-%m-%d"), "%b %Y")
  }

  if (style == "epiweek") {
    .f <- function(x) {
      wk <- lubridate::epiweek(as.Date(x, "%Y-%m-%d"))
      yr <- lubridate::epiyear(as.Date(x, "%Y-%m-%d"))
      gsub("NA NA", NA, paste(wk, yr))
    }
  }
  map_funct(values, .f)
}

#' Map Values - Partial
#'
#' Deprecated
#'
#' @param values vector of values to match/replace
#' @param map named character vector to pass to 'grep_v'
#' @param replacement replacement value.  If NULL (default) the original value
#'  is returned.
#'
#' @return vector of same length as the original value.
#' @export
#'
pmap_values <- function(values, map, replacement = NULL) {
  replacement <- replacement[1]
  new_map <-
    unlist(lapply(1:length(map), function(x) {
      ## find all unique matches in values
      rslt <- unique(grep_v(map[x],  values))
      ## assign name to those values
      stats::setNames(rslt, rep(names(map)[x], length(rslt)))
    }))

  map_values(values, new_map, replacement = replacement)
}



