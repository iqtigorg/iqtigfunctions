#' Extracts the year out of a date-like object
#'
#' Supports \code{mm.yyyy}, \code{[1-4]{1}/yyyy}, \code{yyyy} and uses
#' \code{\link{as.Date}} to detect other date-like formats.
#'
#' @param x a vector coercible to a character vector
#'
#' @return An integer vector of years
#'
#' @examples
#' to_year("03.1999")
#'
#' to_year("4/2013")
#'
#' to_year("2015")
#'
#' to_year("2019-03-01")
#'
#' @export
to_year <- function(x) {
  non_na_x <- x[!is.na(x)]

  is_mm.yyyy <- grepl(pattern = "^(0[1-9]|1[0-2])\\.\\d{4}$", x = non_na_x)
  if (all(is_mm.yyyy, na.rm = TRUE)) {
    return(as.integer(substr(x, start = 4L, stop = 7L)))
  }

  is_quarter_slash_yyyy <- grepl(pattern = "^[1-4]{1}/\\d{4}$", x = non_na_x)
  if (all(is_quarter_slash_yyyy, na.rm = TRUE)) {
    return(as.integer(substr(x, start = 3L, stop = 7L)))
  }

  is_yyyy <- grepl(pattern = "^\\d{4}$", x = non_na_x)
  if (all(is_yyyy, na.rm = TRUE)) {
    return(as.integer(x))
  }

  as.integer(format(as.Date(x), "%Y"))
}
