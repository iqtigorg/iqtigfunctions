#' Replace NAs
#'
#' Replaces all NAs in the first argument by the value in the second argument.
#'
#' Fails if both arguments do not have the same type.
#'
#' @param x a vector of values with potential NAs
#' @param by a replacement value of the same type as x
#'
#' @return A vector of the same length as x with NAs replaced by 'by'
#'
#' @examples
#' replace_na(c(20, NA, 10), 15)
#'
#' @export
replace_na <- function(x, by) {
  stopifnot(length(by) == 1L, !is.na(by))
  if (is.integer(x) && is.numeric(by) && all(as.integer(by) == by)) {
    by <- as.integer(by)
  } else if (!is.integer(x) && is.numeric(x) && is.integer(by)) {
    by <- as.numeric(by)
  } else if (!setequal(class(x), class(by))) {
    stop("x and y should have the same datatype.\nCurrently you try to",
      " replace values of type ", paste(class(x), collapse = ", "),
      " by values of type ", paste(class(by), collapse = ", "),
      call. = FALSE
    )
  }
  x[is.na(x)] <- by
  x
}
