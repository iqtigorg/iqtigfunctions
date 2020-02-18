#' Returns the minimum or maximum of a vector
#'
#' If the sequence is empty, it returns a type stable NA. NAs are ignored.
#'
#' @param x a vector
#' @examples
#' minimum(1:10)
#' minimum(NA)
#' @rdname min_max
#' @export
minimum <- function(x) {
  minmax(min, x)
}

#' @rdname min_max
#' @export
maximum <- function(x) {
  minmax(max, x)
}

#' Sums up multiple variables by row
#'
#' Similar to \code{\link{rowSums}}, but treats NAs as 0 and is intented to
#' accept multiple vectors.
#'
#' @param ... an arbitrary number of numeric or logical 1D vectors
#'
#' @return a numeric vector >= 0
#'
#' @examples
#'
#' row_sums(1:9, 9:1, c(1, 0, NA))
#'
#' \dontrun{
#' row_sums(GEHTRAINING, HILFSMITTEL, MEDIKATION, STURZRISIKO)
#' }
#'
#' @export
row_sums <- function(...) {
  rowSums(cbind(...), na.rm = TRUE)
}

allowed_minmax_types <- c(
  "numeric", "logical", "character",
  "integer", "Date", "POSIXt"
)
minmax <- function(fun, x) {
  if (!inherits(x, allowed_minmax_types)) {
    stop(
      "Data type ", paste0(class(x), collapse = ","), " is not supported",
      call. = FALSE
    )
  }

  values <- x[!is.na(x)]
  not_empty <- length(values) > 0L
  if (not_empty) {
    return(fun(values))
  }
  if (is.integer(x)) {
    NA_integer_
  } else if (is.character(x)) {
    NA_character_
  } else if (is.numeric(x)) {
    NA_real_
  } else if (is.logical(x)) {
    NA_integer_
  } else if (inherits(x, "Date")) {
    as.Date(NA)
  } else if (inherits(x, "POSIXct")) {
    copy_timezone(x, as.POSIXct(NA))
  } else if (inherits(x, "POSIXlt")) {
    copy_timezone(x, as.POSIXlt(NA))
  } else {
    # unreachable
    stop("This type is not supported in min/max", call. = FALSE) # nocov
  }
}

copy_timezone <- function(from, to) {
  tzone <- attr(from, "tzone", exact = TRUE)
  if (is.character(tzone) && length(tzone) == 1L && nchar(tzone) > 0L) {
    attr(to, "tzone") <- tzone
  }
  to
}
