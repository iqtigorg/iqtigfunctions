#' Aggregate values within groups
#'
#' @param lhs an aggregation expression
#' @param rhs a single name or a vector of names, where 'name' refers to the
#'   data type \code{name}, i.e. a quoted symbol.
#'
#' The function dynamically constructs a data.frame based on the values that are
#' used within the two expressions. It is assumed that all symbols in the
#' expression refer to character or numeric vectors of the same length in the
#' calling environment.
#'
#' @examples
#' with(mtcars, max(hp) %group_by% cyl)
#'
#' with(mtcars, max(hp) %group_by% c(cyl, drat))
#'
#' @import data.table
#' @export
`%group_by%` <- function(lhs, rhs) {
  envir <- parent.frame()
  lhs <- substitute(lhs)
  rhs <- substitute(rhs)

  # first we look at the two expressions and gather all variable names
  # that might be relevant for the computation
  relevant_values <- function(x) {
    is_relevant_value(envir, x)
  }
  lhs_names <- keep(relevant_values, of = gather_names(lhs))
  rhs_names <- keep(relevant_values, of = gather_names(rhs))
  relevant_variable_names <- unique(c(lhs_names, rhs_names))
  if (is_empty(rhs_names)) {
    stop(
      "The right-hand side of %group_by% does not have any valid names",
      call. = FALSE
    )
  }

  # then we create a data.table with only these relevant variables
  data <- setDT(
    build_data_frame_from_variables(envir, relevant_variable_names)
  )

  # and finaly we construct a data.table group by operation on that
  # data frame.
  grouped_computation(envir, data, lhs, by = rhs_names)
}

grouped_computation <- function(envir, data, lhs, by) {
  expr <- bquote(data[, result_col := .(lhs), by = .(by)])
  execution_environment <- new.env(hash = FALSE, parent = envir)
  execution_environment[["data"]] <- data
  eval(expr, envir = execution_environment)

  results <- data[["result_col"]]
  orignal_order <- order(data[["original_row_number"]])
  results[orignal_order]
}

is_empty <- function(vec) {
  length(vec) == 0L
}

gather_names <- function(expr) {
  if (is.name(expr)) {
    as.character(expr)
  } else if (length(expr) > 1L) {
    as.character(unique(unlist(sapply(expr, gather_names))))
  } else {
    character(0L)
  }
}

is_relevant_value <- function(envir, variable_name) {
  if (!is.null(value <- get0(variable_name, envir = envir))) {
    return(length(value) > 0L &&
             (is.character(value) || is.numeric(value) || is.logical(value) ||
                inherits(value, c("Date", "POSIXct"))))
  }
  FALSE
}

keep <- function(predicate, of) {
  test <- vapply(of, predicate, logical(1L))
  of[test]
}

build_data_frame_from_variables <- function(envir, variable_names) {
  data <- lapply(variable_names, function(x) {
    get0(x, envir = envir)
  })
  names(data) <- variable_names
  data <- as.data.table(data)
  data[["original_row_number"]] <- seq_len(nrow(data))
  data
}
