#' Gather composite fields
#'
#' Certain fields can have multiple values that are often encoded in a sequence
#' of columns, typically named FIELD_1, FIELD_2, FIELD_3, etc. This function
#' identifies these columns and collapses them into a single list column
#' "FIELD". This is in particular necessary for \code{\link{\%any_like\%}},
#' \code{\link{\%all_like\%}} etc., which expect such columns as input.
#'
#' @param data a data frame
#' @examples
#' data <- data.frame(
#'   OPSCHLUESSEL_1 = c("C5923", "C5923", "C575"),
#'   OPSCHLUESSEL_2 = c("C599.3", "C59AB", NA_character_),
#'   OPSCHLUESSEL_3 = NA_character_,
#'   TDS_B = c(1, 2, 3),
#'   stringsAsFactors = FALSE
#' )
#' data <- gather_composite_fields(data)
#' data$OPSCHLUESSEL
#'
#' @export
gather_composite_fields <- function(data) {
  stopifnot(is.data.frame(data))
  col_names <- colnames(data)
  composite_fields <- grepl(
    x = col_names,
    pattern = "^([a-zA-Z0-9]+)_([0-9]+)$"
  )
  composite_fields <- col_names[composite_fields]

  # we are interested in the first part of the composite
  # fields (e.g. *OPSCHLUESSEL*_1)
  composite_field_base_names <- unique(gsub("_.*$", "", composite_fields))

  # for each composite field base name, we find all associated composite fields
  # and combine them into a character vector per row.
  # The result is a list column with a character vector for each row
  for (field in composite_field_base_names) {
    col_match <- grepl(
      x = col_names,
      pattern = paste0("^", field, "_([0-9]+)$")
    )

    # we also check if the sequence is indeed a number ranging from 1 to n
    # if not, we do not convert that field
    composite_field_numbers <- sort(vapply(
      X = strsplit(col_names[col_match], split = "_", fixed = TRUE),
      FUN = function(x) as.integer(x[[2L]]),
      FUN.VALUE = integer(1L)
    ))
    not_a_sequence <- any(
      composite_field_numbers != seq_along(composite_field_numbers)
    )
    if (not_a_sequence) {
      # not a valid composite field
      # let's exclude it from the list
      composite_fields <- setdiff(composite_fields, col_names[col_match])
      next
    }

    # gather all values in a list, transpose it and then filter out all NAs
    values <- lapply(col_names[col_match], function(x) data[[x]])

    col_types <- vapply(values, typeof, character(1L))
    all_same_type <- length(unique(col_types)) == 1L
    if (!all_same_type) {
      warning("Composite fields ", field, " have differenty types",
        call. = FALSE
      )
    }
    values <- stats::setNames(do.call(Map, c(list(f = c), values)), NULL)
    values <- lapply(values, function(x) x[!is.na(x)])
    data[[field]] <- values
  }
  cols_to_retain <- setdiff(colnames(data), composite_fields)
  data[, cols_to_retain, drop = FALSE]
}
