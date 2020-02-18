#' Binary comparison operators
#'
#' Operators that perform vectorized comparisons, treating \code{NA}s as
#' \code{FALSE}.
#'
#' However, equality tests between two \code{NA} values yield \code{TRUE}.
#' In that case a warning is issued.
#'
#' @param lhs the left hand side of the operator
#' @param rhs the right hand side of the operator
#'
#' @rdname comparison_operators
#'
#' @examples
#' 2 %==% 2
#'
#' 2 %==% 3
#'
#' 2 %==% NA
#'
#' NA %==% NA
#'
#' @export
`%==%` <- function(lhs, rhs) {
  stopifnot(
    !(length(lhs) > 1L && length(rhs) > 1L) || length(lhs) == length(rhs)
  )

  na_lhs <- is.na(lhs)
  na_rhs <- is.na(rhs)
  both_nas <- na_lhs & na_rhs

  warn_both_nas("%==%", "TRUE", both_nas)

  ifelse_internal(na_lhs | na_rhs, both_nas, lhs == rhs)
}

#' @rdname comparison_operators
#' @export
`%!=%` <- function(lhs, rhs) {
  stopifnot(
    !(length(lhs) > 1L && length(rhs) > 1L) || length(lhs) == length(rhs)
  )

  na_lhs <- is.na(lhs)
  na_rhs <- is.na(rhs)
  any_na <- na_lhs | na_rhs
  both_nas <- na_lhs & na_rhs
  not_both_na <- !both_nas

  warn_both_nas("%!=%", "FALSE", both_nas)

  ifelse_internal(any_na, not_both_na, lhs != rhs)
}

#' @rdname comparison_operators
#' @export
`%>=%` <- function(lhs, rhs) {
  stopifnot(
    !(length(lhs) > 1L && length(rhs) > 1L) || length(lhs) == length(rhs)
  )

  na_lhs <- is.na(lhs)
  na_rhs <- is.na(rhs)
  both_nas <- na_lhs & na_rhs

  warn_both_nas("%>=%", "TRUE", both_nas)

  ifelse_internal(na_lhs | na_rhs, both_nas, lhs >= rhs)
}

#' @rdname comparison_operators
#' @export
`%<=%` <- function(lhs, rhs) {
  stopifnot(
    !(length(lhs) > 1L && length(rhs) > 1L) || length(lhs) == length(rhs)
  )

  na_lhs <- is.na(lhs)
  na_rhs <- is.na(rhs)
  both_nas <- na_lhs & na_rhs

  warn_both_nas("%<=%", "TRUE", both_nas)

  ifelse_internal(na_lhs | na_rhs, both_nas, lhs <= rhs)
}

warn_both_nas <- function(operator, result, both_nas) {
  if (any(both_nas)) {
    warning("NA ", operator, " NA leads to ", result, ". ",
      "However you should avoid comparing NA with NA as ",
      "it can have unwanted consequences.",
      call. = FALSE
    )
  }
}

#' @rdname comparison_operators
#' @export
`%<%` <- function(lhs, rhs) {
  stopifnot(
    !(length(lhs) > 1L && length(rhs) > 1L) || length(lhs) == length(rhs)
  )
  condition <- is.na(lhs) | is.na(rhs)
  ifelse_internal(condition, rep.int(FALSE, length(condition)), lhs < rhs)
}

#' @rdname comparison_operators
#' @export
`%>%` <- function(lhs, rhs) {
  stopifnot(
    !(length(lhs) > 1L && length(rhs) > 1L) || length(lhs) == length(rhs)
  )
  condition <- is.na(lhs) | is.na(rhs)
  ifelse_internal(condition, rep.int(FALSE, length(condition)), lhs > rhs)
}

#' Between
#'
#' Checks if a value is between two values (limits included). It treats
#' \code{NA} as \code{FALSE}.
#'
#' @param lhs left hand side
#' @param rhs right hand side, needs to be a numeric vector of length 2 with no
#'   NAs.
#'
#' @return logical vector of length `length(lhs)`
#' @export
`%between%` <- function(lhs, rhs) {
  stopifnot(length(rhs) == 2L, !anyNA(rhs))
  (lhs %>=% rhs[1L]) & (lhs %<=% rhs[2L])
}

#' Match value lists against patterns
#'
#' Checks if any|all elements on the left hand side are matched exactly by one
#' of the patterns on the right hand side.
#'
#' @examples
#' list("abcde", c("bcd", "abc")) %any_like% "ab%"
#'
#' @param lhs a character/integer vector or a list of character/integer vectors.
#' Make sure all list elements have the same type as \code{rhs}
#' @param rhs a character/integer vector. In case of a character vector,
#' wildcard values \code{\%} can be used
#'
#' It is recommended that all strings only contain ASCII characters.
#'
#' @return A logical vector with the same length as 'lhs'
#' @rdname any_all_in_like
#' @export
`%any_like%` <- function(lhs, rhs) {
  all_any_like(lhs, rhs, any_aggregation)
}

#' @examples
#' list("abcde", c("bcd", "abc")) %all_like% "ab%"
#'
#' @rdname any_all_in_like
#' @export
`%all_like%` <- function(lhs, rhs) {
  all_any_like(lhs, rhs, all_aggregation)
}

#' @examples
#' list("abcde", c("bcd", "abc")) %any_in% c("abc", "abcde")
#'
#' @rdname any_all_in_like
#' @export
`%any_in%` <- function(lhs, rhs) {
  all_any_in(lhs, rhs, any_aggregation)
}

#' @examples
#' list("abcde", c("bcd", "abc")) %all_in% c("abc", "abcde")
#'
#' @rdname any_all_in_like
#' @export
`%all_in%` <- function(lhs, rhs) {
  all_any_in(lhs, rhs, all_aggregation)
}

all_any_like <- function(lhs, rhs, aggregate_function) {
  stopifnot(!is.null(rhs), is.character(rhs))
  if (is.character(lhs)) {
    lhs <- as.list(lhs)
  }
  stopifnot(is.list(lhs))

  patterns_without_wildcard <- gsub(
    pattern = "%",
    replacement = "",
    x = rhs,
    fixed = TRUE
  )
  if (any(nchar(patterns_without_wildcard) == 0L, na.rm = TRUE)) {
    stop("The right-hand side needs to have at least one non-empty character",
      call. = FALSE
    )
  }

  patterns <- build_regexp(rhs)

  lhs_vec <- unlist(lhs, recursive = FALSE, use.names = FALSE)

  match_vec <- Reduce(function(acc, pattern) {
    acc | grepl(
      pattern = pattern,
      x = lhs_vec,
      ignore.case = TRUE,
      perl = TRUE
    )
  }, x = patterns, init = rep.int(FALSE, length(lhs_vec)))

  aggregate_final_results(aggregate_function, lhs, match_vec)
}

aggregate_final_results <- function(aggregate_function, lhs, match_vec) {
  lhs_lengths <- lengths(lhs, use.names = FALSE)
  result_vec <- fold_vector(lhs, lhs_lengths, as.numeric(match_vec))
  aggregate_function(result_vec, lhs_lengths)
}

fold_vector <- function(lhs, lhs_lengths, match_vec) {
  match_vec <- cumsum(match_vec)
  offsets <- cumsum(lhs_lengths)
  result_vec <- numeric(length(lhs))
  result_vec[which(offsets > 0)] <- match_vec[offsets]
  result_vec <- result_vec - c(0, result_vec[seq_len(length(lhs)) - 1L])
  result_vec
}

all_aggregation <- function(result_vec, lhs_lengths) {
  result_vec == lhs_lengths
}

any_aggregation <- function(result_vec, lhs_lengths) {
  result_vec > 0
}

build_regexp <- function(patterns) {
  patterns <- escape_special_characters(patterns)
  patterns <- paste0("^", patterns, "$")

  # we need to make sure the regexp does not become too large so we form
  # groups of maximum 500 codes
  n_patterns <- length(patterns)
  n_groups <- ceiling(n_patterns / 500)
  pattern_groups <- split(patterns, seq_len(n_patterns) %% n_groups)
  lapply(pattern_groups, function(x) {
    paste0(wildcards_to_regexp(x), collapse = "|")
  })
}

escape_special_characters <- function(patterns) {
  regexp_special_chars <- c(
    "\\", "^", "$", ".", "+","?", "*",
    "(", ")", "[", "]", "{", "}", "|"
  )
  for (char in regexp_special_chars) {
    patterns <- gsub(char, paste0("\\", char), x = patterns, fixed = TRUE)
  }
  patterns
}

wildcards_to_regexp <- function(patterns) {
  gsub(pattern = "%", replacement = ".*", x = patterns, fixed = TRUE)
}

all_any_in <- function(lhs, rhs, aggregate_function) {
  if (!is.list(lhs)) {
    lhs <- as.list(lhs)
  }
  stopifnot(length(rhs) > 0L)
  if (anyNA(rhs)) {
    stop(
      "The ride-hand side contains NAs. This is not allowed.",
      .call = FALSE
    )
  }
  if (is.character(rhs)) {
    rhs <- tolower(rhs)
  }

  lhs_vec <- unlist(lhs, recursive = FALSE, use.names = FALSE)
  if (is.character(lhs_vec)) {
    lhs_vec <- tolower(lhs_vec)
  }

  match_vec <- lhs_vec %in% rhs

  aggregate_final_results(aggregate_function, lhs, match_vec)
}

ifelse_internal <- function(test, lhs, rhs) {
  # it is assumed that there are no NAs and all inputs are logical
  if (length(test) == 0L) {
    return(test)
  }
  res <- rhs
  res[test] <- lhs[test]
  res
}
