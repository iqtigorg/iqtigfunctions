test_that("composite fields are correctly converted to list columns", {
  data <- data.frame(
    OPSCHLUESSEL_2 = c("1", "1", NA_character_),
    OPSCHLUESSEL_1 = c(NA_character_, "42", NA_character_),
    OPSCHLUESSEL_3 = c("2", NA_character_, NA_character_),
    TEST_2 = 1:3,
    stringsAsFactors = FALSE
  )
  result <- gather_composite_fields(data)

  # removes spreaded columns
  expect_equal(colnames(result), c("TEST_2", "OPSCHLUESSEL"))

  # correctly gathers everything into a list
  expect_equal(result[["OPSCHLUESSEL"]], list(c("1", "2"), c("1", "42"), character(0L)))

  # does not treat fields with non-sequential numbering as composite fields
  expect_equal(result[["TEST_2"]], 1:3)

  # also works with data frames without composite fields
  result <- gather_composite_fields(mtcars)
  expect_equal(colnames(result), colnames(mtcars))
})

test_that("composite fields retain the original type", {
  data <- data.frame(
    OPSCHLUESSEL_1 = c(1L, 1L, NA_integer_),
    OPSCHLUESSEL_2 = c(NA_integer_, 42L, NA_integer_),
    OPSCHLUESSEL_3 = c(2L, NA_integer_, NA_integer_),
    stringsAsFactors = FALSE
  )
  result <- gather_composite_fields(data)
  expect_true(all(
    vapply(result[["OPSCHLUESSEL"]], is.integer, logical(1L))
  ))
})

test_that("composite fields work with numerics", {
  data <- data.frame(
    OPSCHLUESSEL_1 = c(1, 1, NA_real_),
    OPSCHLUESSEL_2 = c(NA_real_, 42, NA_real_),
    OPSCHLUESSEL_3 = c(2, NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )
  result <- gather_composite_fields(data)
  expect_true(all(
    vapply(result[["OPSCHLUESSEL"]], is.numeric, logical(1L))
  ))
})

test_that("composite fields with different types produce warnings", {
  data <- data.frame(
    OPSCHLUESSEL_1 = c(1L, 1L, NA_integer_),
    OPSCHLUESSEL_2 = as.character(c(NA_integer_, 42L, NA_integer_)),
    stringsAsFactors = FALSE
  )
  expect_warning(
    gather_composite_fields(data),
    "OPSCHLUESSEL", all = TRUE
  )
})
