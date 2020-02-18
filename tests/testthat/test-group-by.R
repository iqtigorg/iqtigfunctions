describe("group_by", {
  it("applies a function within a group", {
    expect_silent(result <- with(mtcars, maximum(hp) %group_by% cyl))
    expected <- dplyr::group_by(mtcars, cyl)
    expected <- dplyr::mutate(expected, res = max(hp))$res
    expect_equal(result, expected)
  })
  it("it supports multiple groups", {
    expect_silent(result <- with(mtcars, maximum(hp) %group_by% c(cyl, vs)))
    expected <- dplyr::group_by(mtcars, cyl, vs)
    expected <- dplyr::mutate(expected, res = max(hp))$res
    expect_equal(result, expected)
  })
  it("supports return values with length > 1L", {
    expect_silent(result <- with(mtcars, (hp * 2) %group_by% cyl))
    expect_equal(result, mtcars$hp * 2)
  })
  it("supports filtering groups", {
    data <- mtcars
    data$condition <- data$hp >= 100
    expect_silent(result <- with(data, maximum(mpg[condition]) %group_by% cyl))
    expected <- dplyr::group_by(data, cyl)
    expected <- dplyr::filter(expected, condition)
    expected <- dplyr::summarise(expected, res = max(mpg))
    expected <- dplyr::left_join(data, expected, by = "cyl")$res
    expect_equal(result, expected)
  })
  it("captures the defining environment", {
    fun <- function(x) x * 2
    result <- with(mtcars, fun(hp) %group_by% cyl)
    expect_equal(result, mtcars$hp * 2)
  })
  it("supports constant left-hand sides", {
    result <- with(mtcars, 1 %group_by% cyl)
    expect_equal(result, rep.int(1, nrow(mtcars)))
  })
  it("supports Date and POSIXct", {
    data <- mtcars
    data$date <- Sys.Date() - data$cyl
    data$dt <- Sys.time() - data$cyl
    expect_silent(result <- with(data, minimum(date) %group_by% cyl))
    expect_equal(
      result,
      dplyr::transmute(dplyr::group_by(data, cyl), res = minimum(date))$res
    )
    expect_silent(result <- with(data, minimum(dt) %group_by% cyl))
    expect_equal(
      result,
      dplyr::transmute(dplyr::group_by(data, cyl), res = minimum(dt))$res
    )
  })
  it("fails if columns do not exists", {
    expect_error(with(mtcars, maximum(hp) %group_by% notthere), "valid")
  })
  it("fails if the expr returns results with unexpected length", {
    expect_error(
      with(mtcars, (1:10) %group_by% cyl),
      regexp = "Supplied"
    )
  })
  it("fails if rhs is not a call or name", {
    expect_error(
      with(mtcars, (1:10) %group_by% 1),
      regexp = "The right-hand side of %group_by% does not have any valid names"
    )
  })
  it("fails if rhs is empty call", {
    expect_error(
      with(mtcars, (1:10) %group_by% c()),
      regexp = "The right-hand side of %group_by% does not have any valid names"
    )
  })
})
