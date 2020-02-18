describe("to_year", {
  it("extracts the year out of a date string", {
    expect_equal(
      to_year(c("2017-01-01", "2016-01-01", NA_character_)),
      c(2017L, 2016L, NA_integer_)
    )
  })
  it("extracts the year out of a date object", {
    expect_equal(to_year(as.Date("2017-01-01")), 2017L)
  })
  it("extracts the year out of our month.year format", {
    expect_equal(
      to_year(c("01.2016", "12.2017", NA_character_)),
      c(2016L, 2017L, NA_integer_)
    )
  })
  it("fails if the date is not the right format", {
    expect_error(to_year("test"))
    expect_error(to_year("14.2016"))
  })
  it("supports quarters with quarter/year format", {
    expect_equal(to_year(c("1/2016", "2/2017")), c(2016L, 2017L))
    expect_equal(to_year(c("4/2016", NA_character_)), c(2016L, NA_integer_))
  })
  it("supports input in the form yyyy", {
    expect_equal(to_year(c("2016", "2017")), c(2016L, 2017L))
  })
})
