describe("replace_na", {
  it("replaces NAs by values", {
    expect_equal(replace_na(c(1, NA_real_), 3), c(1, 3))
  })
  it("fails if x and y are not of the same type", {
    expect_error(replace_na(c(1, NA_real_), "3"), "type")
  })
  it("fails if y is length != 1", {
    expect_error(replace_na(c(1, NA_real_), c(1, 2)))
  })
  it("fails if y is NA", {
    expect_error(replace_na(c(1, NA_real_), NA_real_))
  })
  it("implicitly converts integers to numerics", {
    expect_identical(replace_na(c(NA_real_, 1), -1L), c(-1, 1))
  })
  it("implicitly converts numerics to integers when possible", {
    expect_identical(replace_na(c(NA_integer_, 1L), -1), c(-1L, 1L))
  })
  it("does not implicitly convert numerics to integers when impossible", {
    expect_error(replace_na(c(NA_integer_, 1L), -1.5), "type")
  })
})
