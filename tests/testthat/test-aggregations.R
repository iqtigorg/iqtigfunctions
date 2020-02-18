length_0_date <- as.Date("2018-01-01")[FALSE]
length_0_utc_posixct <- as.POSIXct("2018-01-01", tz = "UTC")[FALSE]
length_0_utc_posixlt <- as.POSIXlt("2018-01-01", tz = "UTC")[FALSE]
na_utc_posixct <- as.POSIXct(c("2018-01-01", NA_character_), tz = "UTC")[2L]
na_utc_posixlt <- as.POSIXlt(c("2018-01-01", NA_character_), tz = "UTC")[2L]

describe("minimum", {
  it("returns the minimum and ignores NAs", {
    expect_equal(minimum(c(3, NA, 5, 293, 4, NA)), 3)
  })
  it("returns NA if length = 0", {
    expect_equal(minimum(integer()), NA_integer_)
    expect_equal(minimum(character()), NA_character_)
    expect_equal(minimum(numeric()), NA_real_)
    expect_equal(minimum(logical()), NA_integer_)
    expect_equal(minimum(length_0_date), as.Date(NA))
    expect_equal(minimum(length_0_utc_posixct), na_utc_posixct)
    expect_equal(minimum(length_0_utc_posixlt), na_utc_posixlt)
  })
  it("fails for non supported datatypes", {
    expect_error(minimum(1 + 1i), "supported")
    expect_error(minimum(c(1 + 1i, 1 + 2i)), "supported")
  })
  it("is type stable for base types", {
    for (i in 0:2) {
      expect_type(minimum(logical(i)), "integer")
      expect_type(minimum(numeric(i)), "double")
      expect_type(minimum(integer(i)), "integer")
      expect_s3_class(
        minimum(as.Date(integer(i), origin = "1970-01-01")),
        "Date"
      )
      expect_s3_class(
        minimum(as.POSIXct(integer(i), origin = "1970-01-01")),
        "POSIXct"
      )
    }
  })
})

describe("maximum", {
  it("returns the maximum and ignores NAs", {
    expect_equal(maximum(c(3, NA, 5, 293, 4, NA)), 293)
  })
  it("returns NA if length = 0", {
    expect_equal(maximum(integer()), NA_integer_)
    expect_equal(maximum(character()), NA_character_)
    expect_equal(maximum(numeric()), NA_real_)
    expect_equal(maximum(logical()), NA_integer_)
    expect_equal(maximum(length_0_date), as.Date(NA))
    expect_equal(maximum(length_0_utc_posixct), na_utc_posixct)
    expect_equal(maximum(length_0_utc_posixlt), na_utc_posixlt)
  })
  it("fails for non supported datatypes", {
    expect_error(maximum(1 + 1i), "supported")
    expect_error(maximum(c(1 + 1i, 1 + 2i)), "supported")
  })
  it("is type stable for base types", {
    for (i in 0:2) {
      expect_type(maximum(logical(i)), "integer")
      expect_type(maximum(numeric(i)), "double")
      expect_type(maximum(integer(i)), "integer")
      expect_s3_class(
        maximum(as.Date(integer(i), origin = "1970-01-01")),
        "Date"
      )
      expect_s3_class(
        maximum(as.POSIXct(integer(i), origin = "1970-01-01")),
        "POSIXct"
      )
    }
  })
})

describe("row_sums", {
  it("row sums multiple vectors", {
    expect_equal(
      row_sums(1:3, 1:3),
      1:3 + 1:3
    )
  })
  it("treats NA as 0", {
    expect_equal(
      row_sums(1:3, c(1, NA_real_, 3)),
      c(2, 2, 6)
    )
    expect_equal(
      row_sums(c(NA_real_, NA_real_, NA_real_)),
      c(0, 0, 0)
    )
  })
  it("does not work without any arguments", {
    expect_error(row_sums())
  })
  it("fails if non-numerics are passed in", {
    expect_error(row_sums(c("no", "numeric"), 1:2))
  })
})
