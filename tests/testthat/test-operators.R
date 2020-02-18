context("operators")

describe("%==%", {
  it("converts NA to FALSE", {
    expect_equal(c(NA, 2, 5) %==% c(1, 2, 5), c(FALSE, TRUE, TRUE))
  })
  it("evaluates NA %==% NA to TRUE and warns", {
    x <- expect_warning(NA %==% NA, "NA %==% NA", all = TRUE)
    expect_true(x)
  })
  it("works with length(0) arguments", {
    expect_equal(integer(0L) %==% 2, logical(0L))
    expect_equal(1 %==% integer(0L), logical(0L))
  })
  it("works with examples from the docs", {
    expect_true(2 %==% 2)
    expect_true(expect_warning(NA %==% NA))
    expect_false(2 %==% 3)
    expect_false(2 %==% NA)
  })
})

describe("%!=%", {
  it("treats NA is a seperate type", {
    expect_equal(
      c(NA_real_, 2, 5) %!=% c(1, 2, 6),
      c(TRUE, FALSE, TRUE)
    )
  })
  it("works with length(0) arguments", {
    expect_equal(integer(0L) %!=% 2, logical(0L))
    expect_equal(1 %!=% integer(0L), logical(0L))
  })
  it("warns if NA != NA is called", {
    x <- expect_warning(NA %!=% NA, "NA %!=% NA", all = TRUE)
    expect_false(x)
  })
  it("works with examples from the docs", {
    expect_true(2 %!=% 1)
    expect_false(expect_warning(NA %!=% NA))
    expect_false(2 %!=% 2)
    expect_true(2 %!=% NA)
  })
})

describe("%>=%", {
  it("converts NA to FALSE", {
    expect_equal(c(NA, 6, 5) %>=% c(1, 2, 5), c(FALSE, TRUE, TRUE))
  })
  it("evaluates NA %>=% NA to TRUE", {
    x <- expect_warning(NA %>=% NA, "NA %>=% NA", all = TRUE)
    expect_true(x)
  })
  it("works with length(0) arguments", {
    expect_equal(integer(0L) %>=% 2, logical(0L))
    expect_equal(1 %>=% integer(0L), logical(0L))
  })
  it("works with examples from the docs", {
    expect_true(2 %>=% 1)
    expect_false(2 %>=% 3)
    expect_false(2 %>=% NA)
  })
})

describe("%<=%", {
  it("converts NA to FALSE", {
    expect_equal(c(NA, 1, 5) %<=% c(1, 2, 5), c(FALSE, TRUE, TRUE))
  })
  it("evaluates NA %<=% NA to TRUE", {
    x <- expect_warning(NA %<=% NA, "NA %<=% NA", all = TRUE)
    expect_true(x)
  })
  it("works with length(0) arguments", {
    expect_equal(integer(0L) %<=% 1, logical(0L))
    expect_equal(1 %<=% integer(0L), logical(0L))
  })
  it("works with examples from the docs", {
    expect_true(2 %<=% 3)
    expect_false(2 %<=% 1)
    expect_false(2 %<=% NA)
  })
})

describe("%<%", {
  it("converts NA to FALSE", {
    expect_equal(c(NA, 1, 5) %<% c(1, 2, 5), c(FALSE, TRUE, FALSE))
  })
  it("works with length(0) arguments", {
    expect_equal(integer(0L) %<% 2, logical(0L))
    expect_equal(1 %<% integer(0L), logical(0L))
  })
  it("works with examples from the docs", {
    expect_true(2 %<% 3)
    expect_false(2 %<% 1)
    expect_false(2 %<% NA)
  })
})

describe("%>%", {
  it("converts NA to FALSE", {
    expect_equal(c(NA, 3, 5) %>% c(1, 2, 5), c(FALSE, TRUE, FALSE))
  })
  it("works with length(0) arguments", {
    expect_equal(integer(0L) %>% 2, logical(0L))
    expect_equal(1 %>% integer(0L), logical(0L))
  })
  it("works with examples from the docs", {
    expect_true(2 %>% 1)
    expect_false(2 %>% 2)
    expect_false(2 %>% NA)
  })
})

describe("%between%", {
  it("works as expected", {
    expect_true(5 %between% c(1, 6))
    expect_false(5 %between% c(1, 3))
  })
  it("fails if rhs is no length 2 vector", {
    expect_error(5 %between% 6)
  })
  it("handles NAs and treats them as false", {
    expect_false(NA %between% c(1, 6))
  })
  it("does not allow NAs on the right hand side", {
    expect_error(1 %between% c(NA_real_, 5))
    expect_error(1 %between% c(1, NA_real_))
    expect_error(1 %between% c(NA_real_, NA_real_))
  })
})

describe("%all_in%", {
  it("checks all element per row", {
    lhs <- list(c("2", "1"), c("2", "3"))
    rhs <- c("1", "2")
    expect_equal(c(TRUE, FALSE), lhs %all_in% rhs)
  })
  it("fails if rhs is not a length >= 1 vector", {
    expect_error("1" %all_in% NA_character_)
    expect_error("1" %all_in% c())
  })
  it("returns true if no elements are in the left hand side", {
    expect_equal(c(FALSE, TRUE), list("2", character(0L)) %all_in% "1")
  })
  it("supports integer vectors", {
    lhs <- list(c(2L, 1L), c(2L, 3L))
    rhs <- c(1L, 2L)
    expect_equal(c(TRUE, FALSE), lhs %all_in% rhs)
  })
  it("is case insensitive", {
    expect_true("a" %all_in% "A")
    expect_true("B" %all_in% "b")
  })
  it("can handle numerics and integers", {
    expect_true(13 %all_in% 13L)
    expect_true(13L %all_in% 13)
  })
})

describe("%any_in%", {
  it("checks any element per row", {
    lhs <- list(c("2", "1"), c("2", "3"))
    rhs <- c("1", "2")
    expect_equal(c(TRUE, TRUE), lhs %any_in% rhs)
  })
  it("fails if rhs is not a length >= 1 vector", {
    expect_error("1" %any_in% NA_character_)
    expect_error("1" %any_in% c())
    expect_error(1 %any_in% NA_real_)
  })
  it("returns false if no elements are in the left hand side", {
    expect_equal(c(FALSE, FALSE), list("2", character(0L)) %any_in% "1")
  })
  it("supports a set of test cases", {
    test_that("any_in testcase #1", {
      lhs <- list(c(2, 1))
      rhs <- c(1, 2, 3)
      expect_equal(TRUE, lhs %any_in% rhs)
    })

    test_that("any_in testcase #2", {
      lhs <- list(c(2, 1))
      rhs <- c(4, 3)
      expect_equal(FALSE, lhs %any_in% rhs)
    })

    test_that("any_in testcase #3", {
      lhs <- c(2, 1)
      rhs <- c(1, 2, 3)
      expect_equal(c(TRUE, TRUE), lhs %any_in% rhs)
    })
  })

  it("is case insensitive", {
    expect_true("a" %any_in% "A")
    expect_true("B" %any_in% "b")
  })

  it("throws an error if rhs is NA", {
    expect_error("test" %any_in% NA)
  })

  it("can handle numerics and integers", {
    expect_true(13 %any_in% 13L)
    expect_true(13L %any_in% 13)
  })

  it("supports integer vectors", {
    lhs <- c(2L, 1L)
    rhs <- c(1L, 2L, 3L)
    expect_equal(c(TRUE, TRUE), lhs %any_in% rhs)
  })
})

describe("%any_like%", {
  it("works without wildcards", {
    expect_equal(
      c("ate", "te", "test") %any_like% "te",
      c(FALSE, TRUE, FALSE)
    )
  })
  it("returns FALSE if LHS is NA", {
    expect_equal(
      NA_character_ %any_like% "test",
      FALSE
    )
  })

  it("supports multiple wildcars", {
    expect_equal(
      c("atest", "te", "tes") %any_like% "%te%",
      c(TRUE, TRUE, TRUE)
    )
    expect_equal(
      c("atest", "te", "tes") %any_like% "te%",
      c(FALSE, TRUE, TRUE)
    )
    expect_equal(
      c("ate", "te", "tee") %any_like% "%te",
      c(TRUE, TRUE, FALSE)
    )
    expect_equal(
      c("aabcc", "abc", "abxc") %any_like% "%abc%",
      c(TRUE, TRUE, FALSE)
    )
  })

  it("escapes regex characters", {
    expect_equal(
      c("abc.*\\see", "abc ") %any_like% "%.*\\s%",
      c(TRUE, FALSE)
    )
  })

  it("covers a range of test cases", {

    test_that("any_like testcase #1", {
      lhs <- list(c("C123", "C59999.3"), c("C1234", "C51.3"))
      rhs <- c("C51%", "C52%", "C53%", "C54%", "C55%", "C56%", "C57%", "C58%")
      expect_equal(c(FALSE, TRUE), lhs %any_like% rhs)
    })

    test_that("any_like testcase #2", {
      lhs <- list(c("C123", "C51.3", "C55"))
      rhs <- c("C51%", "C52%", "C53%", "C54%", "C55%", "C56%", "C57%", "C58%")
      expect_equal(TRUE, lhs %any_like% rhs)
    })

    test_that("any_like testcase #3", {
      lhs <- "1-123.33:X"
      rhs <- c("1-444%", "1-123%", "1-555%", "1-666.4%")
      expect_equal(TRUE, lhs %any_like% rhs)
    })

    test_that("any_like testcase #4", {
      lhs <- list(c("1-456:X", "1-567:X", "1-8910", "1-1112.x:X", "1-1345.01"))
      rhs <- c("1-652.4%", "1-652.6%", "1-652.y%", "1-123%")
      expect_equal(FALSE, lhs %any_like% rhs)
    })

  })

  it("throws error if rhs is length 0", {
    expect_error("test" %any_like% "", "character")
  })

  it("throws error if rhs is only wildcard", {
    expect_error("test" %any_like% "%", "character")
  })

  it("works with large list of patterns", {
    patterns <- c(as.character(1:10042), "abc%")
    expect_true("abc1000" %any_like% patterns)
  })

  it("escapes special regexp characters", {
    expect_false("\\\\^a" %any_like% "\\^a")
    expect_true("\\^[]()*{}$.+?|a" %any_like% "\\^[]()*{}$.+?|a")
  })
})

describe("%all_like%", {
  it("works with characters", {
    lhs <- c("1-123.123:X", "1-123.E123:X")
    rhs <- c("1-123.1%", "1-123.e%")
    expect_equal(c(TRUE, TRUE), lhs %all_like% rhs)
  })

  it("works without wildcard", {
    expect_equal(c("wat", "wa", "foo") %all_like% "wa", c(FALSE, TRUE, FALSE))
  })

  it("throws error if rhs is only wildcard", {
    expect_error("test" %all_like% "%", "character")
  })

  it("throws error if rhs is length 0", {
    expect_error("test" %all_like% "", "character")
  })

  it("returns true if lhs is empty", {
    expect_true(list(character(0L)) %all_like% "wa")
  })

  it("covers a range of test cases", {

    test_that("all_like testcase #1", {
      lhs <- list(c("1-123.12:R", "1-678"))
      rhs <- c("1-6%", "1-123%")
      expect_equal(TRUE, lhs %all_like% rhs)
    })

    test_that("all_like testcase #2", {
      lhs <- list(c("1-123.12:R", "1-456.03"))
      rhs <- c("5%", "1-123%")
      expect_equal(FALSE, lhs %all_like% rhs)
    })

    test_that("all_like testcase #3", {
      lhs <- list(c("1-123.90:R", "1-123.E123:R"))
      rhs <- c("1-123.9%", "1-123.e%")
      expect_equal(TRUE, lhs %all_like% rhs)
    })

  })

  it("works with large list of patterns", {
    patterns <- c(as.character(1:10042), "abc%")
    expect_true("abc1000" %all_like% patterns)
  })

  it("escapes special regexp characters", {
    expect_false("\\\\^a" %all_like% "\\^a")
    expect_true("\\^[]()*{}$.+?|a" %all_like% "\\^[]()*{}$.+?|a")
  })
})
