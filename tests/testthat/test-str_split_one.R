# Basic case ==================

test_that("str_split_one() splits a string", {
  expect_equal(str_split_one("a,b,c", ","), c("a", "b", "c"))
})

# Edge case ==================

test_that("str_split_one() exposes features of stringr::str_split()", {
  expect_equal(str_split_one("a,b,c,d", ",", n = 2), c("a", "b,c,d")) # Only first 2 characters are split
  expect_equal(str_split_one("a.b", stringr::fixed(".")), c("a", "b")) # Match fixed characters, not expression
  expect_equal(str_split_one("abc", "b"), c("a", "c")) # Match regex
})

test_that("str_split_one() splits an empty string", {
  expect_equal(str_split_one("", ","), c(""))
})

test_that("str_split_one() returns a singular string with no split", {
  expect_equal(str_split_one("abc", ","), c("abc"))
})

test_that("str_split_one() returns string of length 0", {
  expect_equal(length(character(0)), 0)
  expect_equal(str_split_one(character(0), ","), character(0))
})

# Error case ==================

test_that("str_split_one() errors if input length > 1", {
  expect_error(str_split_one(c("a,b","c,d"), ","))
})

test_that("str_split_one() errors if not string", {
  expect_error(str_split_one(1000, ","))
})
