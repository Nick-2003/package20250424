# 1992: True
test_that("year divisible by 4", {
  # "expected use cases" tests to be added here
  expect_equal(is_leap(1992), TRUE)

  expect_type(is_leap(1992), "logical")
})

# 2000: True
test_that("year divisible by 4, 100 and 400", {
  # "expected use cases" tests to be added here
  expect_equal(is_leap(2000), TRUE)

  expect_type(is_leap(2000), "logical")
})

# 1900: False
test_that("year divisible by 4 and 100, but not 400", {
  # "expected use cases" tests to be added here
  expect_equal(is_leap(1900), FALSE)

  expect_type(is_leap(1900), "logical")
})

# 2021: False
test_that("year not divisible by 4, 100 or 400", {
  # "expected use cases" tests to be added here
  expect_equal(is_leap(2021), FALSE)

  expect_type(is_leap(2021), "logical")
})

# 1802: False
test_that("year is even but not divisible by 4", {
  # "expected use cases" tests to be added here
  expect_equal(is_leap(1802), FALSE)

  expect_type(is_leap(1802), "logical")
})

# Return an error if the user puts in the value of 0
test_that("error if 0", {
  expect_error(is_leap(0))
})

# Return an error if the user puts in a negative value
test_that("error if negative", {
  expect_error(is_leap(-2000))
})

# Returns an error if the user puts in a string
test_that("error if incorrect types", {
  expect_error(is_leap("2000"))
})

# Return an error if the user puts in a non-interger value
test_that("error if non-integer", {
  expect_error(is_leap(2000.4))
})

# Return an error if the user puts in a list of numbers
test_that("error if list of numbers", {
  expect_error(is_leap(c(1992, 2000)))
})