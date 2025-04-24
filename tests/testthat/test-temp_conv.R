# Basic cases ====================

test_that("Fareinheit to Celcius", {
  testthat::expect_equal(temp_conv(41, "F", "C"), 5)
  testthat::expect_type(temp_conv(41, "F", "C"), "double")
})

test_that("Celsius to Fareinheit", {
  testthat::expect_equal(temp_conv(5, "C", "F"), 41)
  testthat::expect_type(temp_conv(5, "C", "F"), "double")
})

test_that("Fareinheit to Kelvin", {
  testthat::expect_equal(temp_conv(41, "F", "K"), 278.15)
  testthat::expect_type(temp_conv(41, "F", "K"), "double")
})

test_that("Kelvin to Fareinheit", {
  testthat::expect_equal(temp_conv(278.15, "K", "F"), 41)
  testthat::expect_type(temp_conv(278.15, "K", "F"), "double")
})

test_that("Celsius to Kelvin", {
  testthat::expect_equal(temp_conv(5, "C", "K"), 278.15)
  testthat::expect_type(temp_conv(5, "C", "K"), "double")
})

test_that("Kelvin to Celsius", {
  testthat::expect_equal(temp_conv(278.15, "K", "C"), 5)
  testthat::expect_type(temp_conv(278.15, "K", "C"), "double")
})

test_that("Same scale", {
  testthat::expect_equal(temp_conv(5, "C", "C"), 5)
  testthat::expect_type(temp_conv(5, "C", "C"), "double")
})

# Edge cases ====================
test_that("Edge of Farenheit", {
  testthat::expect_equal(temp_conv(-459.67, "F", "K"), 0)
  testthat::expect_type(temp_conv(-459.67, "F", "K"), "double")
})

test_that("Edge of Celsius", {
  testthat::expect_equal(temp_conv(-273.15, "C", "F"), -459.67)
  testthat::expect_type(temp_conv(-273.15, "C", "F"), "double")
})

test_that("Edge of Kelvin", {
  testthat::expect_equal(temp_conv(0, "K", "C"), -273.15)
  testthat::expect_type(temp_conv(0, "K", "C"), "double")
})

# Error cases ====================
test_that("Incorrect scale for start_scale and end_scale", {
  testthat::expect_error(temp_conv(0, "OK", "K"))
  testthat::expect_error(temp_conv(0, "K", "OK"))
  testthat::expect_error(temp_conv(0, "OK", "OK"))
})

test_that("Invalid value for start_scale", {
  testthat::expect_error(temp_conv(-460, "F", "F"))
  testthat::expect_error(temp_conv(-280, "C", "C"))
  testthat::expect_error(temp_conv(-1, "K", "K"))
})

test_that("temp cannot be string", {
  testthat::expect_error(temp_conv("0", "F", "F"))
  testthat::expect_error(temp_conv("0", "C", "C"))
  testthat::expect_error(temp_conv("0", "K", "K"))
})

test_that("temp cannot be list", {
  testthat::expect_error(temp_conv(c(0, 1), "F", "F"))
  testthat::expect_error(temp_conv(c(0, 1), "C", "C"))
  testthat::expect_error(temp_conv(c(0, 1), "K", "K"))
})