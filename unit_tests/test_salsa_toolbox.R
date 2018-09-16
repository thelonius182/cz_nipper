# Load the libraries
library(testthat)

# Source the scripts
source("quadratic_function.R")

# Define the context
context("Vierkantsvergelijking uitvoeren")

# Define the tests
test_that("wortels van elkaar verschillen", {
  calculated_root <- quadratic_equation(1, 7, 10)
  
  expect_is(calculated_root, "numeric")
  expect_length(calculated_root, 2)
  expect_lt(calculated_root[1], calculated_root[2])
})

test_that("script stopt als het moet", {
  expect_error(quadratic_equation(0, 7, 10))
})

test_that("function returns only one value for repeated roots", {
  calculated_root <- quadratic_equation(1, 6000, 9000000)
  expect_length(calculated_root, 1)
  expect_equal(calculated_root,-3000)
})
