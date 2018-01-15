library(JMolFarsPkg)
context("Make filename")

test_that("Filename is of class character",{
  filename <- make_filename(2013)
  expect_is(filename, "character")
})

test_that("Filename matches FARS filename example",{
  filename <- make_filename(2013)
  expect_match(filename, "accident_2013.csv.bz2")
})
