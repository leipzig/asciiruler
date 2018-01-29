library(asciiruler)
context("ruler display")

myruler<-asciiruler(low=-30,high=30,borders=TRUE)

test_that("default.asciiruler", {
  expect_equal(myruler$output,"+----------------------------------------------------------------+\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||\n|  |    |    |    |    |    |    |    |    |    |    |    |    | |\n|-30  -25  -20  -15  -10   -5    0    5    10   15   20   25   30|\n+----------------------------------------------------------------+")
})

test_that("as.character", {
  expect_equal(as.character(myruler),myruler$output)
})

myrulerstrict<-asciiruler(low=-30,high=30,borders=FALSE,strict_width=TRUE)
test_that("strict", {
  expect_equal(myrulerstrict$output,"|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||\n|    |    |    |    |    |    |    |    |    |    |    |    |\n   -25  -20  -15  -10   -5    0    5    10   15   20   25    ")
  expect_equal(width(myrulerstrict),61)
})

test_that("printing and catting", {
  expect_output(print(myruler),myruler$output)
  expect_output(cat(myruler),myruler$output)
  expect_output(cat("foo"),"foo")
})

