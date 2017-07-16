context("see_question")

test_that("Errors where appropriate", {
  expect_error(see_question(-1), regexp = "question number.*whole number")
  expect_error(see_question(60), regexp = "question is not available")
  
  expect_error(see_question(data.frame(x = 1)), regexp = "must be one of the data sets in this package")
})

test_that("Returns data", {
  expect_identical(see_question(Census2016_ancestories), Census2016_ancestories)
})

test_that("Images similar to those expected", {
  skip_if_not_installed("visualTest")
  skip_if_not_installed("png")
  tmp <- tempfile(fileext = ".png")
  png(tmp, width = 1344, height = 805)
  see_question(Census2016_ancestories)
  dev.off()
  visualTest::isSimilar(tmp, "BF2EC1C1C4D1C4CE", threshold = 8)
})
