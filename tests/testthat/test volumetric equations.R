test_that("Test that if Cylinder works", {
  result <- Cylinder(10, 3)
  expect_equal(result, (10 * 3 / 2 * 3 / 2 * pi))
  expect_equal(length(result), 1)
  expect_equal(is.numeric(result), TRUE)
})

test_that("Test that if Cone works", {
  result <- Cone(10, 3)
  expect_equal(result, (10 * 3 / 2 * 3 / 2 * pi / 3))
  expect_equal(length(result), 1)
  expect_equal(is.numeric(result), TRUE)
})

test_that("Test that if Ellipsoid works", {
  result <- Ellipsoid(10, 3)
  expect_equal(result, (10 / 2 * 3 / 2 * 3 / 2 * pi * 4 / 3))
  expect_equal(length(result), 1)
  expect_equal(is.numeric(result), TRUE)
})

test_that("Test that if LWR works", {
  result <- LWR(10, 3, 0.132)
  expect_equal(result, (10 * 3 * 3 * 0.132))
  expect_equal(length(result), 1)
  expect_equal(is.numeric(result), TRUE)
})
