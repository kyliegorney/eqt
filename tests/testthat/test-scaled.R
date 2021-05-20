list2env(random_groups_dat, envir = globalenv())

test_that("default settings leave x unchanged", {
  y <- scaled(x, digits = 16)
  expect_equal(y, x)
})

test_that("point2 and sd2 arguments work together", {
  y <- scaled(x, point2 = 500, sd2 = 100, digits = 16)
  expect_equal(mean(y), 500)
  expect_equal(sd(y), 100)
})
