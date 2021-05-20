list2env(nonequiv_groups_dat, envir = globalenv())

test_that("identity equating produces a mean SEE equal to 0", {
  id <- eqt(x, y)
  expect_equal(boot(id)$mean_see, 0)
})

test_that("RMSE is correct", {
  tm <- eqt(x, y, xv = 1:30, yv = 1:30, linear = "mean", method = "tucker")
  tm_boot <- boot(tm)
  expect_equal(
    tm_boot$scales[, "rmse"]^2,
    tm_boot$scales[, "see"]^2 + tm_boot$scales[, "bias"]^2
  )
})
