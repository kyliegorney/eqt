list2env(nonequiv_groups_dat, envir = globalenv())

test_that("mean_diff and sd_diff arguments work", {
  out <- anchor(y, size = 30)
  expect_lte(abs(diff(out$moments[, "mean"])), 0.01)
  expect_lte(abs(diff(out$moments[, "sd"])), 0.01)
})

test_that("sd_spread argument works", {
  out <- anchor(y, size = 30, sd_spread = 0.75)
  expect_lte(abs(diff(out$moments[, "mean"])), 0.01)
  expect_lte(abs(diff(c(0.75, 1) * out$moments[, "sd"])), 0.01)
})
