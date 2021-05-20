list2env(nonequiv_groups_dat, envir = globalenv())

id <- eqt(x, y)
tm <- eqt(x, y, xv = 1:30, yv = 1:30, linear = "mean", method = "tucker")
tl <- eqt(x, y, xv = 1:30, yv = 1:30, linear = "linear", method = "tucker")
ca <- eqt(x, y, xv = 1:30, yv = 1:30, linear = "linear", method = "chained",
          circle = "simplified")

test_that("default produces an average of linear equatings", {
  id_tm_tl <- multipleqt(id, tm, tl)
  expect_equal(
    id_tm_tl$coef[, 4],
    rowMeans(id_tm_tl$coef[, 1:3])
  )
})

test_that("point-wise method uses the exact weights, angle bisector method does
          not", {
  
  pw <- multipleqt(id, tm, tl, weights = 1:3, method = "point-wise")
  expect_equal(
    pw$coef[1, 4],
    weighted.mean(pw$coef[1, 1:3], w = 1:3)
  )
  expect_equal(
    pw$coef[2, 4],
    weighted.mean(pw$coef[2, 1:3], w = 1:3)
  )
  
  ab <- multipleqt(id, tm, tl, weights = 1:3, method = "angle bisector")
  expect_false(all(ab$coef[1, 4] == weighted.mean(ab$coef[1, 1:3], w = 1:3)))
  expect_false(all(ab$coef[2, 4] == weighted.mean(ab$coef[2, 1:3], w = 1:3)))
})

test_that("point-wise and angle bisector methods produce equal results only when
          equatings are parallel", {
  expect_equal(
    coef(multipleqt(id, tm, method = "point-wise")),
    coef(multipleqt(id, tm, method = "angle bisector"))
  )
  expect_false(identical(
    coef(multipleqt(id, tl, method = "point-wise")),
    coef(multipleqt(id, tl, method = "angle bisector"))
  ))
})
