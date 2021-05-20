list2env(nonequiv_groups_dat, envir = globalenv())

test_that("identity equating works", {
  id <- eqt(x, y)
  expect_equal(
    id$coef,
    c(intercept = 0, slope = 1)
  )
  expect_equal(
    id$scales[, "raw"],
    id$scales[, "eqt"]
  )
})

test_that("mean equating works", {
  me <- eqt(x, y, linear = "mean")
  tm <- eqt(x, y, xv = 1:30, yv = 1:30, linear = "mean", method = "tucker")
  expect_setequal(me$coef["slope"], 1)
  expect_setequal(tm$coef["slope"], 1)
})

test_that("linear equating works", {
  li <- eqt(x, y, linear = "linear")
  tl <- eqt(x, y, xv = 1:30, yv = 1:30, linear = "linear", method = "chained")
  expect_false(identical(li$coef["slope"], 1))
  expect_false(identical(tl$coef["slope"], 1))
})

test_that("circle-arc equating works", {
  ca_si <- eqt(x, y, linear = "mean", circle = "simplified")
  ca_sy <- eqt(x, y, linear = "mean", circle = "symmetric")
  expect_length(ca_si$coef, 5L)
  expect_equal(
    eqt(x, y, linear = "linear", circle = "simplified"),
    ca_si
  )
  expect_false(identical(
    ca_si$coef,
    ca_sy$coef
  ))
  expect_false(identical(
    coef(eqt(x, y, xv = 1:30, yv = 1:30, linear = "linear", method = "chained",
             circle = "simplified")),
    coef(eqt(x, y, xv = 1:30, yv = 1:30, linear = "linear", method = "chained",
             circle = "symmetric"))
  ))
})

test_that("weights argument works", {
  tm_u <- eqt(x, y, xv = 1:30, yv = 1:30, linear = "mean", method = "tucker")
  tm_w <- eqt(x, y, xv = 1:30, yv = 1:30, linear = "mean", method = "tucker",
              weights = c(0.8, 0.2))
  expect_false(identical(
    tm_u$weights,
    tm_w$weights
  ))
  expect_false(identical(
    tm_u$coef,
    tm_w$coef
  ))
  expect_equal(
    eqt(x, y, xv = 1:30, yv = 1:30, linear = "linear", method = "chained"),
    eqt(x, y, xv = 1:30, yv = 1:30, linear = "linear", method = "chained",
        weights = c(0.8, 0.2))
  )
})

test_that("anchor argument works", {
  ll_i <- eqt(x, y, xv = 1:30, yv = 1:30, linear = "linear",
              method = "levine observed")
  ll_e <- eqt(x, y, xv = 1:30, yv = 1:30, linear = "linear",
              method = "levine observed", anchor = "external")
  expect_false(identical(
    ll_i$anchor,
    ll_e$anchor
  ))
  expect_false(identical(
    ll_i$coef,
    ll_e$coef
  ))
  expect_equal(
    eqt(x, y, xv = 1:30, yv = 1:30, linear = "mean", method = "tucker"),
    eqt(x, y, xv = 1:30, yv = 1:30, linear = "mean", method = "tucker",
        anchor = "external")
  )
})

test_that("lower argument works", {
  ca_0 <- eqt(x, y, xv = 1:30, yv = 1:30, linear = "linear",
              method = "chained", circle = "simplified")
  ca_20 <- eqt(x, y, xv = 1:30, yv = 1:30, linear = "linear",
               method = "chained", circle = "simplified", lower = c(20, 20))
  expect_false(identical(
    ca_0$lower,
    ca_20$lower
  ))
  expect_false(identical(
    ca_0$coef,
    ca_20$coef
  ))
})
