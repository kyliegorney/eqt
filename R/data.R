#' Item response data
#' 
#' @description Item response data simulated under different equating designs.
#' 
#' @format Each is a list of two matrices, `x` and `y`, containing item
#' responses from the new and old forms, respectively. Each matrix has 25 rows
#' (persons) and 100 columns (items).
#' 
#' @details
#' Two equating designs are simulated. Under the random groups design, the test
#' forms have no items in common. However, under the common-item nonequivalent
#' groups design, the test forms are simulated to have the first 30 items in
#' common.
#' 
#' ```
#' # Setup ---------------------------------------------------------------------
#' 
#' # install.packages("devtools")
#' devtools::install_github("kyliegorney/sim")
#' 
#' library(sim)
#' set.seed(1004)
#' 
#' # Random groups design ------------------------------------------------------
#' 
#' # Item responses from the old and new forms, respectively
#' y <- sim_irt(theta = rnorm(25), b = rnorm(100))
#' x <- sim_irt(theta = rnorm(25), b = rnorm(100))
#' 
#' random_groups_dat <- list(x = x, y = y)
#' 
#' # Common-item nonequivalent groups design -----------------------------------
#' 
#' # Item responses from the old form
#' b <- rnorm(100)
#' y <- sim_irt(theta = rnorm(25), b = b)
#' 
#' # Select 30 anchor items
#' ind <- anchor(y, size = 30, verbose = FALSE)
#' 
#' # Rearrange to place the anchor items first
#' y <- cbind(y[, ind], y[, -ind])
#' 
#' # Item responses from the new form
#' x <- sim_irt(theta = rnorm(25), b = c(b[ind], rnorm(70)))
#' 
#' nonequiv_groups_dat <- list(x = x, y = y)
#' ```
"random_groups_dat"

#' @format
#' @rdname random_groups_dat
"nonequiv_groups_dat"
