#' Compute scaled scores
#' 
#' @description Compute scaled scores by defining properties of the new scale.
#' 
#' @param x A numeric vector to be scaled.
#' @param point1,point2 `point1` is a single score to be scaled to `point2`. For
#'   example, if `point1 = mean(x)` (default), then `point2` represents the mean
#'   of the new scale.
#' @param sd2 The standard deviation of the new scale.
#' @param digits The number of decimal places to retain when rounding the scaled
#'   scores. Default is `0`.
#' 
#' @return A vector of scaled scores is returned.
#' 
#' @inherit eqt author
#' 
#' @examples
#' # Setup ---------------------------------------------------------------------
#' 
#' # Extract data
#' list2env(random_groups_dat, envir = globalenv())
#' 
#' # Raw test scores
#' raw <- rowSums(x)
#' 
#' # Compute scaled scores -----------------------------------------------------
#' 
#' # Define the new mean
#' scaled(raw, point2 = 500)
#' 
#' # Define the new mean and SD
#' scaled(raw, point2 = 500, sd2 = 100)
#' 
#' # Define the new maximum and SD
#' scaled(raw, point1 = max(raw), point2 = 1000, sd2 = 100)
#' 
#' @importFrom stats sd
#' @export

scaled <- function(x, point1 = mean(x), point2, sd2, digits = 0) {
  
  if (all(x[-1] == x[1])) {
    stop("All elements in `x` cannot be identical.", call. = FALSE)
  }
  
  if (missing(point2)) {
    point2 <- point1
  }
  
  if (missing(sd2)) {
    sd2 <- sd(x)
  } else if (sd2 <= 0) {
    stop("`sd2` must be greater than 0.", call. = FALSE)
  }

  out <- sd2 / sd(x) * x + point2 - sd2 / sd(x) * point1
  round(out, digits = digits)
}
