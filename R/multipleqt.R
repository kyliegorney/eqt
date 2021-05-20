#' Weight multiple equating functions
#'
#' @description Weight multiple equating functions using the point-wise weighted
#' average method or the weighted angle bisector method.
#'
#' @param ... One or more objects of class `eqt`.
#' @param weights A vector of weights to be applied to the equatings. Default
#'   weights all equatings equally.
#' @param method `"point-wise"` (default) uses the point-wise weighted average
#'   method, and `"angle bisector"` uses the weighted angle bisector method (see
#'   Details).
#' @param p Use when `method = "angle bisector"`. An integer greater than or
#'   equal to 1 indicating the \eqn{L_p} circle to be used. Default is `2`.
#' @inheritParams eqt
#'
#' @return An object of class `multipleqt` is returned with the supplied
#' arguments and the following output. Equatings are numbered such that `eqt1`
#' is the first equating and so on.
#' \item{coef}{A matrix of coefficients.}
#' \item{scales}{A matrix of all possible raw scores and equated scores.}
#' \item{scores}{A matrix of raw scores and equated scores for each examinee.}
#' 
#' @details
#' If all equatings are parallel (i.e., they all have the same slope), then the
#' two methods will produce identical results. Else, the results will differ.
#' * The point-wise weighted average method uses the exact `weights` that are
#'   provided. However, the resulting transformation is not symmetric unless all
#'   equatings are parallel. Circle-arc equatings are accepted as input.
#' * The weighted angle bisector method adjusts the `weights` to ensure that the
#'   resulting transformation is symmetric. However, circle-arc equatings are
#'   not accepted as input.
#' 
#' @inherit eqt author
#'
#' @references
#' Holland, P. W., & Strawderman, W. E. (2011). How to average equating
#' functions, if you must. In A. A. von Davier (Ed.), *Statistical Models for
#' Test Equating, Scaling, and Linking* (pp. 89--107). Springer.
#' \url{https://doi.org/10.1007/978-0-387-98138-3_6}
#'
#' Kolen, M. J., & Brennan, R. L. (2014). *Test Equating, Scaling, and Linking:
#' Methods and Practices* (3rd ed.). Springer.
#' \url{https://doi.org/10.1007/978-1-4939-0317-7}
#'
#' @seealso [boot()], [eqt()]
#'
#' @examples
#' # Setup ---------------------------------------------------------------------
#' 
#' # Extract data
#' list2env(random_groups_dat, envir = globalenv())
#' 
#' # Identity equating
#' id <- eqt(x, y)
#' 
#' # Mean equating
#' me <- eqt(x, y, linear = "mean")
#' 
#' # Linear equating
#' li <- eqt(x, y, linear = "linear")
#' 
#' # Synthetic linking functions -----------------------------------------------
#' 
#' # Equal weights
#' multipleqt(id, li)
#' 
#' # Custom weights
#' multipleqt(id, li, weights = c(0.8, 0.2))
#' 
#' # Symmetric weights
#' multipleqt(id, li, method = "angle bisector")
#' 
#' # Other linking functions ---------------------------------------------------
#' 
#' # Use three equatings
#' multipleqt(id, me, li)
#' 
#' # Without identity equating
#' multipleqt(me, li)
#' 
#' @export

multipleqt <- function(..., weights, method = c("point-wise", "angle bisector"),
                       p = 2, verbose = TRUE) {
  
  object <- list_dots(...)
  E <- length(object)
  
  if (E == 1L) {
    return(object[[1]])
  }

  method <- match.arg(method)
  
  if (any(!is.eqt(object))) {
    stop("All objects must have class `eqt`.", call. = FALSE)
  } else if (p < 1L) {
    stop("`p` must be greater than or equal to 1.", call. = FALSE)
  } else if (!all_identical(object, which = "x")) {
    stop("`x` must be identical in all objects.", call. = FALSE)
  }
  
  if (missing(weights)) {
    weights <- rep(1 / E, times = E)
  } else if (length(weights) != E) {
    stop("`weights` must be a numeric vector of length ", E, ".", call. = FALSE)
  } else if (sum(weights) != 1) {
    weights <- proportions(weights)
  }
  
  if (all(vapply(object, function(x) is.null(x$circle), logical(1L)))) {
    circle <- FALSE
    coef <- vapply(object, function(x) x$coef, double(2L))
  } else if (method == "point-wise") {
    circle <- TRUE
    coef <- vapply(object, function(x) c(x$coef, NA, NA, NA)[1:5], double(5L))
    row.names(coef)[3:5] <- c("x_center", "y_center", "radius")
  } else {
    stop("Circle-arc equatings are not accepted for the angle bisector method.",
         call. = FALSE)
  }
  colnames(coef) <- paste0("eqt", 1:E)
  
  if (method == "point-wise") {
    p <- NULL
  } else {
    weights <- weights * (1 + coef[2, ]^p)^(-1/p) /
      sum(weights * (1 + coef[2, ]^p)^(-1/p))
  }
  
  if (!circle) {
    coef <- cbind(coef, eqt = as.double(coef %*% weights))
  }
  
  scales <- cbind(
    raw = object[[1]]$scales[, "raw"],
    vapply(object, function(x) {
      x$scales[, "eqt"]
    }, double(nrow(object[[1]]$scales))),
    eqt = NA
  )
  colnames(scales)[2:(E+1)] <- paste0("eqt", 1:E)
  scales[, "eqt"] <- scales[, 2:(E+1)] %*% weights
  scores <- get_scores(object[[1]]$scores[, "raw"], scales)
  
  if (verbose) {
    out <- remove_null(list(object = object, weights = weights, method = method,
      p = p, coef = coef, scales = scales, scores = scores))
    class(out) <- "multipleqt"
    out
  } else {
    scores[, "eqt"]
  }
}

# S3 methods -------------------------------------------------------------------

#' @export
coef.multipleqt <- function(object, ...) {
  object$coef
}

#' @export
print.multipleqt <- function(x, ...) {
  print(round(x$coef, digits = 2))
}

#' @export
summary.multipleqt <- function(object, ...) {
  summary(object$scores)
}
