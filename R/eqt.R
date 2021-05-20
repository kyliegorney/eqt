#' Small-sample equating
#'
#' @description A collection of methods commonly used to perform small-sample
#' equating.
#'
#' @param x,y Matrices of item responses from the new and old forms,
#'   respectively.
#' @param xv,yv Numeric vectors containing indices of the anchor items, where
#'   `xv` corresponds to `x`, and `yv` corresponds to `y`. If either is `NULL`,
#'   then the random groups design is assumed (see Details).
#' @param linear Character indicating the transformation to be applied.
#'   `"identity"` (default) does not use any estimation, `"mean"` estimates the
#'   intercept, and `"linear"` estimates the intercept and slope.
#' @param method Character indicating the method to be applied under the
#'   common-item nonequivalent groups design (see Details). Options are
#'   `"nominal weights"`, `"tucker"`, `"levine observed"`, `"levine true"`, and
#'   `"chained"`.
#' @param circle If `NULL` (default), then circle-arc equating is not performed.
#'   Else, options are `"simplified"` and `"symmetric"`.
#' @param ... Additional arguments to be used with specific methods.
#'   \describe{
#'     \item{`weights`}{Use with nominal weights, Tucker, and Levine
#'       observed-score equating. A numeric vector of length 2 indicating the
#'       weights to be applied to the new and old populations, respectively. If
#'       `NULL` (default), then weights are proportional to sample size.}
#'     \item{`anchor`}{Use with Levine observed-score and Levine true-score
#'       equating. An `"internal"` anchor (default) contributes to the total
#'       test score, while an `"external"` anchor does not.}
#'     \item{`lower`}{Use with circle-arc equating. A numeric vector of length 2
#'       indicating the lower end points to be used for `x` and `y`,
#'       respectively. Default is `c(0, 0)`.}
#'   }
#' @param verbose Logical indicating whether the full output should be returned.
#'   Default is `TRUE`. If `FALSE`, then only the equated scores are returned.
#' 
#' @details
#' If the test forms have no items in common (`xv` or `yv = NULL`), then the
#' random groups design is assumed, and a method is not required.
#' 
#' If `xv` and `yv` are provided, then the common-item nonequivalent groups
#' design is assumed, and a method is applied to estimate the synthetic
#' population parameters. Depending on the method, additional arguments may be
#' required.
#' 
#' @return An object of class `eqt` is returned with the supplied arguments and
#' the following output.
#' \item{synth}{A matrix of the mean and SD of the synthetic populations, if
#'   estimated.}
#' \item{coef}{A vector of coefficients.}
#' \item{scales}{A matrix of all possible raw scores and equated scores.}
#' \item{scores}{A matrix of raw scores and equated scores for each examinee.}
#' 
#' @author Kylie N. Gorney \email{kyliengorney@@gmail.com}
#'
#' @references
#' Babcock, B., Albano, A., & Raymond, M. (2012). Nominal weights mean equating:
#' A method for very small samples. *Educational and Psychological Measurement*,
#' *72*(4), 608--628. \url{https://doi.org/10.1177/0013164411428609}
#' 
#' Kolen, M. J., & Brennan, R. L. (2014). *Test Equating, Scaling, and Linking:
#' Methods and Practices* (3rd ed.). Springer.
#' \url{https://doi.org/10.1007/978-1-4939-0317-7}
#'
#' Livingston, S. A., & Kim, S. (2008). *Small-sample equating by the circle-arc
#' method* (Research Report 08-39). Educational Testing Service.
#'
#' @seealso [boot()], [multipleqt()]
#'
#' @examples
#' # Random groups design ------------------------------------------------------
#' 
#' # Extract data
#' list2env(random_groups_dat, envir = globalenv())
#' 
#' # Identity equating
#' eqt(x, y)
#' 
#' # Mean equating
#' eqt(x, y, linear = "mean")
#' 
#' # Linear equating
#' eqt(x, y, linear = "linear")
#' 
#' # Common-item nonequivalent groups design -----------------------------------
#' 
#' # Extract data
#' list2env(nonequiv_groups_dat, envir = globalenv())
#' 
#' # Tucker mean equating
#' eqt(x, y, xv = 1:30, yv = 1:30, linear = "mean", method = "tucker")
#' 
#' # Levine observed-score equating with an external anchor
#' eqt(x, y, xv = 1:30, yv = 1:30, linear = "linear",
#'     method = "levine observed", anchor = "external")
#' 
#' # Simplified circle-arc with chained linear equating
#' eqt(x, y, xv = 1:30, yv = 1:30, linear = "linear", method = "chained",
#'     circle = "simplified")
#' 
#' @importFrom stats cov sd var
#' @export

eqt <- function(x, y,
                xv = NULL, yv = NULL,
                linear = c("identity", "mean", "linear"),
                method = c("nominal weights", "tucker", "levine observed",
                           "levine true", "chained"),
                circle = NULL,
                ...,
                verbose = TRUE) {

  if (!is.matrix(x) || !is.matrix(y)) {
    stop("`x` and `y` must be matrices.", call. = FALSE)
  }
  
  if (any(x != 0 & x != 1) || any(y != 0 & y != 1)) {
    stop("All elements in `x` and `y` must be 0 or 1.", call. = FALSE)
  }
  
  linear <- match.arg(linear)

  if (is.null(xv) || is.null(yv)) {
    method <- xv <- yv <- NULL
  } else if (length(xv) != length(yv)) {
    stop("`xv` and `yv` must be the same length.", call. = FALSE)
  } else if (linear == "identity") {
    xv <- yv <- method <- circle <- NULL
  } else {
    method <- match.arg(method)
    XV <- rowSums(x[, xv])
    YV <- rowSums(y[, yv])
  }
  
  X <- rowSums(x)
  Y <- rowSums(y)
  
  if (method %in% c("levine observed", "levine true") &&
      !is.null(list_dots(...)$anchor) &&
      list_dots(...)$anchor == "external") {
    X <- X - XV
    Y <- Y - YV
  }
  
  ix <- ncol(x)
  iy <- ncol(y)
  iv <- length(xv)
  raw_scale <- 0:ix
  
  if (is.null(circle)) {
    out <- linear(X, Y, XV, YV, ix, iy, iv, linear, method, ...)
    eqt_scale <- out$coef[1] + out$coef[2] * raw_scale
    
  } else {
    
    circle <- match.arg(circle, choices = c("simplified", "symmetric"))
    out <- circle(X, Y, XV, YV, ix, iy, iv, linear, method, circle, ...)
    
    if (is.null(method)) {
      linear <- "mean"
    }
    
    if (is.na(out$coef[5])) {
      eqt_scale <- out$coef[1] + out$coef[2] * raw_scale
      
    } else {
    
      if (out$above) {
        arc <- out$coef[4] + sqrt(out$coef[5]^2 - (raw_scale - out$coef[3])^2)
      } else {
        arc <- out$coef[4] - sqrt(out$coef[5]^2 - (raw_scale - out$coef[3])^2)
      }
      out$above <- NULL
      
      if (circle == "simplified") {
        eqt_scale <- arc + out$coef[1] + out$coef[2] * raw_scale
      } else {
        eqt_scale <- arc
      }
    }
  }
  
  out$scales <- cbind(raw = raw_scale, eqt = eqt_scale)
  out$scores <- get_scores(X, out$scales)
  
  if (verbose) {
    out <- remove_null(c(list(x = x, y = y, xv = xv, yv = yv, linear = linear,
      method = method, circle = circle), out))
    class(out) <- "eqt"
    out
  } else {
    out$scores[, "eqt"]
  }
  
}

# S3 methods -------------------------------------------------------------------

#' @export
coef.eqt <- function(object, ...) {
  object$coef
}

#' @export
print.eqt <- function(x, ...) {
  print(round(x$coef, digits = 2))
}

#' @export
summary.eqt <- function(object, ...) {
  summary(object$scores)
}

# Circle functions -------------------------------------------------------------

#' Compute center coordinates of a circle
#' 
#' @param Xp,Yp Numeric vectors of length 3.
#' 
#' @noRd

center <- function(Xp, Yp) {
  ((Xp[1]^2 + Yp[1]^2) * (Yp[3] - Yp[2]) +
     (Xp[2]^2 + Yp[2]^2) * (Yp[1] - Yp[3]) +
     (Xp[3]^2 + Yp[3]^2) * (Yp[2] - Yp[1])) /
   (2 * (Xp[1]*(Yp[3] - Yp[2]) + Xp[2]*(Yp[1] - Yp[3]) + Xp[3]*(Yp[2] - Yp[1])))
}

#' Compute circle-arc coefficients
#' 
#' @noRd

circle <- function(X, Y, XV, YV, ix, iy, iv, linear, method, circle, ...,
                   lower = c(0, 0)) {
  
  if (!is.numeric(lower) || length(lower) != 2L) {
    stop("`lower` must be a numeric vector of length 2.", call. = FALSE)
  }
  
  Xp <- c(lower[1], mean(X), ix)
  Yp <- c(lower[2], mean(Y), iy)
  
  slope <- (Yp[3] - Yp[1]) / (Xp[3] - Xp[1])
  intercept <- Yp[1] - Xp[1] * slope
  
  if (is.null(method)) {
    out <- NULL
  } else {
    out <- linear(X, Y, XV, YV, ix, iy, iv, linear, method, ...)
    Yp[2] <- out$coef[1] + Xp[2] * out$coef[2]
  }
  
  if (circle == "simplified") {
    Yp <- Yp - (intercept + slope * Xp)
  }
  
  x_center <- center(Xp, Yp)
  y_center <- center(Yp, Xp)

  if (is.finite(x_center) && is.finite(y_center)) {
    
    radius <- sqrt((x_center - Xp[1])^2 + (y_center - Yp[1])^2)
    crit <- if (circle == "simplified") 0 else y_center
    
    out$coef <- c(intercept = intercept, slope = slope, x_center = x_center,
                  y_center = y_center, radius = radius)
    out$lower <- lower
    out$above <- Yp[2] > crit
  }
  
  out
}

# Linear functions -------------------------------------------------------------

#' Compute gamma coefficients
#' 
#' @noRd

gamma <- function(X, V, ix, iv, linear, method, anchor) {
  switch(method,
    "nominal weights" = ix / iv,
    "tucker" = cov(X, V) / var(V),
    "levine observed" = ,
    "levine true" = ifelse(anchor == "internal",
      var(X) / cov(X, V),
      (var(X) + cov(X, V)) / (var(V) + cov(X, V))
    ),
    "chained" = switch(linear,
      mean = 1,
      linear = sd(X) / sd(V)
    )
  )
}

#' Compute linear coefficients
#' 
#' @noRd

linear <- function(X, Y, XV, YV, ix, iy, iv, linear, method, weights,
                   anchor = "internal") {
  
  if (is.null(method)) {
    mX <- mean(X)
    mY <- mean(Y)
    sX <- sd(X)
    sY <- sd(Y)
    weights <- anchor <- synth <- NULL
    
  } else {
    gX <- gamma(X, XV, ix, iv, linear, method, anchor)
    gY <- gamma(Y, YV, iy, iv, linear, method, anchor)
    
    if (method %in% c("levine true", "chained")) {
      mX <- mean(X)
      mY <- mean(Y) + gY * (mean(XV) - mean(YV))
      sX <- gX
      sY <- gY
      weights <- synth <- NULL
      
    } else {
      
      if (missing(weights)) {
        weights <- proportions(c(length(X), length(Y)))
      } else if (!is.numeric(weights) || length(weights) != 2L) {
        stop("`weights` must be a numeric vector of length 2.", call. = FALSE)
      } else if (sum(weights) != 1) {
        weights <- proportions(weights)
      }
      
      mX <- mean(X) - weights[2] * gX * (mean(XV) - mean(YV))
      mY <- mean(Y) + weights[1] * gY * (mean(XV) - mean(YV))
      sX <- sqrt(var(X) - weights[2] * gX^2 * (var(XV) - var(YV)) +
        weights[1] * weights[2] * gX^2 * (mean(XV) - mean(YV))^2)
      sY <- sqrt(var(Y) + weights[1] * gY^2 * (var(XV) - var(YV)) +
        weights[1] * weights[2] * gY^2 * (mean(XV) - mean(YV))^2)

      synth <- matrix(c(mX, mY, sX, sY), ncol = 2,
                      dimnames = list(c("x_synth", "y_synth"), c("mean", "sd")))
    }
    
    if (!method %in% c("levine observed", "levine true")) {
      anchor <- NULL
    }
  }
  
  slope <- switch(linear,
    identity = ,
    mean = 1,
    linear = sY / sX
  )
  
  intercept <- switch(linear,
    identity = 0,
    mean = ,
    linear = mY - mX * slope
  )
  
  list(weights = weights, synth = synth, anchor = anchor,
       coef = c(intercept = intercept, slope = slope))
}
