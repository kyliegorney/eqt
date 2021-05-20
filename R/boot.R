#' Compute bootstrap standard errors
#'
#' @description Compute bootstrap standard errors for one or more equatings.
#'
#' @param object An object of class `eqt` or `multipleqt`.
#' @param rep The number of replications to be conducted. Default is `1000`.
#' @param crit A vector of scores to be used as the criterion when computing the
#'   bias and RMSE. Defaults to the results of the original equating.
#' 
#' @details
#' Bootstrapping offers an alternative to computing analytic standard errors. In
#' this process, samples are repeatedly drawn from the original data with
#' replacement. Equating is then conducted on these samples, and results are
#' compared across replications.
#' 
#' @return An object of class `boot` is returned with the following output.
#' \item{scales}{A matrix of raw scores, criterion scores, mean equated scores,
#'   standard errors of equating, bias, and RMSEs.}
#' \item{scores}{A matrix of raw scores, criterion scores, and mean equated
#'   scores for each examinee.}
#' \item{mean_see}{A weighted average of the standard errors. Weights are chosen
#'   using the frequencies of the observed scores (Kolen & Brennan, 2014).}
#'
#' @inherit eqt author
#'
#' @references
#' Kolen, M. J., & Brennan, R. L. (2014). *Test Equating, Scaling, and Linking:
#' Methods and Practices* (3rd ed.). Springer.
#' \url{https://doi.org/10.1007/978-1-4939-0317-7}
#'
#' @seealso [eqt()], [multipleqt()]
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
#' # Linear equating
#' li <- eqt(x, y, linear = "linear")
#' 
#' # Multiple equatings
#' id_li <- multipleqt(id, li)
#' 
#' # Compute bootstrap standard errors -----------------------------------------
#' 
#' # Use an eqt object
#' boot(li)
#' 
#' # Use a multipleqt object
#' boot(id_li)
#'
#' @export

boot <- function(object, rep = 1000, crit) {

  if (!is.eqt(object) && !is.multipleqt(object)) {
    stop("`object` must have class `eqt` or `multipleqt`.", call. = FALSE)
  }
  
  crit0 <- object$scales[, "eqt"]
  C <- length(crit0)
  
  if (missing(crit)) {
    crit <- crit0
  } else if (length(crit) != C) {
    stop("`crit` must be a vector of length ", C, ".", call. = FALSE)
  }

  args <- extract_args(object)
  dat <- matrix(NA, nrow = C, ncol = rep)
  
  if (is.eqt(object)) {
    
    for (r in 1:rep) {
      y <- sample_boot(object$y)
      x <- sample_boot(object$x)
      dat[, r] <- do.call(eqt, args = c(list(x, y), args))$scales[, "eqt"]
    }
    
  } else {
    
    M <- length(object$object)
    indiv <- vector("list", M)
    args_inner <- lapply(object$object, extract_args)
    y_identical <- all_identical(object$object, which = "y")
    
    for (r in 1:rep) {
      y <- sample_boot(object$object[[1]]$y)
      x <- sample_boot(object$object[[1]]$x)
      
      for (m in 1:M) {
        if (!y_identical) {
          y <- sample_boot(object$object[[m]]$y)
        }
        indiv[[m]] <- do.call(eqt, args = c(list(x, y), args_inner[[m]]))
      }
      
      dat[, r] <- do.call(multipleqt, args = c(indiv, args))$scales[, "eqt"]
    }
  }

  scales <- cbind(
    raw = 0:(C-1),
    crit = crit,
    mean = rowMeans(dat),
    see = sqrt(rowMeans((dat - rowMeans(dat))^2)),
    bias = rowMeans(dat) - crit,
    rmse = sqrt(rowMeans((dat - crit)^2))
  )

  scores <- get_scores(object$scores[, "raw"], scales)
  mean_see <- sqrt(mean(scores[, "see"]^2))

  out <- list(scales = scales, scores = scores[, 1:3], mean_see = mean_see)
  class(out) <- "boot"
  out
}

# S3 methods -------------------------------------------------------------------

#' @export
print.boot <- function(x, ...) {
  cat("Mean SEE:", x$mean_see, "\n")
}

#' @export
summary.boot <- function(object, ...) {
  summary(object$scores)
}

# Helper functions -------------------------------------------------------------

#' Extract arguments
#' 
#' @param object An object of class `eqt` or `multipleqt`.
#' 
#' @noRd

extract_args <- function(object) {
  object[names(object) %in% c("xv", "yv", "linear", "method", "circle",
                              "weights", "anchor", "lower", "p")]
}

#' Draw bootstrap samples
#' 
#' @param x A matrix of item responses.
#' 
#' @noRd

sample_boot <- function(x) {
  x[sample.int(nrow(x), replace = TRUE), ]
}
