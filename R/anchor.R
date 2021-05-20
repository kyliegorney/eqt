#' Select anchor items
#' 
#' @description Select anchor items to represent the total test.
#' 
#' @param x A matrix of item responses.
#' @param size The number of anchor items.
#' @param mean_diff The maximum absolute difference between the mean of the
#'   anchor test \eqn{p}-values and the mean of the total test \eqn{p}-values.
#'   Default is `0.01`.
#' @param sd_diff The maximum absolute difference between the SD of the anchor
#'   test \eqn{p}-values and the SD of the total test \eqn{p}-values multiplied
#'   by `sd_spread`. Default is `0.01`.
#' @param sd_spread A value greater than 0 indicating the spread of the anchor
#'   test \eqn{p}-values relative to the spread of the total test
#'   \eqn{p}-values. `1` (default) indicates a minitest, and smaller values
#'   indicate a semi-miditest or a miditest (Sinharay & Holland, 2007).
#' @param max_secs,sd_diff_adj The maximum number of seconds to search for a
#'   solution. Default is `5`. If a solution is not found, then `sd_diff` is
#'   increased by `sd_diff_adj` and a message is given.
#' @param verbose Logical indicating whether full output should be returned.
#'   Default is `TRUE`. If `FALSE`, then only the anchor item indices are
#'   returned.
#' 
#' @return A list is returned with the following output.
#' \item{ind}{A vector of anchor item indices.}
#' \item{moments}{A matrix of the means and SDs of the total test \eqn{p}-values
#'   and the anchor test \eqn{p}-values.}
#' \item{cor}{The correlation between the total test scores and the anchor test
#'   scores.}
#' 
#' @inherit eqt author
#' 
#' @references
#' Sinharay, S., & Holland, P. W. (2007). Is it necessary to make anchor tests
#' mini-versions of the tests being equated or can some restrictions be relaxed?
#' *Journal of Educational Measurement*, *44*(3), 249--275.
#' 
#' @examples
#' # Extract data
#' list2env(nonequiv_groups_dat, envir = globalenv())
#' 
#' # Minitest
#' anchor(y, size = 30)
#' 
#' # Semi-miditest with a 75% spread
#' anchor(y, size = 30, sd_spread = 0.75)
#' 
#' @importFrom stats cor sd
#' @export

anchor <- function(x, size, mean_diff = 0.01, sd_diff = 0.01, sd_spread = 1,
                   max_secs = 5, sd_diff_adj = 0.01, verbose = TRUE) {
  
  if (!is.matrix(x)) {
    stop("`x` must be a matrix.", call. = FALSE)
  } else if (any(x != 0 & x != 1)) {
    stop("All elements in `x` must be 0 or 1.", call. = FALSE)
  } else if (sd_spread <= 0L) {
    stop("`sd_spread` must be greater than 0.", call. = FALSE)
  }
  
  pval <- colMeans(x)
  mean0 <- mean(pval)
  sd0 <- sd(pval)
  ind0 <- which(pval != 0 & pval != 1)
  ind <- sample(ind0, size = size)
  time0 <- Sys.time()
  
  while (abs(mean(pval[ind]) - mean0) > mean_diff ||
         abs(sd(pval[ind]) - sd0 * sd_spread) > sd_diff) {
    ind <- sample(ind0, size = size)
    if (difftime(Sys.time(), time0) > max_secs) {
      sd_diff <- sd_diff + sd_diff_adj
      message("Solution not found in ", max_secs,
              ". `sd_diff` increased to ", sd_diff, ".")
      time0 <- Sys.time()
    }
  }
  
  if (verbose) {
    list(
      ind = ind,
      moments = matrix(c(mean0, mean(pval[ind]), sd0, sd(pval[ind])), ncol = 2,
                       dimnames = list(c("total", "anchor"), c("mean", "sd"))),
      cor = cor(rowSums(x), rowSums(x[, ind]))
    )
  } else {
    ind
  }
}
