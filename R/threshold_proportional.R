#' Brain Network Construction via threshold
#'
#' Construct brain connectivity network
#' w/ specified links percentage via threshold
#'
#' @param W covariance matrix.
#' @param p links percentage.
#' @return thresholded connectivity matrix.
#' @examples
#'  library(Matrix)
#'  x <- runif(20, 1, 5)
#'  mat <- matrix(x, nrow = 4)
#'  cov <-  cov(mat)
#'  threshold_proportional(cov, 0.3)
#'
#' @importFrom Matrix tril
#' @export
threshold_proportional <- function(W, p) {
  n <- nrow(W) # number of nodes
  diag(W) <- 0 # clear diagonal

  if (isSymmetric(W)) {
    # make sure symmetry is preserved
    W <- as.matrix(tril(W))
    ud <- 2
  } else {
    ud <- 1
  }

  # sort by magnitude
  E <- sort(abs(W), decreasing = T, index.return = T)
  en <- round((n^2-n)*p/ud)  # number of links to be preserved

  W[E$ix[-c(1:en)]] <- 0 # apply threshold

  if (ud == 2) {
    W <- W+t(W) # recover symmetry
  }

  return(W)
}
