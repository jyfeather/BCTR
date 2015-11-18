#' Brain Network Construction via SICE
#'
#' @import glasso
#' @import Matrix
SICE_proportional <- function(W, p, rho_init = 1, rho_min = 0, rho_max = 5, errRange = 10) {
  time_now <- Sys.time()
  total_edge = nrow(W)^2
  edge_expected = p * total_edge + nrow(W) # because of diagonal
  rho_current <- rho_init

  while (TRUE) {
    # fit glasso
    fit <- glasso(W, rho_current)
    edges <- round(fit$wi, digits = 3)
    edge_current <- nnzero(edges)

    # check jump out condition
    edge_diff <- edge_current-edge_expected
    if (abs(edge_diff) < errRange) {
      break
    }

    # set consumed time bound to 30 seconds
    time_collapsed <- difftime(Sys.time(), time_now, units = 'secs')
    if (time_collapsed > 30) {
      break
    }

    # update rho via binary search
    if (edge_diff < 0) {
      rho_max <- rho_current
    } else {
      rho_min <- rho_current
    }
    rho_current <- (rho_max+rho_min)/2
  }

  # prepare return information
  diag(edges) <- 0
  returnInfo <- list(edges = edges, rho = rho_current)
  return(returnInfo)
}
