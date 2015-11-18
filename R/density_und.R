density_und <- function(CIJ) {
  N <- nrow(CIJ)
  tri <- as.matrix(tril(CIJ))
  K <- sum(tri!=0)
  kden <- K/((N^2-N)/2)
  return(kden)
}