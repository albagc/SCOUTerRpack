#'
#' scoutsimple
#'
#' Shift of an array with a single step.
#'
#' @param X Matrix with observations that will be shifted as rows.
#' @param pcaref List with the elemements of a PCA model: \code{m} (mean), \code{s} (standard deviation), \code{prepro} (preprocessing:
#' \code{"none"}, \code{"cent"} or \code{"autosc"}), \code{P} (loading matrix), \code{lambda} (vector with variances of each PC).
#' @param T2.target A number indicating the target value for the T^2_A after the shift. Set to \code{NA} by default.
#' @param SPE.target A number indicating the target value for the SPE after the shift. Set to \code{NA} by default.
#' @return list with elements: \code{X}, matrix with the new and shifted data, \code{SPE} and \code{T2} vectors with the statistic values
#' of each one of the new generated outliers or observations, elements \code{step.spe} and \code{step.t2} make reference to the step
#' of each observation. Finally, the element \code{tag}, is a vector of ones as long as the number of generated observations.
#' @export
scoutsimple <- function(X, pcaref, T2.target = NA, SPE.target = NA){
  if (is.null(dim(X)) == TRUE){
    X <- t(as.matrix(X))
  }
  n <- nrow(X)
  pcaout <- pcame(X, pcaref)
  Xaux <- pcaout$Xpreprocessed
  T2.0 <- pcaout$T2
  SPE.0 <- pcaout$SPE

  if (is.na(T2.target[1]) == TRUE){
    T2.target = T2.0}
  if (is.na(SPE.target[1]) == TRUE){
    SPE.target = SPE.0}

  a <- sqrt(T2.target / T2.0) - 1
  b <- sqrt(SPE.target / SPE.0) - 1

  Xout <- xshift(X, pcaref$P, a = a, b = b)

  outscout <- list()
  outscout$X <- Xout
  outscout$SPE <- SPE.target
  outscout$T2 <- T2.target
  outscout$step.spe <- matrix(1, n, 1)
  outscout$step.t2 <- matrix(1, n, 1)
  outscout$tag <- as.vector(kronecker(matrix(1, n, 1), t(1:(nrow(Xout)/nrow(X)))))
  return(outscout)
}
