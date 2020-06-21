#'
#' Shift of an array with a single step.
#'
#' \code{scoutsimple} returns a new list with the information about the shifts performed to the observations in X.
#'
#' @param X A vector or matrix with observations that will be shifted as rows.
#' @param pcaref A list with the elemements of a PCA model: m (mean), s (standard deviation), prepro (preprocessing),
#' P (loading matrix), lambda (vector with variances of each PC). The preprocessing element is a character with possible values "none",
#' if any preprocessing should be performed on X, "cent", if a mean-centering should be performed on X, or "autosc", it a mean-centering
#' and unitary variance scaling (autoscaling) should be performed on X.
#' @param T2.y A number indicating the target value for the T^2_A after the shift. Set to NA by default.
#' @param SPE.y A number indicating the target value for the SPE after the shift. Set to NA by default.

#' @return scoutsimple list with elements with information about the shifted data. The matrix X, contains the new data, the SPE and T2 contain
#' the statistic values of each one of the new generated outliers or observations. The elements step.spe and step.t2 make reference to the step
#' at which each observation of the shifted dada is located. Finally, the element tag, has different values for each observation in X according
#' to each step (if tag.mode == "series"), otherwise is a vector of ones with as many elements as rows in X.

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
