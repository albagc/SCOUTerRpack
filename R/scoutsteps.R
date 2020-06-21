#'
#' Shift of an array following a step-wise pattern.
#'
#' \code{scoutsteps} returns a new list with the information about the shifts performed to the observations in X.
#'
#' @param X A vector or matrix with observations that will be shifted as rows.
#' @param pcaref A list with the elemements of a PCA model: m (mean), s (standard deviation), prepro (preprocessing),
#' P (loading matrix), lambda (vector with variances of each PC). The preprocessing element is a character with possible values "none",
#' if any preprocessing should be performed on X, "cent", if a mean-centering should be performed on X, or "autosc", it a mean-centering
#' and unitary variance scaling (autoscaling) should be performed on X.
#' @param T2.y A number indicating the target value for the T^2_A after the shift. Set to NA by default.
#' @param SPE.y A number indicating the target value for the SPE after the shift. Set to NA by default.
#' @param nsteps An integer indicating the number of steps in which the shift from the reference to the target values of the SPE and the T^2_A
#' will be performed.
#' @param gspe A mumber indicating the term that will tune the spacing between steps for the SPE. Set to 1 by default (linear spacing).
#' @param st2 A mumber indicating the term that will tune the spacing between steps for the SPE. Set to 1 by default (linear spacing).
#' @param tag.mode A character with value "series" if the tag output vector should discriminate between steps or not. Set to "" by default.
#'
#' @return scoutsteps list with elements with information about the shifted data. The matrix X, contains the new data, the SPE and T2 contain
#' the statistic values of each one of the new generated outliers or observations. The elements step.spe and step.t2 make reference to the step
#' at which each observation of the shifted dada is located. Finally, the element tag, has different values for each observation in X according
#' to each step (if tag.mode == "series"), otherwise is a vector of ones with as many elements as rows in X.

scoutsteps <- function(X, pcaref, T2.M = NA, SPE.M = NA, nsteps = 1, gspe = 1, gt2 = 1, tag.mode = "") {
  # Calculate initial values for SPE and T^2 of observations in X.
  if (is.null(dim(X)) == TRUE){
    X <- t(as.matrix(X))
  }
  n <- nrow(X)
  pcaout <- pcame(X, pcaref)
  Xaux <- pcaout$Xpreprocessed
  T2.0 <- pcaout$T2
  SPE.0 <- pcaout$SPE
  # When the target value is not specified, the target value will be the initial value of the
  # statistic (there will not be any shift).
  if (is.na(T2.M[1]) == TRUE){
    T2.M = T2.0}
  if (is.na(SPE.M[1]) == TRUE){
    SPE.M = SPE.0}

  factor.spe <- diag(((1:nsteps) / nsteps) ^ gspe)
  factor.t2 <- diag(((1:nsteps) / nsteps) ^ gt2)
  steps.spe <- SPE.0 + kronecker(matrix(1, 1, nsteps), SPE.M - SPE.0) %*% factor.spe
  steps.t2 <- T2.0 + kronecker(matrix(1, 1, nsteps), T2.M - T2.0) %*% factor.t2

  spe.targets <- as.vector(steps.spe)
  t2.targets <- as.vector(steps.t2)
  spe.refs <- as.vector(kronecker(matrix(1, nsteps, 1), SPE.0))
  t2.refs <- as.vector(kronecker(matrix(1, nsteps, 1), T2.0))
  Xsteps <- kronecker(matrix(1, nsteps, 1), Xaux)

  a <- sqrt(t2.targets / t2.refs) - 1
  b <- sqrt(spe.targets / spe.refs) - 1

  Xout <- xshift(Xsteps, pcaref$P, a = a, b = b)

  if (pcaref$prepro == 'cent'){
    Xrec <- Xout + pcaref$m
  } else if (pcaref$prepro == 'autosc') {
    Xrec <- Xout * kronecker(matrix(1, nrow(Xout), 1), t(pcaref$s)) + pcaref$m
  }

  outscout <- list()
  outscout$X <- Xout
  outscout$SPE <- spe.targets
  outscout$T2 <- t2.targets
  outscout$step.spe <- as.vector(kronecker(matrix(1, n, 1), t(1:nsteps)))
  outscout$step.t2 <- as.vector(kronecker(matrix(1, n, 1), t(1:nsteps)))
  if (tag.mode == "series"){
    outscout$tag <- as.vector(kronecker(matrix(1, n, 1), t(1:nsteps)))
  } else {
    outscout$tag <- as.vector(matrix(1, nrow(Xout), 1))
  }

  return(outscout)
}
