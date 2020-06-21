#'
#' Shift of an array following a grid pattern.
#'
#' \code{scoutgrid} returns a new list with the information about the shifts performed to the observations in X.
#'
#' @param X A vector or matrix with observations that will be shifted as rows.
#' @param pcaref A list with the elemements of a PCA model: m (mean), s (standard deviation), prepro (preprocessing),
#' P (loading matrix), lambda (vector with variances of each PC). The preprocessing element is a character with possible values "none",
#' if any preprocessing should be performed on X, "cent", if a mean-centering should be performed on X, or "autosc", it a mean-centering
#' and unitary variance scaling (autoscaling) should be performed on X.
#' @param T2.y A number indicating the target value for the T^2_A after the shift. Set to NA by default.
#' @param SPE.y A number indicating the target value for the SPE after the shift. Set to NA by default.
#' @param nsteps.spe An integer indicating the number of steps in which the shift from the reference to
#' the target value of the SPE will be performed. Set to 1 by default
#' @param nsteps.t2 An integer indicating the number of steps in which the shift from the reference to the
#' target value of the T^2_A will be performed. Set to 1 by default
#' @param gspe A mumber indicating the term that will tune the spacing between steps for the SPE. Set to 1 by default (linear spacing).
#' @param st2 A mumber indicating the term that will tune the spacing between steps for the SPE. Set to 1 by default (linear spacing).
#' @param tag.mode A character with value "series" if the tag output vector should discriminate between steps or not. Set to "" by default.
#'
#' @return scoutgrid list with elements with information about the shifted data. The matrix X, contains the new data, the SPE and T2 contain
#' the statistic values of each one of the new generated outliers or observations. The elements step.spe and step.t2 make reference to the step
#' at which each observation of the shifted dada is located. Finally, the element tag, has different values for each observation in X according
#' to each step (if tag.mode == "series"), otherwise is a vector of ones with as many elements as rows in X.
#'
#' @export

scoutgrid <- function(X, pcaref, T2.M  = NA, SPE.M = NA, nsteps.spe = 1,
                      nsteps.t2 = 1, gspe = 1, gt2 = 1, tag.mode = "") {
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

  factor.spe <- diag(((1:nsteps.spe) / nsteps.spe) ^ gspe)
  factor.t2 <- diag(((1:nsteps.t2) / nsteps.t2) ^ gt2)
  steps.spe <- SPE.0 + kronecker(matrix(1, 1, nsteps.spe), SPE.M - SPE.0) %*% factor.spe
  steps.t2 <- T2.0 + kronecker(matrix(1, 1, nsteps.t2), T2.M - T2.0) %*% factor.t2

  spe.targets <- as.vector(kronecker(matrix(1, nsteps.t2, 1), steps.spe))
  t2.targets <- as.vector(kronecker(matrix(1, nsteps.spe, 1), as.vector(steps.t2)))
  spe.refs <- kronecker(matrix(1, nsteps.spe * nsteps.t2, 1), SPE.0)
  t2.refs <- kronecker(matrix(1, nsteps.spe * nsteps.t2, 1), T2.0)
  Xgrid <- kronecker(matrix(1, nsteps.spe * nsteps.t2, 1), Xaux)

  a <- sqrt(t2.targets / t2.refs) - 1
  b <- sqrt(spe.targets / spe.refs) - 1

  Xout <- xshift(Xgrid, pcaref$P, a = a, b = b)

  # Xout <- scoutsimple(Xgrid, pcaref, SPE.target = spe.targets, T2.target = t2.targets)

  if (pcaref$prepro == 'cent'){
    Xrec <- Xout + pcaref$m
  } else if (pcaref$prepro == 'autosc') {
    Xrec <- Xout * kronecker(matrix(1, nrow(Xout), 1), t(pcaref$s)) + pcaref$m
  }
  outscout <- list()
  outscout$X <- Xout
  outscout$SPE <- spe.targets
  outscout$T2 <- t2.targets
  outscout$step.spe <- as.vector(kronecker(matrix(1, nsteps.t2*n, 1), t(1:nsteps.spe)))
  outscout$step.t2 <- as.vector(kronecker(matrix(1, nsteps.spe, 1), as.vector(kronecker(matrix(1, n, 1), t(1:nsteps.t2)))))
  if (tag.mode == "series"){
    outscout$tag <- as.vector(kronecker(matrix(1, n, 1), t(1:(nrow(Xout)/nrow(X)))))
  } else {
    outscout$tag <- as.vector(matrix(1, nrow(Xout), 1))
  }
  return(outscout)
}

