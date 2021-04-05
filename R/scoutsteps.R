#'
#' scoutsteps
#'
#' Shift of an array following a step-wise pattern.
#'
#' @param X Matrix with observations that will be shifted as rows.
#' @param pcaref List with the elements of a PCA model: 
#' * \code{m}: mean.
#' * \code{s}: standard deviation.
#' * \code{prepro}: preprocessing: \code{"none"}, \code{"cent"} or \code{"autosc"}.
#' * \code{P}: loading matrix.
#' * \code{lambda}: vector with variances of each PC.
#' @param T2.target A number indicating the target value for the Hotelling's T^2_A after the shift. 
#' Set to \code{NA} by default.
#' @param SPE.target A number indicating the target value for the Squared Prediction Error after  
#' the shift. Set to \code{NA} by default.
#' @param nsteps A number indicating the number of steps between the reference and target 
#' values of the SPE and the T^2. Set to \code{1} by default.
#' @param gspe A number indicating the term that will tune the spacing between steps for the SPE. 
#' Set to \code{1} by default (linear spacing).
#' @param gt2 A number indicating the term that will tune the spacing between steps for the SPE. 
#' Set to \code{1} by default (linear spacing).
#' @param A PC selected to perform the shift.
#' @return list with elements: 
#' * \code{X}: matrix with the new and shifted data.
#' * \code{SPE}: SPE of each one of the generated outliers in the list element \code{X}. 
#' * \code{T2}: T^2 of each one of the generated outliers in the list element \code{X}.
#' * \code{step.spe}: step of each observation according to the shift of the SPE.
#' * \code{step.t2}: step of each observation according to the shift of the T^2.
#' * \code{tag}: is a vector of ones as long as the number of generated observations.
#' @examples 
#' X <- as.matrix(X)
#' pcamodel.ref <- pcamb_classic(X, 3, 0.1, "autosc") # PCA-MB with all observations
#' # Shift a set of observations increasing the T^2  and the SPE in 4 linear steps:
#' outsteps <- scoutsteps(X, pcamodel.ref, T2.target = matrix(40, nrow(X), 1), 
#' SPE.target = matrix(50, nrow(X), 1), nsteps = 4)
#' # Shift a set of observations increasing the SPE in 4 non-linear steps:
#' outsteps <- scoutsteps(X, pcamodel.ref, SPE.target = matrix(50, nrow(X), 1), nsteps = 4, 
#' gspe = 0.3)
#' @export
scoutsteps <- function(X, pcaref, T2.target = NA, SPE.target = NA, nsteps = 1, 
                       gspe = 1, gt2 = 1, A = 0) {
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
  if (is.na(T2.target[1]) == TRUE){
    T2.target = T2.0}
  if (is.na(SPE.target[1]) == TRUE){
    SPE.target = SPE.0}

  factor.spe <- diag(((1:nsteps) / nsteps) ^ gspe)
  factor.t2 <- diag(((1:nsteps) / nsteps) ^ gt2)
  steps.spe <- SPE.0 + kronecker(matrix(1, 1, nsteps), SPE.target - SPE.0) %*% factor.spe
  steps.t2 <- T2.0 + kronecker(matrix(1, 1, nsteps), T2.target - T2.0) %*% factor.t2

  spe.targets <- as.vector(steps.spe)
  t2.targets <- as.vector(steps.t2)
  spe.refs <- as.vector(kronecker(matrix(1, nsteps, 1), SPE.0))
  t2.refs <- as.vector(kronecker(matrix(1, nsteps, 1), T2.0))
  Xsteps <- kronecker(matrix(1, nsteps, 1), Xaux)
  
  if (A == 0){
    a <- sqrt(t2.targets / t2.refs) - 1
  } else {
    tA <- rep(pcaout$Tscores[,A], 1,nsteps)^2
    a <- sqrt((1 + pcaref$lambda[A]*(t2.targets - t2.refs)/tA)) - 1
  }

  b <- sqrt(spe.targets / spe.refs) - 1

  Xout <- xshift(Xsteps, pcaref$P, a = a, b = b, A = A)

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
  outscout$tag <- as.vector(matrix(1, nrow(Xout), 1))
  return(outscout)
}
