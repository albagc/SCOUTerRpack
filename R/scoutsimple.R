#'
#' scoutsimple
#'
#' Shift of an array with a single step.
#'
#' @param X Matrix with observations that will be shifted as rows.
#' @param pcaref List with the elements of a PCA model: 
#' * \code{m}: mean.
#' * \code{s}: standard deviation.
#' * \code{prepro}: preprocessing: \code{"none"}, \code{"cent"} or \code{"autosc"}.
#' * \code{P}: loading matrix.
#' * \code{lambda}: vector with variances of each PC.
#' @param T2.target A number indicating the target value for the T^2_A after the shift. 
#' Set to \code{NA} by default.
#' @param SPE.target A number indicating the target value for the SPE after the shift. 
#' Set to \code{NA} by default.
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
#' # Shift a set of observations increasing only the T^2 in one step:
#' outsimple <- scoutsimple(X, pcamodel.ref, T2.target = matrix(40, nrow(X), 1))
#' @export
scoutsimple <- function(X, pcaref, T2.target = NA, SPE.target = NA, A = 0){
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
  if (A == 0){
    a <- sqrt(T2.target / T2.0) - 1
  } else {
    tA <- pcaout$Tscores[,A]
    a <- sqrt((1 + pcaref$lambda[A]*(T2.target - T2.0)/tA^2)) - 1
  }
  b <- sqrt(SPE.target / SPE.0) - 1

  Xout <- xshift(X, pcaref$P, a = a, b = b, A = A)

  outscout <- list()
  outscout$X <- Xout
  outscout$SPE <- SPE.target
  outscout$T2 <- T2.target
  outscout$step.spe <- matrix(1, n, 1)
  outscout$step.t2 <- matrix(1, n, 1)
  outscout$tag <- as.vector(kronecker(matrix(1, n, 1), t(1:(nrow(Xout)/nrow(X)))))
  return(outscout)
}
