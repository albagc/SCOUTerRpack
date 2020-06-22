#'
#' Shift of an observation. The performed operation results as a combination of two main directions: the direction of
#' maximum gradient for the SPE (weighted by the parameter b) and the direction of the projection of the observation
#' on the model (weighted by the parameter a).
#'
#' @param X Matrix with observations that will be shifted
#' @param P Loading matrix of the PCA model according to which the shfit will be performed.
#' @param a A number or vector tuning the shift in the direction of its projection.
#' @param b A number or vector tuning the shift in the direction of its residual.
#' @return Matrix with shifted observation as rows, keeping the order of the input matrix \code{X}.
#' @export
xshift <- function(X, P, a, b){
  if (is.null(dim(X)) == TRUE){
    X <- t(as.matrix(X))
  }
  n <- nrow(X)
  I <- diag(nrow(P))
  Xout <- matrix(NA, nrow = 0, ncol = ncol(X))
  for (ni in 1:nrow(X)){
    x2 <- (X[ni,] %*% (I + b[ni] * (I - (P %*% t(P)))) %*% (I + a[ni] * (P %*% t(P))))
    Xout <- rbind(Xout, x2)
  }
  return(Xout)
}
