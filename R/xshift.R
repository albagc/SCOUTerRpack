#'
#' Shift of an observation.
#'
#' \code{xshift} returns a new array, shifted accordingly to the factors a and b which determine the direction of the shift. The performed
#' operation results as a combination of two main directions: the direction of maximum gradient for the SPE (weighted by the parameter b) and
#' the direction of the projection of the observation on the model (weighted by the parameter a).
#'
#' @param X Vector or matrix with observations that will be shifted
#' @param P Loading matrix of the PCA model according to which the shfit will be performed.
#' @param a A number or vector tuning the amount of the shift for each observation from X in the direction of the x's projection.
#' @param b A number tuning the amount of the shift for each observation from X in the direction of the x's residual.
#'
#' @return Xout matrix with shifted observation as rows in the same order as in the input matrix (X).
#'
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
