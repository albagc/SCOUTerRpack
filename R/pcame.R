#'
#' pcame
#'
#' Projection of X onto a PCA model.
#'
#' \code{pcame} performs the projection of the data in X onto the PCA model stored as a list of parameters. It returns the projection
#' of the observations in X, along with the SPE, Hotelling's T^2_A, contribution elements and the reconstruction of X obtained by the
#' PCA model.
#' @param X Matrix with observations that will be projected onto the PCA model.
#' @param pcaref A list with the elemements of a PCA model: \code{m} (mean), \code{s} (standard deviation), \code{prepro} (preprocessing:
#' \code{"none"}, \code{"cent"} or \code{"autosc"}), \code{P} (loading matrix), \code{lambda} (vector with variances of each PC).
#' @return list with elements containing information about X in the PCA model: \code{Xpreprocessed} (matrix \code{X} preprocessed),
#' \code{Tscores} (score matrix with the projection of \code{X} on each one of the A PCs), \code{E} (error matrix with the par of \code{X}
#' not explained by the PCA model), \code{SPE} (vector with the SPE for each observation of \code{X}), \code{T2} (vector with the T^_A for
#' each observation of \code{X}), \code{T2matrix} (matrix with the contributions of each PC to the T^2_A for each observation of \code{X})
#' and \code{Xrec} (matrix with the reconstructed part of \code{X}, i.e. the part of \code{X} explained by the PCA model).
#' @examples 
#' X <- as.matrix(X)
#' pcamodel.ref <- pcamb_classic(X, 3, 0.1, "autosc") # PCA-MB with all observations
#' pcame(X, pcamodel.ref) # Project all observations onto PCA model of pcamodel.ref
#' 
#' pcamodel.ref <- pcamb_classic(X[1:40,], 2, 0.05, "cent") # PCA-MB with first 40 observations
#' pcame(X[-c(1:40),], pcamodel.ref) # Project observations not used in PCA-MB onto PCA model of pcamodel.ref
#' @export
pcame <- function(X, pcaref){
  if (is.null(dim(X)) == TRUE){
    X <- t(as.matrix(X))
  }
  n <- nrow(X)
  if (pcaref$prepro == 'cent'){
    Xaux <- X - pcaref$m
  } else if (pcaref$prepro == 'autosc') {
    Xaux <- (X - pcaref$m) / kronecker(matrix(1, n, 1), t(pcaref$s))
  } else if (pcaref$prepro == 'none') {
    Xaux <- X
  }
  p <- ncol(Xaux)
  P <- pcaref$P
  I <- diag(p)
  Tscores <- Xaux %*% P
  E <- Xaux - Tscores %*% t(P)
  SPE <- rowSums(E ^ 2)
  T2 <- rowSums(Tscores ^ 2 / kronecker(matrix(1, n, 1), pcaref$lambda))
  T2matrix <- Tscores ^ 2 / kronecker(matrix(1, n, 1), pcaref$lambda)
  if (pcaref$prepro == 'cent'){
    Xrec <- Tscores %*% t(P) + pcaref$m
  } else if (pcaref$prepro == 'autosc') {
    Xrec <- Tscores %*% t(P) * kronecker(matrix(1, n, 1), t(pcaref$s)) + pcaref$m
  } else if (pcaref$prepro == 'none') {
    Xrec <- Tscores %*% t(P)
  }
  pcaout <- list()
  pcaout$Xpreprocessed <- Xaux
  pcaout$Tscores <- Tscores
  pcaout$E <- E
  pcaout$SPE <- SPE
  pcaout$T2 <- T2
  pcaout$T2matrix <- T2matrix
  pcaout$Xrec <- Xrec
  return(pcaout)
}

