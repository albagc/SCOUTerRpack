#'
#' Projection of X onto a PCA model.
#'
#' \code{pcame} performs the projection of the data in X onto the PCA model stored as a list of parameters. It returns the projection
#' of the observations in X, along with the SPE, Hotelling's T^2_A, contribution elements and the reconstruction of X obtained by the
#' PCA model.
#'
#' @param X A vector or matrix with observations that will be projected onto the PCA model.
#' @param pcamodel A list with the elemements of a PCA model: m (mean), s (standard deviation), prepro (preprocessing),
#' P (loading matrix), lambda (vector with variances of each PC). The preprocessing element is a character with possible values "none",
#' if any preprocessing should be performed on X, "cent", if a mean-centering should be performed on X, or "autosc", it a mean-centering
#' and unitary variance scaling (autoscaling) should be performed on X.
#'
#' @return pcaout list with fields containing information about X in the PCA model: Xpreprocessed (matrix X preprocessed),
#' Tscores (score matrix with the projection of X on each one of the A PCs), E (error matrix with the par of X not explained by the PCA model),
#' SPE (vector with the SPE for each observation of X), T2 (vector with the T^_A for each observation of X), T2matrix (matrix with the contributions of each PC
#' to the T^2_A for each observation of X) and Xrec (matrix with the reconstructed part of X, i.e. the part of X explained by the PCA model).
#'
#' @export

pcame <- function(X, pcamodel){
  if (is.null(dim(X)) == TRUE){
    X <- t(as.matrix(X))
  }
  n <- nrow(X)
  if (pcamodel$prepro == 'cent'){
    Xaux <- X - pcamodel$m
  } else if (pcamodel$prepro == 'autosc') {
    Xaux <- (X - pcamodel$m) / kronecker(matrix(1, n, 1), t(pcamodel$s))
  } else if (pcamodel$prepro == 'none') {
    Xaux <- X
  }
  p <- ncol(Xaux)
  P <- pcamodel$P
  I <- diag(p)
  Tscores <- Xaux %*% P
  E <- Xaux - Tscores %*% t(P)
  SPE <- rowSums(E ^ 2)
  T2 <- rowSums(Tscores ^ 2 / kronecker(matrix(1, n, 1), pcamodel$lambda))
  T2matrix <- Tscores ^ 2 / kronecker(matrix(1, n, 1), pcamodel$lambda)

  if (pcamodel$prepro == 'cent'){
    Xrec <- Tscores %*% t(P) + pcamodel$m
  } else if (pcamodel$prepro == 'autosc') {
    Xrec <- Tscores %*% t(P) * kronecker(matrix(1, n, 1), t(pcamodel$s)) + pcamodel$m
  } else if (pcamodel$prepro == 'none') {
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

