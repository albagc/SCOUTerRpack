#'
#' pcamb_classic
#'
#' Principal Component Analysis (PCA) model fitting according to a matrix X using singular
#' value decomposition (svd)
#'
#' @param X Matrix with observations that will used to fit the PCA model.
#' @param ncomp An integer indicating the number of PCs that the model will have.
#' @param alpha A number between 0 and 1 indicating the type I risk assumed to calculate 
#' the Upper Control Limits (UCLs) for the Squared Prediction Error (SPE), the Hotelling's 
#' T^2_A and the scores. The confidence level of these limits will be \code{(1-alpha)*100}.
#' @param prepro A string indicating the preprocessing to be performed on X. Its possible 
#' values are: \code{"none"}, for any preprocessing, \code{"cent"}, for a mean-centering,
#' or \code{"autosc"}, for a mean-centering and unitary variance scaling (autoscaling).
#' @return list with elements containing information about PCA model: 
#' * \code{m}: mean vector.
#' * \code{s}: standard deviation vector.
#' * \code{P}: loading matrix with the loadings of each PC stored as columns. 
#' * \code{Pfull}: full loading matrix obtained by the svd,
#' * \code{lambda}: vector with the variance of each PC.
#' * \code{limspe}: Upper Control Limit for the SPE with a confidence level 
#' (1-alpha)*100 %.
#' * \code{limt2}: Upper Control Limit for the T^2_A with a confidence level 
#' (1-alpha)*100 %. 
#' * \code{limits_t}: Upper control Limits for the scores with a confidence level 
#' (1-alpha)*100 %.
#' * \code{prepro}: string indicating the type of preprocessing performed on X.
#' * \code{ncomp}: number of PCs of the PCA model, A.
#' * \code{alpha}: value of the type I risk assumed to calculate the Upper Control 
#' Limits of the SPE, T^2_A and scores.
#' * \code{n}: dimension of the number of rows in X.
#' * \code{S}: covariance matrix of X.
#' @import stats
#' @examples 
#' X <- as.matrix(X)
#' pcamodel.ref <- pcamb_classic(X, 3, 0.1, "autosc") # PCA-MB with all observations
#' pcamodel.ref <- pcamb_classic(X[1:40,], 2, 0.05, "cent") # PCA-MB with first 40 
#' # observations
#' @export
pcamb_classic <- function(X, ncomp, alpha, prepro) {
  m <- colMeans(X)
  s <- apply(X, 2, stats::sd)
  n <- nrow(X)
  if (prepro == 'cent'){
    Xaux <- X - m
  } else if (prepro == 'autosc') {
    Xaux <- (X - m) / kronecker(matrix(1, n, 1), t(s))
  } else if (prepro == 'none') {
    Xaux <- X
  }
  dvout <- svd(Xaux)
  V <- dvout$v
  D <- dvout$d
  P <- V[, 1:ncomp]
  Pfull <- V
  Tsc <- Xaux %*% P
  Tfull <- Xaux %*% Pfull
  E <- Xaux - Tsc %*% t(P)
  SPE <- rowSums(E ^ 2)
  Lambda <- diag(stats::var(Tsc))
  ev <- eigen(stats::cov(E))
  LambdaE <- rev(ev$values)
  lambda <- t(Lambda)
  T2 <- rowSums(Tsc ^2 / kronecker(matrix(1, n, 1), lambda))
  if (prepro == 'cent'){
    Xrec <- Tsc %*% t(P) + m
  } else if (prepro == 'autosc') {
    Xrec <- Tsc %*% t(P) * kronecker(matrix(1, n, 1), t(s)) + m
  } else if (prepro == 'none') {
    Xrec <- Tsc %*% t(P)
  }
  # SPE upper control limit
  theta1 <- sum(LambdaE[-(1:ncomp)])
  theta2 <- sum(LambdaE[-(1:ncomp)] ^ 2)
  theta3 <- sum(LambdaE[-(1:ncomp)] ^ 3)
  h0 <- 1 - 2 * theta1 * theta3 / (3 * theta2 ^ 2)
  z_alpha <- stats::qnorm(1 - alpha)
  spe1 <- z_alpha * sqrt(2 * theta2 * h0 ^ 2) / theta1
  spe2 <- theta2 * h0 * (h0 - 1) / theta1 ^ 2
  cl_spe <- theta1 * (spe1 + 1 + spe2) ^ (1 / h0)
  # T2 upper control limit
  F_alpha <- stats::qf(1 - alpha, ncomp, n - ncomp)
  cl_t2 <- (n ^ 2 - 1) * ncomp / (n * (n - ncomp)) * F_alpha
  # Score limits
  z <- ((n - 1) * (n - 1) / n) * stats::qbeta(1 - alpha, 1, (n - 3) / 2)
  limits_t <- list()
  for (i in 1:ncol(Tfull)){
    limits_t[[paste0('pc',i)]] = c(-sqrt(stats::var(Tfull[, i]) * z),
                                   sqrt(stats::var(Tfull[, i]) * z))
  }
  pcamodel <- list()
  pcamodel$m <- m
  pcamodel$s <- s
  pcamodel$P <- P
  pcamodel$Pfull <- Pfull
  pcamodel$lambda <- lambda
  pcamodel$limspe <- cl_spe
  pcamodel$limt2 <- cl_t2
  pcamodel$limits_t <- limits_t
  pcamodel$prepro <- prepro
  pcamodel$ncomp <- ncomp
  pcamodel$alpha <- alpha
  pcamodel$n <- n
  pcamodel$S <- stats::cov(X)
  return(pcamodel)
}

