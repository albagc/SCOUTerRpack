#'
#' PCA model fitting according to a matrix X using svd.
#'
#' \code{pcamb_classic} performs the PCA model building (PCA-MB) using the data in X and returning the different elements of the PCA model.
#'
#' @param X A vector or matrix with observations that will used to fit the PCA model.
#' @param ncomp An integer indicating the number of PCs that the model will have.
#' @param alpha A number between 0 and 1 indicating the type I risk assumed to calculate the Upper Control Limits for the SPE, the T^2_A and
#' the scores. The confidence level of these limits will be (1-alpha)*100.
#' @param prepro A string indicating the preprocessing that should be performed on X. Its possible values are: "none", if any preprocessing
#' should be performed on X, "cent", if a mean-centering should be performed on X, or "autosc", it a mean-centering and unitary variance
#' scaling (autoscaling) should be performed on X.
#'
#' @return pcamodel list with fields containing information about PCA model: m (mean vector), s (standard deviation vector),
#' P (loading matrix with the loadings of each PC stored as columns), Pfull (full loading matrix obtained by the svd),
#' lambda (vector with the variance of each PC), limspe (Upper Control Limit for the SPE with a confidence level (1-alpha)*100 %),
#' limt2 (Upper Control Limit for the T^2_A with a confidence level (1-alpha)*100 %), limits_t (Upper control Limits for the scores with a
#'  confidence level (1-alpha)*100 %)), prepro (character indicating the type of preprocessing performed on X),
#'  ncomp (number of PCs of the PCA model, A), alpha (value of the type I risk assumed to calculate the Upper Control Limits of the SPE,
#'  T^2_A and scores), n (dimension of the number of rows in X), S (covariance matrix of X).
#'
#' @export

pcamb_classic <- function(X, ncomp, alpha, prepro) {
  m <- colMeans(X)
  s <- apply(X, 2, sd)
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
  Lambda <- diag(var(Tsc))
  ev <- eigen(cov(E))
  LambdaE <- rev(ev$values)
  lambda <- t(Lambda)
  T2 <- rowSums(Tsc ^2 / kronecker(matrix(1, n, 1),lambda))
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
  z_alpha <- qnorm(1 - alpha)
  spe1 <- z_alpha * sqrt(2 * theta2 * h0 ^ 2) / theta1
  spe2 <- theta2 * h0 * (h0 - 1) / theta1 ^ 2
  cl_spe <- theta1 * (spe1 + 1 + spe2) ^ (1 / h0)
  # T2 upper control limit
  F_alpha <- qf(1 - alpha, ncomp, n - ncomp)
  cl_t2 <- (n ^ 2 - 1) * ncomp / (n * (n - ncomp)) * F_alpha
  # Score limits
  z <- ((n - 1) * (n - 1) / n) * qbeta(1 - alpha, 1, (n - 3) / 2)
  limits_t <- list()
  for (i in 1:ncol(Tfull)){
    limits_t[[paste0('pc',i)]] = c(-sqrt(var(Tfull[, i]) * z),
                                   sqrt(var(Tfull[, i]) * z))
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
  pcamodel$S <- cov(X)

  return(pcamodel)
}

