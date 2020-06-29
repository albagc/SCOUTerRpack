#'
#' ht2info
#'
#' Returns information about T^2 statistic for an observation. Two subplots show the information of an observation regarding its
#' T^2 statistic, i.e.: a bar plot indicating the value of the statistic for the observation, and a bar plot with the contribution
#' that each component had for the T^2 value
#'
#' @param HT2 A vector with values of the Hotelling's T^2_A statistic.
#' @param T2matrix A matrix with the contributions of each PC (A columns) for each observation (rows) to the Hotelling's T^2_A statistic.
#' @param limht2 Upper Control Limit for the Hotelling's T^2_A statistic, at a certain confidence
#' level (1-alpha)*100 %.
#' @param iobs Integer with the index of the observation of interest. Default value set to \code{NA}.
#' @return ggplot object with the generated bar plots.
#' @import ggplot2 ggpubr
#' @examples 
#' X <- as.matrix(X)
#' pcamodel.ref <- pcamb_classic(X[1:40,], 2, 0.05, "cent") # PCA-MB with first 40 observations
#' pcaproj <- pcame(X[-c(1:40),], pcamodel.ref) # Project last observations
#' ht2info(pcaproj$T2, pcaproj$T2matrix, pcamodel.ref$limt2, 2) # Information about the T^2 of the row #2
#' @export
ht2info <- function(HT2, T2matrix, limht2, iobs = NA){
  bar.ht2 <- barwithucl(HT2, ucl = limht2, iobs = iobs)  +
    ggplot2::labs(title = bquote(italic(T[.(ncol(T2matrix))]^2)), y = bquote(italic(T[.(paste0(ncol(T2matrix), ",i"))]^2)))
  cont.ht2 <- custombar(T2matrix, iobs = iobs, xlabelname = "PCs") +
    ggplot2::labs(title = bquote("Contributions to" ~italic(T[.(ncol(T2matrix))]^2)))
  cont.ht2$coordinates$limits$y[1] <- 0
  combplot <- ggpubr::ggarrange(bar.ht2, cont.ht2, widths = c(1, 3))
  ht2plots <- list()
  ht2plots$bar.ht2 <- bar.ht2
  ht2plots$cont.ht2 <- cont.ht2
  ht2plots$combplot <- combplot
  return(ht2plots)
}
