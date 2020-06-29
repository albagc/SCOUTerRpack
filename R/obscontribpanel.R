#'
#' obscontribpanel
#'
#' Information about the Hotelling's T^2 and the Squared Predidiction Error (SPE) 
#' of an observation. The term T^2_A makes reference to the T^2 for a model with 
#' A principal components (PCs).
#'
#' @param pcax A list with the elements of the PCA model that will be displayed:
#' SPE, T^2_A and their constributions (E and T2matrix).
#' @param pcaref A list with the PCA model according to which the distance and 
#' contributions are expressed.
#' @param obsid Integer with the index of the observation of interest. Default 
#' set to \code{NA}.
#' @return ggplot object with the generated bar plots in a 1 x 4 subplots layout.
#' @import ggpubr
#' @examples 
#' X <- as.matrix(X)
#' pcamodel.ref <- pcamb_classic(X[1:40,], 2, 0.05, "cent") # PCA-MB with first 
#' # 40 observations
#' pcaproj <- pcame(X[-c(1:40),], pcamodel.ref) # Project last observations
#' obscontribpanel(pcaproj, pcamodel.ref, 2) # Information about the SPE and T^2 
#' # of the row #2
#' @export
obscontribpanel <- function(pcax, pcaref, obsid = NA){
  HT2A.plots <- ht2info(pcax$T2, pcax$T2matrix, pcaref$limt2, iobs = obsid)
  SPE.plots <- speinfo(pcax$SPE, pcax$E, pcaref$limspe, iobs = obsid)
  barplot.spe <- SPE.plots$bar.spe
  barplot.ht2 <- HT2A.plots$bar.ht2
  contrplot.spe <- SPE.plots$cont.spe
  contrplot.ht2 <- HT2A.plots$cont.ht2
  contrplots <- ggpubr::ggarrange(barplot.spe, contrplot.spe, barplot.ht2, 
                                  contrplot.ht2, widths = c(1, 4, 1, 2),
                          nrow = 1, ncol = 4, common.legend = TRUE, legend = "bottom")
  return(contrplots)
}
