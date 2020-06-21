#'
#' Information about T^2 and SPE statistics of an observation.
#'
#' \code{obscontribpanel} returns a ggplot object with four subplots showing the information of an observation regarding
#' its T^2 and SPE statistics, i.e.: information about the distance of the observation in the latent space and its distance to the model
#' hyperplane. On one hand, it presents a bar plot indicating the value of each statistic for the observation, referenced by their
#' Upper Control Limita. On the other hand, it also presents the contributions of that observation both for the T^2 and the SPE.
#'
#' @param obsid Integer with the index of the observation of interest.
#' @param pcaref A list with the PCA model according to which the distance and contributions are expressed.
#' @param pcax A list with the elements of the PCA model that will be displayed: SPE, T^2_A and their constributions (E and T2matrix)
#'
#' @return contrplots ggplot object with the generated bar plots.
#'
#' @export

obscontribpanel <- function(obsid, pcaref, pcax){

  HT2A.plots <- ht2info(pcax$T2, pcax$T2matrix, pcaref$limt2, iobs = obsid)
  SPE.plots <- speinfo(pcax$SPE, pcax$E, pcaref$limspe, iobs = obsid)

  barplot.spe <- SPE.plots$bar.spe
  barplot.ht2 <- HT2A.plots$bar.ht2
  contrplot.spe <- SPE.plots$cont.spe
  contrplot.ht2 <- HT2A.plots$cont.ht2
  contrplots <- ggarrange(barplot.spe, contrplot.spe, barplot.ht2, contrplot.ht2, widths = c(1, 4, 1, 2),
                          nrow = 1, ncol = 4, common.legend = TRUE, legend = "bottom")
  return(contrplots)
}
