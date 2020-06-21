#'
#' Information about Squared Prediction Error (SPE) for an observation.
#'
#' \code{speinfo} returns a ggplot object with two subplots showing the information of an observation regarding its SPE  statistic, i.e.:
#' information about the distance of the observation to the the PCA model hyperplane. On one hand, it presents a bar plot indicating
#' the value of the statistic for the observation, referenced by the Upper Control Limit. On the other hand, it also presents
#' the contribution that each component had for the SPE value of the selected observation (i.e. its error vector).
#'
#' @param SPE A vector with values of the SPE statistic.
#' @param E A matrix with the contributions of each variable (columns) for each observation (rows) to the SPE. It is the error term obtained
#' from the unexplained part of X by the PCA model.
#' @param limht2 A number with the value of the Upper Control Limit for the SPE (at a certain confidence level (1-alpha)*100 %).
#' @param iobs Integer with the index of the observation of interest.
#'
#' @return speplots ggplot object with the generated bar plots.
#'
#' @export

speinfo <- function(SPE, E, limspe, iobs = iobs){

  bar.spe <- barwithucl(SPE, ucl = limspe, iobs = iobs, ylabelname = "SPE") +
    labs(title = bquote(italic("SPE")), y = bquote(italic("SPE"["i"])))
  cont.spe <- custombar(E, iobs = iobs, xlabelname = "Variables") +
    labs(title = bquote("Contributions to" ~italic("SPE")))

  combplot <- ggarrange(bar.spe, cont.spe, widths = c(1, 3))

  speplots <- list()
  speplots$bar.spe <- bar.spe
  speplots$cont.spe <- cont.spe
  speplots$combplot <- combplot
  return(speplots)

}
