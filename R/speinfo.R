#'
#' speinfo
#'
#' Information about the Squared Prediction Error (SPE) of an observation. Two subplots show the information of an observation regarding its
#' SPE statistic, i.e.: a bar plot indicating the value of the statistic for the observation, and a bar plot with the contribution
#' that each component had for the SPE value
#'
#' @param SPE Vector with values of the SPE statistic.
#' @param E Matrix with the contributions of each variable (columns) for each observation (rows) to the SPE. It is the error term obtained
#' from the unexplained part of X by the PCA model.
#' @param limspe Upper Control Limit for the SPE, at a certain confidence level (1-alpha)*100 %.
#' @param iobs Integer with the index of the observation of interest. Default value set to \code{NA}.
#' @return ggplot object with the generated bar plots.
#' @import ggplot2
#' @export
speinfo <- function(SPE, E, limspe, iobs = NA){
  bar.spe <- barwithucl(SPE, ucl = limspe, iobs = iobs, ylabelname = "SPE") +
    ggplot2::labs(title = bquote(italic("SPE")), y = bquote(italic("SPE"["i"])))
  cont.spe <- custombar(E, iobs = iobs, xlabelname = "Variables") +
    ggplot2::labs(title = bquote("Contributions to" ~italic("SPE")))
  combplot <- ggpubr::ggarrange(bar.spe, cont.spe, widths = c(1, 3))

  speplots <- list()
  speplots$bar.spe <- bar.spe
  speplots$cont.spe <- cont.spe
  speplots$combplot <- combplot
  return(speplots)
}
