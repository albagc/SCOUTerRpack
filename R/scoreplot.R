#'
#' Score plot
#'
#' \code{scoreplot} returns a ggplot object with the score plot for the observations of a matrix, expressing their coordinates
#' in terms of the Principal Components obtained according to the provided pca model. It also includes the option
#' of distinguishing beteween observations and a customized title and also displays the confidence ellipsoid for the scores for
#' a confidence level of (1-alpha)*100 %.
#'
#' @param X A matrix with the data to be displayed.
#' @param pcamodel A list wiht the PCA model elements.
#' @param obstag Optional array with an integer for each observation used as a group tag.
#' @param pcx Optional integer with the number of the PC in the horizontal axis, set to 1 by default.
#' @param pcy Optional integer with the number of the PC in the vertical axis, set to 2 by default.
#' @param alpha Optional number between 0 and 1 expressing the type I risk assumed in the compuatation of the confidence ellipse,
#' set to 0.05 (5 %) by default.
#' @param plottitle Optional string with the plot title, "Score plot" by default.
#'
#' @return scplotobj ggplot object with the generated score plot.
#'
#' @export

scoreplot <- function(X, pcamodel, obstag = matrix(0, nrow(X), 1), pcx = 1, pcy = 2, alpha = 0.05, plottitle = "Score plot\n"){
  # Calcualte the scores according to the PCA model in pcamodel list
  pcavalues <- pcame(X, pcamodel)
  scplot <- scoreplotsimple(pcavalues$Tscores, pcx = pcx, pcy  = pcy, obstag = obstag,
                            alpha = alpha, varT = pcamodel$lambda, plottitle = plottitle)
  return(scplot)
}

