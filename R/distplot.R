#'
#' Distance plot
#'
#' \code{distplot} returns a ggplot object with the distance plot for the observations of a matrix, expressing their coordinates
#' in terms of the Squared Prediction Error and the Hotelling's T^2 according to the provided pca model. It also includes the option
#' of distinguishing beteween observations and a customized title.
#'
#' @param X A matrix with the data to be displayed.
#' @param pcamodel A list wiht the PCA model elements.
#' @param obstag Optional array with an integer for each observation used as a group tag.
#' @param plottitle Optional string with the plot title. Set to \code{"Distance plot"} by default.
#'
#' @return distplotobj ggplot object with the generated distance plot.
#'
#' @export


distplot <- function(X, pcamodel, obstag = matrix(0, nrow(X), 1), plottitle =  "Distance plot\n") {
  # Calcualte the scores according to the PCA model in pcamodel list
  pcavalues <- pcame(X, pcamodel)
  distplot <- distplotsimple(pcavalues$T2, pcavalues$SPE, pcamodel$limt2, pcamodel$limspe, pcamodel$ncomp,
                             obstag = obstag, plottitle = plottitle, alpha = pcamodel$alpha)
  return(distplot)

}
