#'
#' distplot
#'
#' Returns the distance plot providing a dataset and a pca model. 
#' 
#' Coordinates are expressed in terms of the Hotelling's T^2 (x-axis) and the Squared Prediction
#' Error (y-axis) obtained projecting X on the provided pca model. 
#' Observations can be identified by the obstag input argument.
#'
#' @param X data matrix with observations to be displayed in the distance plot.
#' @param pcaref list with the information of the PCA model.
#' @param obstag Optional column vector of integers indicating the group of each
#' observation (\code{0} or \code{1}). Default value set to \code{matrix(0, nrow(X), 1)}.
#' @param plottitle Optional string with the plot title. Set to \code{"Distance plot"} by default.
#' @return ggplot object with the distance plot.
#' @examples
#' X <- as.matrix(X)
#' pcamodel.ref <- pcamb_classic(X, 2, 0.05, "cent")
#' distplot(X, pcamodel.ref)
#' tags <- dotag(X[1:40,], X[-c(1:40),])
#' distplot(X, pcamodel.ref, obstag = tags, plottitle = "D plot title")
#' @export
distplot <- function(X, pcaref, obstag = matrix(0, nrow(X), 1), plottitle =  "Distance plot\n") {
  # Calcualte the scores according to the PCA model in pcamodel list
  pcavalues <- pcame(X, pcaref)
  distplot <- distplotsimple(pcavalues$T2, pcavalues$SPE, pcaref$limt2, pcaref$limspe, pcaref$ncomp,
                             obstag = obstag, plottitle = plottitle, alpha = pcaref$alpha)
  return(distplot)

}
