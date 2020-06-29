#'
#' dscplot
#'
#' Returns the distance plot and the score plot providing a data matrix and a Principal Component
#' Analysis (PCA) model. Observations can be identified by the obstag input argument.
#'
#' @param X Matrix with the data to be displayed.
#' @param pcamodel List with the PCA model elements.
#' @param obstag Optional column vector of integers indicating the group of each
#' observation (\code{0} or \code{1}). Default value set to \code{matrix(0, nrow(X), 1)}.
#' @param pcx Optional integer with the number of the PC in the horizontal axis. 
#' Set to \code{1} by default.
#' @param pcy Optional integer with the number of the PC in the vertical axis.
#' Set to \code{2} by default.
#' @param alpha Optional number between 0 and 1 expressing the type I risk assumed in 
#' the computation of the confidence ellipse,
#' set to \code{0.05} (5 %) by default.
#' @param nrow Optional number of rows the plot layout. Set to \code{1} by default.
#' @param ncol Optional number of columns the plot layout. Set to \code{2} by default.
#' @param legpos Optional string with the position of the legend. Set to \code{"bottom"} 
#' by default.
#' @return ggplot object with the generated score plot.
#' @import ggpubr
#' @examples 
#' X <- as.matrix(X)
#' pcamodel.ref <- pcamb_classic(X[1:40,], 3, 0.05, "cent")
#' dscplot(X, pcamodel.ref)
#' dscplot(X, pcamodel.ref, nrow = 2, ncol = 1)
#' tags <- dotag(X[1:40,], X[-c(1:40),])
#' dscplot(X, pcamodel.ref, obstag = tags, pcy = 3)
#' @export

dscplot <- function(X, pcamodel, obstag = matrix(0, nrow(X), 1), pcx = 1, pcy = 2, 
                    alpha = 0.05, nrow = 1, ncol = 2, legpos = "bottom"){
  Tplot <- scoreplot(X, pcamodel, pcx = 1, pcy = 2, alpha = alpha, obstag = obstag)
  Dplot <- distplot(X, pcamodel, obstag = obstag)
  dsclayout <- ggpubr::ggarrange(Dplot, Tplot, nrow = nrow, ncol = ncol, 
                                 common.legend = TRUE, legend = legpos)
  return(dsclayout)
}
