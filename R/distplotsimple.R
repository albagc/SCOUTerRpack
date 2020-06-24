#'
#' displotsimple
#'
#' Returns the distance plot directly providing the coordiantes and Upper Control Limits. 
#' 
#' Coordinates are expressed in terms of the Hotelling's T^2 (x-axis) and the Squared Prediction
#' Error (y-axis) obtained projecting X on the provided pca model. 
#' Observations can be identified by the obstag input argument.
#'
#' @param T2 Vector with the Hotelling's T^2 values for each observation.
#' @param SPE Vector with the SPE values for each observation.
#' @param lim.t2 Value of the Upper Control Limit for the T^2 statistic.
#' @param lim.spe Value of the Upper Control Limit for the SPE.
#' @param ncomp An integer indicating the number of PCs.
#' @param obstag Optional column vector of integers indicating the group of each
#' observation (\code{0} or \code{1}). Default value set to \code{matrix(0, nrow(X), 1)}.
#' @param alpha Optional number between 0 and 1 expressing the type I risk assumed in the compuatation of the
#' Upper Control Limits set to \code{0.05} (5 %) by default.
#' @param plottitle Optional string with the plot title, \code{"Distance plot"} by default.
#' @return distplotobj ggplot object with the generated distance plot.
#' @export
distplotsimple <- function(T2, SPE, lim.t2, lim.spe, ncomp, obstag = matrix(0, length(T2), 1), alpha = 0.05,
                           plottitle =  "Distance plot\n"){
  a <- 1.1*max(c(T2, lim.t2))
  b <- 1.1*max(c(SPE, lim.spe))
  conflev <- (1 - alpha)*100
  # Distance plot
  set <- obstag > 0
  df.plot <- as.data.frame(cbind(T2, SPE, obstag, set))
  colnames(df.plot) = c("T2", "SPE", "tag", "set")
  df.plot$tag <- as.factor(df.plot$tag)
  df.plot$set[df.plot$set == 0] <- "Obs.ref"
  df.plot$set[df.plot$set == 1] <- "Obs.new"
  df.plot$set <- as.factor(df.plot$set)
  distplot <- ggplot2::ggplot(data = df.plot, ggplot2::aes(x = T2, y = SPE)) +
    # Upper Control Limits
    ggplot2::geom_vline(xintercept = lim.t2, colour = "red", linetype = "dashed", size = 0.75,
               show.legend = TRUE)+
    ggplot2::geom_hline(yintercept = lim.spe, colour = "red", linetype = "dashed", size = 0.75) +
    # Observations (points)
    ggplot2::geom_point(data = df.plot, mapping = ggplot2::aes(x = T2, y = SPE, colour = set, shape = set),
               size = 3, alpha = 0.5) +
    # Group series settings
    ggplot2::scale_colour_manual(name = "",
                        values = c("Obs.ref" = "blue", "Obs.new" = "red"),
                        breaks = c("Obs.ref", "Obs.new")) +
    ggplot2::scale_shape_manual(name = "", values = c(16, 17),
                       breaks = c("Obs.ref", "Obs.new")) +
    # Plot settings
    ggplot2::theme_bw() + ggplot2::coord_cartesian(ylim=c(0, b), xlim = c(0, a)) +
    ggplot2::labs(x = bquote(italic(~T[.(ncomp)]^2)), y = "SPE", title = plottitle,
         subtitle = bquote(UCL[.(paste0((1-alpha)*100, "%"))])) +
    ggplot2::guides(colour = ggplot2::guide_legend("", override.aes = list(alpha = 1)), shape = ggplot2::guide_legend("")) +
    ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
          axis.title.x = ggplot2::element_text(face = "italic", size = 8),
          axis.title.y = ggplot2::element_text(face = "italic", size = 8),
          legend.direction = "vertical", legend.text = ggplot2::element_text(size = 8),
          plot.subtitle = ggplot2::element_text(size = 6))
  return(distplot)
}
