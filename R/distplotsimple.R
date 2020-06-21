#'
#' Distance plot
#'
#' Returns a ggplot object expressing the observations according to their Hotelling's T^2 (T^2) and
#' Squared Prediction Error (SPE) coordinates. It also includes the Upper Control Limit references of each statistic, along with
#' the option of distinguishing beteween observations and a customized title.
#'
#' @param T2 Vector with the Hotelling's T^2 values for each observation.
#' @param SPE Vector with the SPE values for each observation.
#' @param lim.t2 Value of the Upper Control Limit for the T^2 statistic.
#' @param lim.spe Value of the Upper Control Limit for the SPE.
#' @param obstag Optional array with an integer for each observation used as a group tag. Set to \code{matrix(0, length(T2), 1)}
#' by default.
#' @param alpha Optional number between 0 and 1 expressing the type I risk assumed in the compuatation of the confidence ellipse,
#' set to 0.05 (5 %) by default.
#' @param plottitle Optional string with the plot title, \code{"Distance plot"} by default.
#'
#' @return distplotobj ggplot object with the generated distance plot.
#'
#' @export

distplotsimple <- function(T2, SPE, lim.t2, lim.spe, ncomp, obstag = matrix(0, length(T2), 1), alpha = 0.05,
                           plottitle =  "Distance plot\n"){

  a <- 1.1*max(c(T2, lim.t2))
  b <- 1.1*max(c(SPE, lim.spe))
  conflev <- (1 - alpha)*100

  # Score plot
  df.plot <- as.data.frame(cbind(T2, SPE, obstag, obstag > 0))
  colnames(df.plot) = c("T2", "SPE", "tag", "set")
  df.plot$tag <- as.factor(df.plot$tag)
  df.plot$set[df.plot$set == 0] <- "Obs.ref"
  df.plot$set[df.plot$set == 1] <- "Obs.new"
  df.plot$set <- as.factor(df.plot$set)

  distplot <- ggplot(data = df.plot, aes(x = T2, y = SPE)) +
    # Upper Control Limits
    geom_vline(xintercept = lim.t2, colour = "red", linetype = "dashed", size = 0.75,
               show.legend = TRUE)+
    geom_hline(yintercept = lim.spe, colour = "red", linetype = "dashed", size = 0.75) +
    # Observations (points)
    geom_point(data = df.plot, mapping = aes(x = T2, y = SPE, colour = set, shape = set),
               size = 3, alpha = 0.5) +
    # Group series settings
    scale_colour_manual(name = "",
                        values = c("Obs.ref" = "blue", "Obs.new" = "red"),
                        breaks = c("Obs.ref", "Obs.new")) +
    scale_shape_manual(name = "", values = c(16, 17),
                       breaks = c("Obs.ref", "Obs.new")) +
    # Plot settings
    theme_bw() + coord_cartesian(ylim=c(0, b), xlim = c(0, a)) +
    labs(x = bquote(italic(~T[.(ncomp)]^2)), y = "SPE", title = plottitle,
         subtitle = bquote(UCL[.(paste0((1-alpha)*100, "%"))])) +
    guides(colour = guide_legend("", override.aes = list(alpha = 1)), shape = guide_legend("")) +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 20),
          axis.title.x = element_text(face = "italic", size = 18),
          axis.title.y = element_text(face = "italic", size = 18),
          legend.direction = "vertical", legend.text = element_text(size = 18))

  return(distplot)
}
