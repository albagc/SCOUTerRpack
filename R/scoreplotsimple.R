#'
#' scoreplotsimple
#' 
#' Returns the score plot providing the scores matrix, \strong{T}. Observations can 
#' be identified by the obstag input argument.
#'
#' @param Tscores Matrix with the scores to be displayed, with the information of each PC stored by columns.
#' @param pcx Optional integer with the number of the PC in the horizontal axis. Set to \code{1} by default.
#' @param pcy Optional integer with the number of the PC in the vertical axis. Set to \code{2} by default.
#' @param obstag Optional column vector of integers indicating the group of each
#' observation (\code{0} or \code{1}). Default value set to \code{matrix(0, nrow(X), 1)}.
#' @param alpha Optional number between 0 and 1 expressing the type I risk assumed in the compuatation of the confidence ellipse,
#' set to \code{0.05} (5 %) by default.
#' @param varT Optional parameter expressing the variance of each PC. Set to \code{var(Tscores)} by default.
#' @param plottitle Optional string with the plot title. Set to \code{"Score plot"} by default.
#' @return ggplot object with the generated score plot.
#' @import ggplot2 stats
#' @export
scoreplotsimple <- function(Tscores, pcx = 1, pcy  = 2, obstag = matrix(0, nrow(Tscores), 1),
                            alpha = 0.05, varT = stats::var(Tscores), plottitle = "Score plot\n"){

  t1 <- Tscores[, pcx]
  t2 <- Tscores[, pcy]
  n <- nrow(Tscores)
  z <- ((n - 1) * (n - 1) / n) * stats::qbeta(1 - alpha, 1, (n - 3) / 2)
  limits.t1 = sqrt(varT[pcx] * z) # horizontal radius
  limits.t2 = sqrt(varT[pcy] * z) # vertical radius

  # Confidence ellipse (x,y)
  t.ell <- seq(-pi, pi, by = 0.01)
  x.ell <- limits.t1 * cos(t.ell)
  y.ell <- limits.t2 * sin(t.ell)
  T.ellipse <- data.frame(x.ell, y.ell)

  tmin1 <- min(c(t1, - x.ell)) * 1.1
  tmax1 <- max(c(t1, x.ell)) * 1.1
  tlim1 <- max(abs(cbind(tmin1, tmax1)))
  tmin2 <- min(c(t2, - limits.t1)) * 1.1
  tmax2 <- max(c(t2, - limits.t1)) * 1.1
  tlim2 <- max(abs(cbind(tmin2, tmax2)))
  tmaxall <- max(cbind(tlim1, tlim2))
  conflev <- (1 - alpha)*100

  # Score plot
  set <- obstag > 0
  df.plot <- as.data.frame(cbind(t1, t2, obstag, set))
  colnames(df.plot) <- c("t1", "t2", "tag", "set")
  df.plot$tag <- as.factor(df.plot$tag)
  df.plot$set[df.plot$set == 0] <- "Obs.ref"
  df.plot$set[df.plot$set == 1] <- "Obs.new"
  df.plot$set <- as.factor(df.plot$set)

  scplot <- ggplot2::ggplot(data = df.plot, ggplot2::aes(x = t1, y = t2)) +
    # Confidence ellipse
    ggplot2::geom_path(data = T.ellipse, mapping = ggplot2::aes(x = x.ell, y = y.ell), linetype = "dashed",
              colour = "red", size = 0.75) +
    ggplot2::geom_vline(xintercept = 0) + ggplot2::geom_hline(yintercept = 0) +
    # Scores (points)
    ggplot2::geom_point(data = df.plot, mapping = ggplot2::aes(x = t1, y = t2, colour = set, shape = set),
               size = 3, alpha = 0.5) +
    # Group series settings
    ggplot2::scale_colour_manual(name = "",
                        values = c("Obs.ref" = "blue", "Obs.new" = "red"),
                        breaks = c("Obs.ref", "Obs.new")) +
    ggplot2::scale_shape_manual(name = "", values = c(16, 17),
                       breaks = c("Obs.ref", "Obs.new")) +
    # Plot settings
    ggplot2::theme_bw() + ggplot2::coord_cartesian(ylim=c(-tmaxall, tmaxall), xlim = c(-tmaxall, tmaxall)) +
    ggplot2::labs(x = bquote(italic(t[.(pcx)])), y = bquote(italic(t[.(pcy)])), title = plottitle,
         subtitle = bquote(Conf.Ellipse[.(paste0((1-alpha)*100, "%"))])) +
    ggplot2::guides(colour = ggplot2::guide_legend("", override.aes = list(alpha = 1)),
                    shape = ggplot2::guide_legend("")) +
    ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
          axis.title.x = ggplot2::element_text(face = "italic", size = 10),
          axis.title.y = ggplot2::element_text(face = "italic", size = 10),
          legend.direction = "vertical", legend.text = ggplot2::element_text(size = 12))
  return(scplot)
}
