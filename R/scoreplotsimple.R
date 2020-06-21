#'
#' Score plot
#'
#' \code{scoreplotsimple} returns a ggplot object with the score plot for the observations of a matrix, expressing their coordinates
#' in terms of the Principal Components obtained according to the provided pca model. It also includes the option
#' of distinguishing beteween observations and a customized title and also displays the confidence ellipsoid for the scores for
#' a confidence level of (1-alpha)*100 %
#'
#' @param Tscores A matrix with the scores to be displayed, with the information of each PC stored by columns.
#' @param pcx Optional integer with the number of the PC in the horizontal axis, set to 1 by default.
#' @param pcy Optional integer with the number of the PC in the vertical axis, set to 2 by default.
#' @param obstag Optional array with an integer for each observation used as a group tag. Set to \code{matrix(0, nrow(Tscores), 1)}
#' by default.
#' @param alpha Optional number between 0 and 1 expressing the type I risk assumed in the compuatation of the confidence ellipse,
#' set to 0.05 (5 %) by default.
#' @param varT Optional parameter expressing the variance of each PC, set to \code{var(Tscores)} by default.
#' @param plottitle Optional string with the plot title, \code{"Score plot"} by default.
#'
#' @return scplotobj ggplot object with the generated score plot.
#'
#' @export

scoreplotsimple <- function(Tscores, pcx = 1, pcy  = 2, obstag = matrix(0, nrow(Tscores), 1),
                            alpha = 0.05, varT = var(Tscores), plottitle = "Score plot\n"){

  t1 <- Tscores[, pcx]
  t2 <- Tscores[, pcy]
  n <- nrow(Tscores)
  z <- ((n - 1) * (n - 1) / n) * qbeta(1 - alpha, 1, (n - 3) / 2)
  limits.t1 = sqrt(varT[pcx] * z) # horizontal radius
  limits.t2 = sqrt(varT[pcy] * z) # vertical radius
  # Confidence ellipse (x,y)
  t.ell <- seq(-pi, pi, by = 0.01)
  x.ell <- limits.t1 * cos(t.ell)
  y.ell <- limits.t2 * sin(t.ell)
  T.ellipse <- data.frame(x.ell, y.ell)

  tmin1 <- min(rbind(t1, - x.ell)) * 1.1
  tmax1 <- max(rbind(t1, x.ell)) * 1.1
  tlim1 <- max(abs(cbind(tmin1, tmax1)))
  tmin2 <- min(rbind(t2, - limits.t1)) * 1.1
  tmax2 <- max(rbind(t2, - limits.t1)) * 1.1
  tlim2 <- max(abs(cbind(tmin2, tmax2)))
  tmaxall <- max(cbind(tlim1, tlim2))
  conflev <- (1 - alpha)*100

  # Score plot
  df.plot <- as.data.frame(cbind(t1, t2, obstag, obstag > 0))
  colnames(df.plot) <- c("t1", "t2", "tag", "set")
  df.plot$tag <- as.factor(df.plot$tag)
  df.plot$set[df.plot$set == 0] <- "Obs.ref"
  df.plot$set[df.plot$set == 1] <- "Obs.new"
  df.plot$set <- as.factor(df.plot$set)

  scplot <- ggplot(data = df.plot, aes(x = t1, y = t2)) +
    # Confidence ellipse
    geom_path(data = T.ellipse, mapping = aes(x = x.ell, y = y.ell), linetype = "dashed",
              colour = "red", size = 0.75) +
    geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
    # Scores (points)
    geom_point(data = df.plot, mapping = aes(x = t1, y = t2, colour = set, shape = set),
               size = 3, alpha = 0.5) +
    # Group series settings
    scale_colour_manual(name = "",
                        values = c("Obs.ref" = "blue", "Obs.new" = "red"),
                        breaks = c("Obs.ref", "Obs.new")) +
    scale_shape_manual(name = "", values = c(16, 17),
                       breaks = c("Obs.ref", "Obs.new")) +
    # Plot settings
    theme_bw() + coord_cartesian(ylim=c(-tmaxall, tmaxall), xlim = c(-tmaxall, tmaxall)) +
    labs(x = bquote(italic(t[.(pcx)])), y = bquote(italic(t[.(pcy)])), title = plottitle,
         subtitle = bquote(Conf.Ellipse[.(paste0((1-alpha)*100, "%"))])) +
    guides(colour = guide_legend("", override.aes = list(alpha = 1)), shape = guide_legend("")) +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 20),
          axis.title.x = element_text(face = "italic", size = 18),
          axis.title.y = element_text(face = "italic", size = 18),
          legend.direction = "vertical", legend.text = element_text(size = 18))
  return(scplot)
}
