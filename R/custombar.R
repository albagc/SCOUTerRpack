#'
#'custombar
#'
#'Bar plot with customized title and labels. Y-Axis limits are fixed
#'according to the range of the values in X.
#'
#' @param X matrix with observations as row vectors.
#' @param iobs index of the observations whose value will be displayed.
#' @param plotname string with the title of the plot. Set to "" by default.
#' @param ylabelname string with the y-axis label. Set to "Contribution" by default.
#' @param xlabelname string with the y-axis label. Set to "" by default.
#' @return ggplot object with the values of a vector with a customized geom_col layer.
#' @import ggplot2
#' @examples
#' X <- as.matrix(X)
#' custombar(X, 2)
#' custombar(X, 2, plotname = "Observation 2", ylabelname = bquote(x.["j"]), xlabelname= "Variables")
#' @export
custombar <- function(X, iobs, plotname = "", ylabelname = "Contribution", xlabelname = ""){
  df.plot <- data.frame(contribution = X[iobs,], element = seq(1,length(X[iobs,]), by = 1))
  ymax <- max(abs(X))
  barobj <- ggplot2::ggplot(df.plot) +
    ggplot2::geom_col(ggplot2::aes(x = seq(1,length(X[iobs,]), by = 1), y = X[iobs,], alpha = 0.6),
                                                         width = 0.75, fill = "blue", show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = 0, colour = "black") +
    ggplot2::scale_x_continuous(breaks = seq(1, ncol(X), by = 1)) +
    ggplot2::coord_cartesian(ylim = c(-ymax, ymax)*1.1) +
    ggplot2::labs(x = xlabelname, y = ylabelname, title = paste0(plotname,"\n")) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14), 
                   legend.position = "none",
                   axis.title.x = ggplot2::element_text(size = 12), 
                   axis.title.y = ggplot2::element_text(size = 12))
  return(barobj)
}
