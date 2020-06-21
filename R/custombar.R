#'
#' Customized bar plot
#'
#' \code{custombar} returns a ggplot object with the values of a vector with a customized geom_col layer.
#' The y-axis scale is built accounting the maximum absolute value in X. Therefore, the size of the columns is also
#' relative to the magnitude of the extreme values in X.
#'
#' @param X A matrix with the vector representing each observation stored in rows.
#' @param iobs An integer with the observation index.
#' @param plotname Optional string with the plot title. Set to  "" by default.
#' @param xlabelname Optional string with the x-axis label. Set to "Variables" by default.
#'
#' @return custbarob ggplot object with the geom_col layer and other customization layers.
#'
#' @examples
#'
#' @export

custombar <- function(X, iobs, plotname = "", xlabelname = "Variables"){
  df.plot <- data.frame(contribution = X[iobs,], element = seq(1,length(X[iobs,]), by = 1))
  pal.aas <- pal_aaas()
  ymax <- max(abs(X))
  ggplot(df.plot) + geom_col(aes(x = element, y = contribution, alpha = 0.6),
                             width = 0.75, fill = "blue", show.legend = FALSE) +
    geom_hline(yintercept = 0, colour = "black") + theme_bw() + coord_cartesian(ylim = c(-ymax, ymax)*1.1) +
    scale_x_continuous(breaks = seq(1, ncol(X), by = 1)) +
    labs(x = xlabelname, y = "contribution", title = paste0(plotname,"\n")) +
    theme(plot.title = element_text(hjust = 0.5, size = 20), legend.position = "none",
          axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 16))
}

