#'
#' barwithucl
#' 
#' Single bar plot with Upper Control Limis. Customized title and labels.
#' Y-Axis limits are fixed according to the range of the values in x.
#'
#' @param x vector with the values of the statistic.
#' @param iobs index of the observations whose value will be displayed.
#' @param ucl Upper Control Limit of the statistic.
#' @param plotname string with the title of the plot. Set to \code{""} by default.
#' @param ylabelname string with the y-axis label. Set to \code{""} by default.
#' @param xlabelname string with the y-axis label. Set to \code{"Obs. Index"} by default.
#' @return ggplot object with the individual value of a variable as a geom_col
#' with an horizontal line reference.
#' @export
barwithucl <- function(x, iobs, ucl, plotname = "", ylabelname = "", xlabelname = "Obs. Index"){
  df.plot <- data.frame(value = x[iobs], ucl = ucl, obs = iobs)
  barobj <- ggplot2::ggplot(df.plot) + ggplot2::geom_col(ggplot2::aes(x = iobs, y = x[iobs]), alpha = 0.6,
                                           width = 0.75, fill = "blue", show.legend = FALSE) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = ucl), linetype = 2, colour = "red", size = 0.75, show.legend = TRUE) +
    ggplot2::scale_linetype_manual(name = "", values = c("UCL", 2)) +
    ggplot2::scale_x_continuous(breaks = c(iobs-1, iobs, iobs+1)) +
    ggplot2::coord_cartesian(ylim = c(0, max(x, ucl)*1.1)) +
    ggplot2::labs(x = xlabelname, y = ylabelname, title = paste0(plotname,"\n")) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 20),
                   axis.title.x = ggplot2::element_text(size = 18))
  return(barobj)
}
