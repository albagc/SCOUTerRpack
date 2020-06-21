#'
#' Display individual value with reference value of a variable.
#'
#' Returns a ggplot object with the individual value of a variable as a geom_col
#' with an horizontal line reference.
#'
#' @param x A vector witht the variable of interest.
#' @param iobs An integer with the observation index.
#' @param ucl A number with the horizontal reference value.
#' @param plotname Optional string with the plot title. Set to "" by default.
#' @param ylabelname Optional string with the y-axis label. Set to "" by default.
#' @importFrom grDevices rgb2hsv
#' @return Ggplot object with geom_col and geom_hline layers
#' @examples
#' barwithucl(c(1:40), 20, max(c(1:40)))
#' barwithucl(c(1:40), 20, max(c(1:40)), plotname = "Example", ylabel = "x value")
#' @export
barwithucl <- function(x, iobs, ucl, plotname = "", ylabelname = ""){
  df.plot <- data.frame(value = x[iobs], ucl = ucl, obs = iobs)
  barobj <- ggplot(df.plot) + geom_col(aes(x = iobs, y = value), alpha = 0.6,
                                           width = 0.75, fill = "blue", show.legend = FALSE) +
    geom_hline(aes(yintercept = ucl), linetype = 2, colour = "red", size = 0.75, show.legend = TRUE) +
    scale_linetype_manual(name = "", values = c("UCL", 2)) +
    theme_bw() + labs(x = "Obs. Index", y = ylabelname, title = paste0(plotname,"\n")) +
    theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(size = 18)) +
    scale_x_continuous(breaks = c(iobs-1, iobs, iobs+1)) +
    coord_cartesian(ylim = c(0, max(x, ucl)*1.1))
  return(barobj)
}
