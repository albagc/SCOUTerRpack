#'
#' dotag
#'
#' Returns the tag vector to identify two different data sets
#'
#' @param X.zeros Matrix with the tag \code{0}.
#' @param X.ones Matrix with the tag \code{1}.
#' @return \code{tag.all} vector with 0-tags for observations in \code{X.zeros} and 1-tags for observations in \code{X.ones}.
#' @export

dotag <- function(X.zeros = NA, X.ones = NA){
  tag.all <- c(as.vector(matrix(0, nrow = nrow(X.zeros), 1)),
               as.vector(matrix(1, nrow = nrow(X.ones), 1)))
  return(tag.all)
}