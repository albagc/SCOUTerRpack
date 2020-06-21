#'
#' Shift of an observation following a selected pattern.
#'
#' \code{scout} returns a new list with the information about the shifts performed to the observations in X.
#'
#' @param X A vector or matrix with observations that will be shifted as rows.
#' @param pcaref A list with the elemements of a PCA model: m (mean), s (standard deviation), prepro (preprocessing),
#' P (loading matrix), lambda (vector with variances of each PC). The preprocessing element is a character with possible values "none",
#' if any preprocessing should be performed on X, "cent", if a mean-centering should be performed on X, or "autosc", it a mean-centering
#' and unitary variance scaling (autoscaling) should be performed on X.
#' @param T2.y A number indicating the target value for the T^2_A after the shift. Set to NA by default.
#' @param SPE.y A number indicating the target value for the SPE after the shift. Set to NA by default.
#' @param nsteps A number indicating the number of steps between the reference and target values of the SPE and the T^2. Set to 1 by default.
#' @param nsteps.spe An integer indicating the number of steps in which the shift from the reference to
#' the target value of the SPE will be performed. Set to 1 by default
#' @param nsteps.t2 An integer indicating the number of steps in which the shift from the reference to the
#' target value of the T^2_A will be performed. Set to 1 by default
#' @param gspe A mumber indicating the term that will tune the spacing between steps for the SPE. Set to 1 by default (linear spacing).
#' @param gt2 A mumber indicating the term that will tune the spacing between steps for the SPE. Set to 1 by default (linear spacing).
#' @param mode A character indicating the type of shift that will be performed: "simple", "steps" or "grid".
#' @param tag.mode A character with value "series" if the tag output vector should discriminate between steps or not. Set to "" by default.
#'
#' @return scoutgrid list with elements with information about the shifted data. The matrix X, contains the new data, the SPE and T2 contain
#' the statistic values of each one of the new generated outliers or observations. The elements step.spe and step.t2 make reference to the step
#' at which each observation of the shifted dada is located. Finally, the element tag, has different values for each observation in X according
#' to each step (if tag.mode == "series"), otherwise is a vector of ones with as many elements as rows in X.


scout <- function(X, pcaref, T2.y = NA, SPE.y = NA, nsteps = 1, nsteps.spe = 1,
                       nsteps.t2 = 1, gspe = 1, gt2 = 1, mode = "simple", tag.mode = ""){
  T2.target.val <- T2.y
  SPE.target.val <- SPE.y
  n.steps.val <- nsteps
  T2.n.steps.val <- nsteps.t2
  SPE.n.steps.val <- nsteps.spe
  gammaSPE.val <- gspe
  gammaT2.val <- gt2
  if (mode == "simple"){
    outscout <- scoutsimple(X, pcaref, T2.target = T2.target.val, SPE.target = SPE.target.val)
  } else if (mode == "steps"){
    outscout <- scoutsteps(X, pcaref, T2.M = T2.target.val, SPE.M = SPE.target.val, nsteps = n.steps.val,
                           gspe = gammaSPE.val, gt2 = gammaT2.val, tag.mode = tag.mode)
      } else if (mode == "grid"){
    outscout <- scoutgrid(X, pcaref, T2.M = T2.target.val, SPE.M = SPE.target.val, nsteps.spe = SPE.n.steps.val,
                          nsteps.t2 = T2.n.steps.val, gspe = gammaSPE.val, gt2 = gammaT2.val, tag.mode = tag.mode)
  }
  return(outscout)
}
