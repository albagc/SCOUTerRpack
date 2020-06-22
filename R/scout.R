#'
#' scout
#'
#' Shift of an observation following a selected pattern.
#'
#' @param X Matrix with observations that will be shifted as rows.
#' @param pcaref List with the elemements of a PCA model: \code{m} (mean), \code{s} (standard deviation), \code{prepro} (preprocessing:
#' \code{"none"}, \code{"cent"} or \code{"autosc"}), \code{P} (loading matrix), \code{lambda} (vector with variances of each PC).
#' @param T2.y A number indicating the target value for the T^2_A after the shift. Set to \code{NA} by default.
#' @param SPE.y A number indicating the target value for the SPE after the shift. Set to \code{NA} by default.
#' @param nsteps A number indicating the number of steps between the reference and target values of the SPE and the T^2. Set to \code{1}
#' by default.
#' @param nsteps.spe An integer indicating the number of steps in which the shift from the reference to
#' the target value of the SPE will be performed. Set to \code{1} by default
#' @param nsteps.t2 An integer indicating the number of steps in which the shift from the reference to the
#' target value of the T^2_A will be performed. Set to \code{1} by default
#' @param gspe A mumber indicating the term that will tune the spacing between steps for the SPE. Set to \code{1} by default (linear spacing).
#' @param gt2 A mumber indicating the term that will tune the spacing between steps for the SPE. Set to \code{1} by default (linear spacing).
#' @param mode A character indicating the type of shift that will be performed: \code{"simple"}, \code{"steps"} or \code{"grid"}.
#' @return list with elements: \code{X}, matrix with the new and shifted data, \code{SPE} and \code{T2} vectors with the statistic values
#' of each one of the new generated outliers or observations, elements \code{step.spe} and \code{step.t2} make reference to the step
#' of each observation. Finally, the element \code{tag}, is a vector of ones as long as the number of generated observations.
#' @export
scout <- function(X, pcaref, T2.y = NA, SPE.y = NA, nsteps = 1, nsteps.spe = 1,
                       nsteps.t2 = 1, gspe = 1, gt2 = 1, mode = "simple"){
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
    outscout <- scoutsteps(X, pcaref, T2.target = T2.target.val, SPE.target = SPE.target.val, nsteps = n.steps.val,
                           gspe = gammaSPE.val, gt2 = gammaT2.val)
      } else if (mode == "grid"){
    outscout <- scoutgrid(X, pcaref, T2.target = T2.target.val, SPE.target = SPE.target.val, nsteps.spe = SPE.n.steps.val,
                          nsteps.t2 = T2.n.steps.val, gspe = gammaSPE.val, gt2 = gammaT2.val)
  }
  return(outscout)
}
