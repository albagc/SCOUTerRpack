#'
#' scout
#'
#' Shift of an observation following a selected pattern.
#'
#' @param X Matrix with observations that will be shifted as rows.
#' @param pcaref List with the elements of a PCA model: 
#' * \code{m}: mean.
#' * \code{s}: standard deviation.
#' * \code{prepro}: preprocessing: \code{"none"}, \code{"cent"} or \code{"autosc"}.
#' * \code{P}: loading matrix.
#' * \code{lambda}: vector with variances of each PC.
#' @param T2.y A number indicating the target value for the Hotelling's T^2_A after the shift. 
#' Set to \code{NA} by default.
#' @param SPE.y A number indicating the target value for the Squared Prediction Error after the 
#' shift. Set to \code{NA} by default.
#' @param nsteps A number indicating the number of steps between the reference and target 
#' values of the SPE and the T^2. Set to \code{1} by default.
#' @param nsteps.spe An integer indicating the number of steps in which the shift from 
#' the reference to the target value of the SPE will be performed. Set to \code{1} by default.
#' @param nsteps.t2 An integer indicating the number of steps in which the shift from the 
#' reference to the target value of the T^2_A will be performed. Set to \code{1} by default.
#' @param gspe A number indicating the term that will tune the spacing between steps for the SPE. 
#' Set to \code{1} by default (linear spacing).
#' @param gt2 A number indicating the term that will tune the spacing between steps for the SPE. 
#' Set to \code{1} by default (linear spacing).
#' @param mode A character indicating the type of shift that will be performed: \code{"simple"}, 
#' \code{"steps"} or \code{"grid"}.
#' @param A PC selected to perform the shift.
#' @return list with elements: 
#' * \code{X}: matrix with the new and shifted data.
#' * \code{SPE}: SPE of each one of the generated outliers in the list element \code{X}. 
#' * \code{T2}: T^2 of each one of the generated outliers in the list element \code{X}.
#' * \code{step.spe}: step of each observation according to the shift of the SPE.
#' * \code{step.t2}: step of each observation according to the shift of the T^2.
#' * \code{tag}: is a vector of ones as long as the number of generated observations.
#' @examples 
#' X <- as.matrix(X)
#' pcamodel.ref <- pcamb_classic(X, 3, 0.1, "autosc") # PCA-MB with all observations
#' # Shift the first observation:
#' outscout <- scout(X[1,], pcamodel.ref, T2.y = 40, SPE.y = 50, nsteps.spe = 3, nsteps.t2 = 2, 
#' gspe = 3, gt2 = 0.5, mode = "grid")
#' 
#' # Shift a set of observations increasing only the T^2 in one step:
#' outscout <- scout(X, pcamodel.ref, T2.y = matrix(40, nrow(X), 1), mode = "simple")
#' @export
scout <- function(X, pcaref, T2.y = NA, SPE.y = NA, nsteps = 1, nsteps.spe = 1,
                       nsteps.t2 = 1, gspe = 1, gt2 = 1, mode = "simple", A = 0){
  T2.target.val <- T2.y
  SPE.target.val <- SPE.y
  n.steps.val <- nsteps
  T2.n.steps.val <- nsteps.t2
  SPE.n.steps.val <- nsteps.spe
  gammaSPE.val <- gspe
  gammaT2.val <- gt2
  if (mode == "simple"){
    outscout <- scoutsimple(X, pcaref, A = A, 
                            T2.target = T2.target.val, SPE.target = SPE.target.val)
  } else if (mode == "steps"){
    outscout <- scoutsteps(X, pcaref, A = A,
                           T2.target = T2.target.val, SPE.target = SPE.target.val, 
                           nsteps = n.steps.val,
                           gspe = gammaSPE.val, gt2 = gammaT2.val)
      } else if (mode == "grid"){
    outscout <- scoutgrid(X, pcaref, A = A, 
                          T2.target = T2.target.val, SPE.target = SPE.target.val, 
                          nsteps.spe = SPE.n.steps.val, nsteps.t2 = T2.n.steps.val, 
                          gspe = gammaSPE.val, gt2 = gammaT2.val)
  }
  return(outscout)
}
