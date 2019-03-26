#' Calculate Concentrations in Samples based on Calibration curve
#'
#' This function determines the amount of chemical (Ce) in solution
#' @param SampleAbs Absorbance values for samples
#' @param CalibSlope Slope of the calibration curve
#' @param CalibIntercept Intercept of the calibration curve
#' @param DF Dilution factor. Default set to 1
#' @return Concentration of chemical in solution (mg/L)
#' @export


SampleAbstoConc <- function(SampleAbs, CalibSlope, CalibIntercept, DF =1){
    if (CalibIntercept < 0) {
        Ce <-  ((SampleAbs + abs(CalibIntercept))/ CalibSlope)*DF
    } else{
        Ce <-  ((SampleAbs - abs(CalibIntercept))/ CalibSlope)*DF
    }
    return(Ce)
}
