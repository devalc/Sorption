#'  calibration model
#'
#' This function determines fits a linear model between absorbance and known
#' concentrations to determine the calibration curve equation
#' @param Ck Known concentrations (standards)
#' @param A Absorbance observed on spectrophotometer
#' @return returns a linear calibration model and plot
#' @export

CalibrationPlot <- function(Ck,A){
    fit <- lm(A ~ Ck)
    c <- fit$coefficients[[1]]
    m<- fit$coefficients[[2]]
    slope_and_intercept <- c(m,c)
    return(slope_and_intercept)
}
