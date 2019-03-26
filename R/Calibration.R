#'  calibration model
#'
#' This function determines fits a linear model between absorbance and known
#' concentrations to determine the calibration curve equation
#' @param Ck Known concentrations (standards)
#' @param A Absorbance observed on spectrophotometer
#' @return returns a linear calibration model and plot
#' @export

CalibrationCurve <- function(Ck,A){
    fit <- lm(A ~ Ck)
    c <- fit$coefficients[[1]]
    m<- fit$coefficients[[2]]
    slope_and_intercept <-vector(mode="list", length=2)
    names(slope_and_intercept) <- c("slope","intercept")
    slope_and_intercept[[1]] <- m
    slope_and_intercept[[2]] <- c
    return(slope_and_intercept)
}

