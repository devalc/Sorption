#'  calibration model plot
#'
#' This function determines fits a linear model between absorbance and known
#' concentrations to determine the calibration curve equation
#' @param Ck Known concentrations (standards)
#' @param A Absorbance observed on spectrophotometer
#' @return returns a linear calibration model plot
#' @export


CalibrationPlot <- function(Ck,A, cor_lab_x , cor_lab_y ,
                         eq_lab_x, eq_lab_y,  file_name){

    x <- Ck
    y<- A
    fit <- lm(y ~ x)
    name<- paste0(file_name,".pdf")
    namepng<- paste0(file_name,".png")
    coeff = coefficients(fit)
    z <- data.frame(x,y)
    ggscatter(x = "x",y ="y", data = z, xlab = "Known concentration (mg/L)", ylab = " Absorbance", add = "reg.line",
              conf.int = TRUE,
              add.params = list(color = "blue",
                                fill = "lightgray")) +
        stat_cor(method = "pearson", label.x = cor_lab_x, label.y = cor_lab_y) + # Add correlation coefficient
        stat_regline_equation(label.y = eq_lab_y,label.x = eq_lab_x)
    ggsave(name)
    ggsave(namepng, dpi=300)

}
