#' Plot the Linear model fit to the experimental data
#'
#' This function plots the Linear isotherm model fit
#' @param Ce equilibrium solution concentrations in mg/l
#' @param Qe retention by solid (adsorption) mg/kg
#' @param  cor_lab_x,cor_lab_y location on the plot to place pearson r and p-value
#' @param eq_lab_x,eq_lab_y  location on the plot to place equation of the fitted line
#' @param file_name file name/path to which the plot will be exported (e.g: "Plots/Langmuir_plt" )
#' @return A pdf and a png containing the graphics
#' @export


LinearPlot <- function(Ce, Qe, cor_lab_x , cor_lab_y ,
                         eq_lab_x, eq_lab_y,  file_name){

    x <- Ce
    y<- Qe
    fit <- lm(y ~ x)
    coeff = coefficients(fit)
    z <- data.frame(x,y)
    ggscatter(x = "x",y ="y", data = z, xlab = "Ce (mg/L)", ylab = " q (mg/kg)", add = "reg.line",
              conf.int = TRUE,
              add.params = list(color = "blue",
                                fill = "lightgray")) +
        stat_cor(method = "pearson", label.x = cor_lab_x, label.y = cor_lab_y) + # Add correlation coefficient
        stat_regline_equation(label.y = eq_lab_y,label.x = eq_lab_x)
}
