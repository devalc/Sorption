#' Plot the Freundlich model fit to the experimental data
#'
#' This function plots the Freundlich isotherm model fit
#' @param Ce equilibrium solution concentrations in mg/l
#' @param Qe retention by solid (adsorption) mg/kg
#' @param  cor_lab_x,cor_lab_y location on the plot to place pearson r and p-value
#' @param eq_lab_x,eq_lab_y  location on the plot to place equation of the fitted line
#' @return plot
#' @import ggpubr
#' @import ggplot2
#' @import IDPmisc
#' @export

FreundlichPlot <- function(Ce, Qe, cor_lab_x , cor_lab_y ,
                         eq_lab_x, eq_lab_y){
    x <- log10(Ce)
    y <- log10(Qe)
    z <- data.frame(x,y)
    fit <- lm(z$y ~ z$x)
    coeff = coefficients(fit)
    z <- data.frame(x,y)
    ggscatter(x = "x",y ="y", data = z, xlab = "log10 [Ce (mg/L)]", ylab = " log10 [Qe (mg/kg)]", add = "reg.line",
              conf.int = TRUE,
              add.params = list(color = "blue",
                                fill = "lightgray")) +
        stat_cor(method = "pearson", label.x = cor_lab_x, label.y = cor_lab_y) + # Add correlation coefficient
        stat_regline_equation(label.y = eq_lab_y,label.x = eq_lab_x)
}
