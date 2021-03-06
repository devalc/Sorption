#' Estimate Parameters of Freundlich model
#'
#' This function estimates Freundlich isotherm model parameters using the linear form of Freundlich equation.
#' @param Ce equilibrium solution concentrations in mg/l
#' @param Qe retention by solid (adsorption) in mg/kg
#' @param  output_fname file name/path to which the parameters will be exported (e.g: "analysis/langmuir.csv" )
#' @return A csv file containing the estimated parameters
#' @import IDPmisc
#' @export


FreundlichParameters <- function(Ce, Qe, output_fname)
{
    x <- log10(Ce)
    y <- log10(Qe)
    z <- data.frame(x,y)
    z <- IDPmisc::NaRV.omit(z)
    fit <- lm(z$y ~ z$x)
    n <- fit$coefficients[[2]]
    logKf<- fit$coefficients[[1]]
    Kf<- (10**logKf)
    Par_names_header<-paste0("parameters_",sub(pattern = "(.*)\\..*$", replacement = "\\1", basename (output_fname)))
    Par_names<- c("logKf","n", "Kf")
    Par_values_header <- paste0("par_Values_",sub(pattern = "(.*)\\..*$", replacement = "\\1", basename (output_fname)))
    Par_values<- c(logKf,n,Kf)
    out_df <- data.frame(Par_names, Par_values)
    write.table(out_df, output_fname,sep = ",", row.names = FALSE, col.names = c(Par_names_header,  Par_values_header))
}
