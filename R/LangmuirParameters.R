#' Estimate Parameters of Langmuir model
#'
#' This function estimates langmuir isotherm model parameters using the linear form of langmuir equation.
#' @param Ce equilibrium solution concentrations in mg/l
#' @param Qe retention by solid (adsorption) in mg/kg
#' @param  output_fname
#' @return A csv file containing the estimated parameters
#' @export



LangmuirParameters <- function(Ce, Qe, output_fname)
{
    x <- Ce
    y <- Ce/Qe
    fit <- lm(y ~ x)
    c <- fit$coefficients[1]
    slp<- fit$coefficients[2]
    qmax <- 1/slp
    KL<- (c/slp)
    Par_names_header<-paste0("parameters_",sub(pattern = "(.*)\\..*$", replacement = "\\1", basename (output_fname)))
    Par_names<- c("intercept","slope(1/qmax(mg/Kg))", "qmax(mg/Kg)", "KL")
    Par_values_header <- paste0("par_Values_",sub(pattern = "(.*)\\..*$", replacement = "\\1", basename (output_fname)))
    Par_values<- c(c,slp,qmax,KL)
    out_df <- data.frame(Par_names, Par_values)
    write.table(out_df, output_fname,sep = ",", row.names = FALSE, col.names = c(Par_names_header,  Par_values_header))
}
