#' Fits a non linear form of Langmuir model to the Ce and Qe data
#'
#' This function determines the amount of chemical (Ce) in solution
#' @param df data frame containing the data
#' @param Ce equilibrium solution concentrations in mg/l
#' @param Qe retention by solid (adsorption) in mg/kg
#' @param n_guess Guestimate of n
#' @param Kf_guess Guestimate of Kf
#' @return A plot of predicted non linear form of Langmuire model
#' with the Qmax (maximum sorption capacity) and Kl (langmuir constant
#' pertaining to the affinity of substrate towards the chemical) values
#' @export


NonLinearFreundlichPlot <- function(df, Ce, Qe, n_guess, Kf_guess){
    nlm_model<-nls(formula = Qe ~ kf*(Ce)**n,  data = df, start = list(n = n_guess, kf = Kf_guess), algorith = "port" )
    q <- summary(nlm_model)
    coef<-round(coef(nlm_model), 2)
    Qmax<- paste0("n = ", coef[1])
    Kl<-  paste0("Kf (L/kg)= ", coef[2] )
    xNew <- seq(range(Ce)[1],range(Ce)[2],length.out = 200)
    yNew <- predict(nlm_model,list(Ce = xNew))
    plot(Ce, Qe,  pch=25, bg = "blue", xlab = "Ce (mg/L)",
         ylab = "Q (mg/kg)", cex.lab=1.2)
    lines(xNew,yNew, col='red')
    mtext(Qmax, 1, line=-4, adj = 0.9)
    mtext(Kl, 1, line=-2, adj = 0.9)
}
