#' Calculate amount of chemical adsorbed/retained by solid (q)
#'
#' This function determines the amount of chemical (q) in solution adsorbed
#' by the solid
#' @param V Volumne of solution in Liters
#' @param Ci Initial concentration of the solution (ppm)
#' @param Cf Final concentration of the solution (ppm)
#' @param m Mass of solid in Kilograms
#' @return Mass of chemical sorbed to solid (mg/kg)
#' @export

SorbedMass <- function(Ci, Cf, V = 0.025, m= 0.001){
    q <- (Ci - Cf)* V/m
    return(q)
}
