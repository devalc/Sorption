% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FreundlichPlot.R
\name{FreundlichPlot}
\alias{FreundlichPlot}
\title{Plot the Freundlich model fit to the experimental data}
\usage{
FreundlichPlot(Ce, Qe, cor_lab_x, cor_lab_y, eq_lab_x, eq_lab_y)
}
\arguments{
\item{Ce}{equilibrium solution concentrations in mg/l}

\item{Qe}{retention by solid (adsorption) mg/kg}

\item{cor_lab_x, cor_lab_y}{location on the plot to place pearson r and p-value}

\item{eq_lab_x, eq_lab_y}{location on the plot to place equation of the fitted line}
}
\value{
plot
}
\description{
This function plots the Freundlich isotherm model fit
}
