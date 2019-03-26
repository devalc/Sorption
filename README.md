# Sorption

R package for predicting sorption processes using empirical models. Currently 
contains funcitons for the Langmuir and Freundlich models.         

## Getting Started

These instructions will get you a copy of the "Sorption" up and running on your 
local machine.

## How to Install

Currently the package is available only through devtools. 

Install devtools by running following command in your R console: 

```{r}
install.packages("devtools")
```

Now you can install the Sorption package from Github using devtools as:

```{r}
devtools::install_github("devalc/Sorption")
```
# Usage

Once installed the package can be loaded to the system using:
```{r}
library(Sorption")
```
This will also load test data vectors which can be accessed using:

```{r}
test_Ceq , test_Qeq 
```

## Example Usage: Parameters

Parameters for the frequnlich isotherm can be predicted using:
```{r}
FreundlichParameters(test_Ceq, test_Qeq, "test.csv")
```
## Example Usage: Plots

Frequnlich isotherm can be plotted using:
```{r}
FreundlichPlot(test_Ceq, test_Qeq, 0.,0.8,0.,0.9)
```
