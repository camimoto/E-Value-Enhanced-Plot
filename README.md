# E-Value-Plot

E-value is related to the evidence for causality in observational studies that are potentially subject to confounding. More information can be found here:

https://cran.r-project.org/web/packages/EValue/EValue.pdf

https://cran.r-project.org/web/packages/EValue/vignettes/unmeasured-confounding.html

In this package, function bias_plot() plots the bias factor required to explain away a provided relative risk. I added the lower bound of CI for e-value curve and the covariates on the same space.

To get x coordinate of a covariate X, we fit a logistics regression of Treatment vs. X, then obtain the odds ratio for X and convert to relative risk.

To get y coordinate of a covariate X, we fit a logistics regression of Response vs. Y, then obtain the odds ratio for X and convert to relative risk.

See evalue.R for the exact computation.
 
