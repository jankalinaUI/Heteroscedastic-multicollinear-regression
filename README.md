# Heteroscedastic multicollinear regression, version 1.0

The software implemented in R presents a model regression analysis of
a complex dataset under both heteroscedasticity and multicollinearity.
While standard tools are used here as well, the main novelty
is the usage of regularized regression quantiles, including tools
for graphical output and variable selection. This very detailed analysis
was tested over a real economic dataset. The analysis without this 
software would require a tedious work to perform variable selection
from the regularized regression quantiles.

Feel free to use or modify the code.

## Requirements

You need to install these packages of R software: glmnet, quantreg, Qtools.

## Usage

* We recommend to perform the analysis in this order, starting with traditional statistical tools and proceeding to regression quantiles and
finally regularized regression quantiles: StandardAnalysis.R, Quantiles.R, RegularizedQuantiles.R.

## Authors
  * Jan Kalina, The Czech Academy of Sciences, Institute of Computer Science
  * Eva Litavcová, University of Prešov
  * Nicole Tobišková, University of Prešov

## Contact

Do not hesitate to contact us (kalina@cs.cas.cz) or write an Issue.

## How to cite

Please consider citing the following:

Kalina J, Vašaničová P, Litavcová E (2019): Regression quantiles under heteroscedasticity and multicollinearity: Analysis of travel and tourism
competitiveness. Ekonomický časopis 67 (1), 69-85.

## Acknowledgement

This work was supported by the Czech Science Foundation grant GA19-05704S.