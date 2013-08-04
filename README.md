# [PropCIs](https://github.com/shearer/PropCIs)

## resources
The confidence intervals are mostly described in Alan Agresti's [CDA book](http://eu.wiley.com/WileyCDA/WileyTitle/productCd-0470463635.html).  
How to calculate the intervals in other software is described at the [webpage for CDA](http://www.stat.ufl.edu/~aa/cda/cda.html).

To post feature requests or ask for help, try [the PropCIs Issue Tracker](https://github.com/shearer/PropCIs/issues?page=1&state=open).

## Development

To install the development version of the PropCIs package, it is easiest to use the [devtools](http://cran.r-project.org/web/packages/devtools/index.html) package:

    install.packages("devtools")  # if needed..
    library(devtools)
    install_github("PropCIs", "shearer")
    library(PropCIs)
