library(roxygen2)
library(devtools)

# Packing for publication

setwd("/Users/andreas/Documents/GitHub/daDoctoR"); document()


setwd(".."); install("daDoctoR")

# Install from GitHub

setwd("/"); devtools::install_github('agdamsbo/daDoctoR'); library(daDoctoR)

"https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/"

# Fixes

remove.packages("daDoctoR"); .rs.restartR()

