library(roxygen2)
library(devtools)

# Packing for publication

source("/Users/andreas/Documents/GitHub/daDoctoR/updatePackageVersion.R")

setwd("/Users/andreas/Documents/GitHub/daDoctoR")

updatePackageVersion()

document()


# Inspiration: "https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/"


