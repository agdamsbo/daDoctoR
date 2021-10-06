library(roxygen2, devtools)

source("updatePackageVersion.R")

updatePackageVersion()

devtools::document()

# Inspiration: "https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/"

# Author@R: person("Andreas", "Gammelgaard Damsbo", email = "agdamsbo@pm.me", role = c("cre", "aut"))
