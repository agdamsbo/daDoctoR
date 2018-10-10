# Install new version

# Remove

remove.packages("daDoctoR")
.rs.restartR()

# Install from GitHub

setwd("/")
devtools::install_github('agdamsbo/daDoctoR')

library(daDoctoR)
