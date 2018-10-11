# Install new version

# Remove

remove.packages("daDoctoR")
.rs.restartR()


setwd("/")
devtools::install_github('agdamsbo/daDoctoR')

library(daDoctoR)

