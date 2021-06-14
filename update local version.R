# Install new version

# Remove

remove.packages("daDoctoR")
.rs.restartR()

setwd("/Users")

devtools::install_github('agdamsbo/daDoctoR')

library(daDoctoR)


## Safe alternative to devtools-approach

# library(downloader)
# download("https://github.com/agdamsbo/daDoctoR/archive/master.tar.gz", "daDoctoR.tar.gz")
# install.packages("daDoctoR.tar.gz", repos = NULL, type = "source")

# library(daDoctoR)


