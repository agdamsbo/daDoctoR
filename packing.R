rm(list = ls(pos=-1))

library(roxygen2, devtools)

setwd("/Users/au301842/daDoctoR/")

source("updatePackageVersion.R")

updatePackageVersion()

devtools::document()

# Inspiration: "https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/"

# Author@R: person("Andreas", "Gammelgaard Damsbo", email = "agdamsbo@pm.me", role = c("cre", "aut"))


# Commit and push
commit_message<-"updated dob_extract_cpr to also support cpr format ddmmyyxxxx"

library(git2r)
library(lubridate)
git2r::commit(all=TRUE, message=paste(commit_message,now()))

system("/usr/bin/git push origin HEAD:refs/heads/master")
