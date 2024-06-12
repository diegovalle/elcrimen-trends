## This program does

if (Sys.getenv("CI") == "true") {
  install.packages("hrbrthemes", repos = "https://cinc.rud.is")
}

## Auto-Install packages
.packs <- c("readr", "dplyr", "zoo", "ggplot2", "mgcv",
            "lubridate", "stringr", "loo", "rstanarm",
            "tidybayes", "jsonlite", "scales", "season", "directlabels",
            "hrbrthemes", "tidyr", "betareg", "bayesplot")
.success <- suppressWarnings(sapply(.packs, require, character.only = TRUE))
if (length(names(.success)[!.success])) {
  install.packages(names(.success)[!.success])
  sapply(names(.success)[!.success], require, character.only = TRUE)
}

options(stringsAsFactors = FALSE)

# install roboto condesced
if (Sys.getenv("CI") == "true") {
  hrbrthemes::import_roboto_condensed() 
  hrbrthemes::import_econ_sans()
  hrbrthemes::import_public_sans()
  hrbrthemes::import_plex_sans()
  hrbrthemes::import_titillium_web()
}

print("######################################################")
print("######################################################")
print(paste("number of cores:", parallel::detectCores()))
print("######################################################")
print("######################################################")

source("R/national.R")
source("R/states.R")
