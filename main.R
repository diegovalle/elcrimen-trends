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

number_of_samples <- 1000
state_years <- 6

print("######################################################")
print("######################################################")
print(paste("number of cores:", parallel::detectCores()))
print(paste("Number of years for state data:", state_years))
print(paste("N for state simulations samples:", number_of_samples))
print("######################################################")
print("######################################################")

source("R/national.R")
source("R/states.R")
