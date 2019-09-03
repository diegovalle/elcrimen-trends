## This program does

## Auto-Install packages
.packs <- c("readr", "dplyr", "zoo", "ggplot2", "mgcv",
            "lubridate", "stringr", "rconapo", "loo", "rstanarm",
            "tidybayes", "jsonlite", "scales", "season", "directlabels",
            "hrbrthemes", "tidyr", "betareg", "bayesplot")
.success <- suppressWarnings(sapply(.packs, require, character.only = TRUE))
if (length(names(.success)[!.success])) {
  install.packages(names(.success)[!.success])
  sapply(names(.success)[!.success], require, character.only = TRUE)
}

options(stringsAsFactors = FALSE)

source("R/national.R")
source("R/states.R")
