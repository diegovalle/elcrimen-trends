## This program does

if (Sys.getenv("CI") == "true") {
  print(packageVersion("dplyr"))
  # tidybayes need dplyr >= 0.8.0
  install.packages(c("dplyr", "ggplot2", "rstanarm"))
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

# Check if dplyr was updated
if (packageVersion("dplyr") < "0.8.0")
  quit(status = 1)

# install roboto condesced
if (Sys.getenv("CI") == "true") {
  hrbrthemes::import_roboto_condensed() 
}

source("R/national.R")
source("R/states.R")
