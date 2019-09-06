FROM rocker/verse:latest
MAINTAINER Diego Valle

# From https://hub.docker.com/r/jrnold/rstan/dockerfile

RUN apt-get update \
&& apt-get install -y --no-install-recommends apt-utils ed libnlopt-dev libgdal-dev libudunits2-dev cargo jags libcairo2-dev libxt-dev libgeos-dev \
&& apt-get clean \
&& rm -rf /var/lib/apt/lists/
  
# Install rstan
RUN install2.r --error --deps TRUE \
rstan \
&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Global site-wide config -- neeeded for building packages
RUN mkdir -p $HOME/.R/ \
&& echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -flto -ffat-lto-objects  -Wno-unused-local-typedefs \n" >> $HOME/.R/Makevars

# Config for rstudio user
RUN mkdir -p $HOME/.R/ \
&& echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -flto -ffat-lto-objects  -Wno-unused-local-typedefs -Wno-ignored-attributes -Wno-deprecated-declarations\n" >> $HOME/.R/Makevars \
&& echo "rstan::rstan_options(auto_write = TRUE)\n" >> /home/rstudio/.Rprofile \
&& echo "options(mc.cores = parallel::detectCores())\n" >> /home/rstudio/.Rprofile

# Install rstan
RUN install2.r --error --deps TRUE \
rstan \
loo \
bayesplot \
rstanarm \
rstantools \
shinystan \
ggmcmc \
tidybayes \
readr \
dplyr \
zoo  \
ggplot2 \
mgcv \
lubridate \
stringr  \
loo \
jsonlite \
scales \
directlabels \
tidyr \
betareg \
extrafont \
&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Install Roboto condensed
RUN wget -O /tmp/rc.zip https://fonts.google.com/download?family=Roboto%20Condensed \
&& cd /usr/share/fonts \
&& sudo mkdir googlefonts \
&& cd googlefonts \
&& sudo unzip -d . /tmp/rc.zip \
&& sudo chmod -R --reference=/usr/share/fonts/truetype /usr/share/fonts/googlefonts \
&& sudo fc-cache -fv \
&& rm -rf /tmp/rc.zip \
&& Rscript --slave --no-save --no-restore-history -e "extrafont::font_import(prompt=FALSE)"
