## script for running general lake model (baseline code)

# install required packages

install.packages("devtools")
devtools::install_github("GLEON/GLM3r")
install.packages('remotes')
remotes::install_github('usgs-r/glmtools')
devtools::install_github("GLEON/GLMr")
install.packages('lubridate')
remotes::install_github('usgs-r/glmtools')

# load libraries
library(remotes)
library(devtools)
library(glmtools)
library(lubridate)
library(tidyverse)
library(janitor)
library(rLakeAnalyzer)


#### clear console  ####
cat("\f")
rm(list = ls())

# will set the WD relative to where ever this script is stored
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed')
remove.packages('GLM3r')
# load GLMr3
devtools::install_github("robertladwig/GLM3r", ref = "v3.1.1")
library(GLM3r)

# check out which R version we're currently using should be 3.1.1 if above install worked
glm_version()

# run GLM
GLM3r::run_glm(sim_folder = 'C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed', verbose = T)
getwd()
file.exists('output/log.txt')

rm(list = ls())
