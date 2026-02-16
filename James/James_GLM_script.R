# author: James Watt
# date: 14/01/2026

#This script and the GLM3.nml file has been unified for v3.11 - make sure you have this version!

#require(devtools)
#devtools::install_github("robertladwig/GLM3r", ref = "v3.1.1")
#install.packages('remotes')
remotes::install_github('usgs-r/glmtools')
#install.packages("rLakeAnalyzer")
#install.packages("tidyverse")



#### clear console  ####
cat("\f")
rm(list = ls())

# will set the WD relative to where ever this script is stored
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load GLMr3
library(GLM3r)

# check out which R version we're currently using should be 3.1.1 if above install worked
glm_version()

# run GLM - sim folder = . just assings the simulation folder to the wd
GLM3r::run_glm(sim_folder = '.', verbose = T)

GLM3r_folder <-getwd()
nc_file <- file.path(GLM3r_folder, 'aed/output/output.nc')
plot_var_nc()
