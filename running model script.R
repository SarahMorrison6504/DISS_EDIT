install.packages("devtools")

devtools::install_github("GLEON/GLM3r")
install.packages('remotes')
remotes::install_github('usgs-r/glmtools')
devtools::install_github("GLEON/GLMr")
install.packages('lubridate')

library(GLM3r)
library(devtools)
library(glmtools)
library(GLMr)
library(lubridate)
library(tidyr)
library(janitor)

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
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed')
remove.packages('GLM3r')
# load GLMr3
devtools::install_github("robertladwig/GLM3r", ref = "v3.1.1")
library(GLM3r)

# check out which R version we're currently using should be 3.1.1 if above install worked
glm_version()

# run GLM - sim folder = . just assings the simulation folder to the wd
GLM3r::run_glm(sim_folder = '.', verbose = T)

GLM3r_folder <-getwd()
nc_file <- file.path(GLM3r_folder, 'aed/output/output.nc')
plot_var_nc()
# Run GLM
run_glm(sim_folder)
getwd()
file.exists('output/log.txt')

rm(list = ls())
# Source - https://stackoverflow.com/q
# Posted by fjd, modified by community. See post 'Timeline' for change history
# Retrieved 2025-12-08, License - CC BY-SA 3.0

# plotting the csv files----
library(ggplot2)
library(dplyr)
lake <- ('WQ_0') # make dataframe WQ_0 an object
class(lake) # classify what type of data is in lake df
str(lake)
lake <- read.csv("output/WQ_0.csv", stringsAsFactors = FALSE)
str(lake)
lake$time
lake$date <- as.Date(substr(lake$time, 1, 10)) # remove time from data frame


ggplot(lake, aes(x = date, y = temp)) +
  geom_line() +
  theme_bw()

summary(lake)


## trying with my data-----
#'using https://rdrr.io/github/USGS-R/glmtools/man/plot_var_nc.html code
getwd()
file.exists("output.nc")

## plotting temp heatmap
setwd('M:Dissertation-main/GLM/Dissertation-main/GLM/output/output.nc')
install.packages('ncdf4')
library(ncdf4)
library(ggplot2)
nc <- nc_open("output/output.nc")
names(nc$var)  # check variable names
nc_close(nc)  # close nc file

p <- plot_var_nc(
  nc_file = ('C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/output/output.nc'),
  var_name = "temp",
  fig_path = NULL,
  reference = "surface",
  max_depth = 25,
  legend.title = NULL,
  interval = 0.5,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  plot.title = NULL,
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0, 25))
p + coord_cartesian(ylim = c(25, 0))  # limit to 25m for presentation


