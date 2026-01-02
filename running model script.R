# install.packages("devtools")
devtools::install_github("GLEON/GLM3r")
install.packages('remotes')
remotes::install_github('usgs-r/glmtools')
# install.packages("devtools")
devtools::install_github("GLEON/GLMr")

library(GLM3r)
library(devtools)
library(glmtools)
library(GLMr)
library(lubridate)

nml_template_path <- function(){
  return(system.file('GLM/glm3.nml', package=packageName()))
  
}


sim_folder <- 'C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT'
list.files()

Sys.setenv(GLM_PATH = file.path(sim_folder, 'glm.exe'))

run_glm <- function(sim_folder = '.', verbose=TRUE, system.args=character()) {
  
  # verbose true = print output into R console
  
  # Check for glm2.nml
  if(!'glm3.nml' %in% list.files(sim_folder)){
    stop('You must have a valid glm3.nml file in your sim_folder: ', sim_folder)
  }
  
  # Windows-specific run
  if(.Platform$pkgType == "win.binary"){
    return(run_glm3.0_Win(sim_folder, verbose, system.args))
  }
}


# Internal function to call system2 for GLM
glm.systemcall <- function(sim_folder, glm_path, verbose, system.args) {
  
  if(nchar(Sys.getenv("GLM_PATH")) > 0){
    glm_path <- Sys.getenv("GLM_PATH")
    warning(paste0(
      "Custom path to GLM executable set via 'GLM_PATH' environment variable as: ", 
      glm_path))
  }
  
  origin <- getwd()
  setwd(sim_folder)
  
  
  tryCatch({
    if (is.null(verbose)){
      out <- system2(glm_path, wait = TRUE, stdout = TRUE, stderr = NULL, args = system.args)
    } else if (verbose) {
      out <- system2(glm_path, wait = TRUE, stdout = "", stderr = "", args = system.args)
    } else {
      out <- system2(glm_path, wait = TRUE, stdout = NULL, stderr = NULL, args = system.args)
    }
    return(out)
  }, error = function(err) {
    stop(paste("GLM_ERROR:", err$message))
    setwd(origin)
  })
}

# GLM runner
run_glm3.0_Win <- function(sim_folder, verbose, system.args){
  
  # Use glm.exe from simulation folder
  glm_path <- file.path(sim_folder, "glm.exe")
  if(!file.exists(glm_path)){
    stop("GLM executable not found at: ", glm_path)
  }
  
  glm.systemcall(sim_folder, glm_path, verbose, system.args)
}

# simpler?
sim_folder <- "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT"
nml_file <- file.path(sim_folder, "glm3.nml")

# Run GLM

run_glm(sim_folder)
list.files(sim_folder)

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


