# trying to calibrate using the glmtools tutorials


## nitrate using tutorial - https://github.com/GLEON/glmtools/blob/main/tests/testthat/test-calibrate_sim.R------

getwd()

# setup calibration with other factors influencing nitrate results
#get_calib_setup <- function(){
 # setup <- data.frame(
  #'pars' = as.character(c('Fsed_nit', 'Ksed_nit', 'Rdenit', 'Kdenit', 'Rnitrif', 'Knitrif')),
  #'lb' = c(-10, 50, 0.1, 0.5, 0.1, 10), # lower boundaries
  #'ub' = c(10, 200, 1, 5, 1, 200), # upper boundaries
  #'x0' = c(-4.5, 100, 0.26, 2.0, 0.3, 78.1) # initial guesses
#)
#return(setup)
#}
rm(list = ls())
library(glmtools)
library(ncdf4)
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed')
sim_folder <- 'C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed'

field_file  <- file.path(sim_folder, 'calibration/nit_obs.csv')
nml_file    <- file.path(sim_folder,'glm3.nml')
driver_file <- file.path(sim_folder, 'met.csv')
period      <- get_calib_periods('glm3.nml', ratio = 1)
output      <- file.path(sim_folder, 'output/output.nc')

var= 'NIT_nit'

res <- calibrate_sim(
  var = var, path = sim_folder, field_file = field_file,
  nml_file = nml_file,
  glmcmd = NULL,
  first.attempt = TRUE, period = period, method = 'CMA-ES',
  scaling = TRUE, verbose = FALSE,
  metric = 'RMSE', plotting = FALSE,
  target.fit = 1.5, target.iter = 5, output = output
)
file.exists(nml_file)

## for PHS_frp-----
rm(list = ls())
library(glmtools)
library(ncdf4)
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed')
sim_folder <- 'C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed'


field_file  <- file.path(sim_folder, 'calibration.data/PHSfrp.obs.csv')
nml_file    <- ('glm3.nml')
driver_file <- file.path(sim_folder, 'met.csv')
period      <- get_calib_periods(nml_file = nml_file, ratio = 1)
output      <- file.path(sim_folder, 'output/output.nc')

nml_file
var= 'PHS_frp'

res <- calibrate_sim(
  var = var, path = sim_folder, field_file = field_file,
  nml_file = nml_file,
  glmcmd = NULL,
  first.attempt = TRUE, period = period, method = 'CMA-ES',
  scaling = TRUE, verbose = FALSE,
  metric = 'RMSE', plotting = TRUE,
  target.fit = 1.5, target.iter = 5, output = output
)


## TEMP----
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed')
sim_folder <- 'C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed'

field_file  <- file.path(sim_folder, 'calibration.data/emp.temp.csv')
nml_file    <- file.path(sim_folder,'glm3.nml')
driver_file <- file.path(sim_folder, 'met.csv')
period      <- get_calib_periods(nml_file = nml_file, ratio = 1)
output      <- file.path(sim_folder, 'output/output.nc')

var= 'temp'

res <- calibrate_sim(
  var = var, path = sim_folder, field_file = field_file,
  nml_file = nml_file,
  glmcmd = NULL,
  first.attempt = TRUE, period = period, method = 'CMA-ES',
  scaling = TRUE, verbose = FALSE,
  metric = 'RMSE', plotting = TRUE,
  target.fit = 1.5, target.iter = 5, output = output
)

