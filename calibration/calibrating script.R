## for surface temp, using depth = 0
library(glmtools)
sim_dir = run_example_sim()

Sys.setenv(tz='UTC')
getwd()
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT')

# get surface temperature from model output
model_temp <- get_temp(ncfile, reference = 'surface')
model_temp <- model_temp[, c('DateTime', 'temp_0')]

# get observed surface temp
obs <- read.csv('calibration/emp.temp.csv')
obs$DateTime <- as.POSIXct(obs$DateTime)
# merge data sets to compare

compare <- merge(model_temp, obs, by="DateTime")
# plot
library(ggplot2)

ggplot(compare, aes(x = DateTime)) +
  geom_line(aes(y = temp_0, colour = "Model"), linewidth = 1) +
  geom_line(aes(y = temp, colour = "Observed"), linewidth = 1) +
  labs(x = "Date",
       y = "Surface Temperature (°C)",
       colour = "") +
  scale_colour_manual(values = c("Model" = "blue",
                                 "Observed" = "red")) +
  theme_minimal()

# calculate RMSE
rmse_surf_temp <- sqrt(mean((compare$temp_0 - compare$temp)^2, na.rm = TRUE))
rmse_surf_temp
# 3.057669

## nitrate using tutorial - https://github.com/GLEON/glmtools/blob/main/tests/testthat/test-calibrate_sim.R


calib_setup <- get_calib_setup()
print(calib_setup)
getwd()

sim_folder <- tempdir()
glmtoolsfolder <- system.file('extdata', package = 'glmtools')

file.copy(
  list.files(glmtools_folder,full.names = TRUE), 
  sim_folder, overwrite = TRUE)

field_file  <- file.path(sim_folder, 'calibration/nit_obs.csv')
nml_file    <- file.path(sim_folder, 'glm3.nml')
driver_file <- file.path(sim_folder, 'met.csv')
period      <- get_calib_periods(nml_file = nml_file, ratio = 1)
output      <- file.path(sim_folder, 'output/output.nc')

var= 'NIT_nit'

res <- calibrate_sim(
  var = var, path = sim_folder, field_file = field_file,
  nml_file = nml_file, calib_setup = calib_setup,
  glmcmd = NULL,
  first.attempt = TRUE, period = period, method = 'CMA-ES',
  scaling = TRUE, verbose = FALSE,
  metric = 'RMSE', plotting = FALSE,
  target.fit = 1.5, target.iter = 50, output = output
)
