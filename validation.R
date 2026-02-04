## validation using example - https://aemon-j.github.io/LakeEnsemblR/
# downloading LakeEnsemblR


# install.packages("remotes")----
remotes::install_github("GLEON/rLakeAnalyzer")

remotes::install_github("aemon-j/GLM3r", ref = "v3.1.1")
remotes::install_github("USGS-R/glmtools", ref = "ggplot_overhaul")
remotes::install_github("aemon-j/FLakeR", ref = "inflow")
remotes::install_github("aemon-j/GOTMr")
remotes::install_github("aemon-j/gotmtools")
remotes::install_github("aemon-j/SimstratR")
remotes::install_github("aemon-j/MyLakeR")
remotes::install_github("aemon-j/LakeEnsemblR")


# load packages ----
library(rLakeAnalyzer)
library(GLM3r)
library(glmtools)
library(FLakeR)
library(GOTMr)
library(gotmtools)
library(SimstratR)
library(MyLakeR)
library(LakeEnsemnlR)
library(gotmtools)
library(ggplot2)
# example model run----

# Load LakeEnsemblR


# Copy template folder
template_folder <- system.file("extdata/feeagh", package= "LakeEnsemblR")
dir.create("example") # Create example folder
file.copy(from = template_folder, to = "example", recursive = TRUE)
setwd("example/feeagh") # Change working directory to example folder


# Set config file & models
config_file <- "LakeEnsemblR.yaml"
model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")

# Example run
# 1. Export settings - creates directories with all model setups and exports settings from the LER configuration file
export_config(config_file = config_file, model = model)

# 2. Run ensemble lake models
run_ensemble(config_file = config_file, model = model)


## Plot model output using gotmtools/ggplot2
# Extract names of all the variables in netCDF
ncdf <- "output/ensemble_output.nc"
vars <- gotmtools::list_vars(ncdf)
vars # Print variables

p1 <- plot_heatmap(ncdf)

##MY MODEL----
## subsetting data for calibration and validation----

inflow1 <- read.csv("bcs/inflow.csv", stringsAsFactors = FALSE)
inflow2 <- read.csv("bcs/inflow2.csv", stringsAsFactors = FALSE)
inflow3 <- read.csv("bcs/inflow3.csv", stringsAsFactors = FALSE)

met <- read.csv("bcs/meteo_fl.csv", stringsAsFactors = FALSE)
outflow <- read.csv("bcs/outflow1.csv", stringsAsFactors = FALSE)

to_time <- function(df) {
  df$time <- as.POSIXct(df$time, tz = "UTC")
  df$year <- as.integer(format(df$time, "%Y"))
  df
}

inflow1 <- to_time(inflow1)
inflow2 <- to_time(inflow2)
inflow3 <- to_time(inflow3)
met     <- to_time(met)
outflow <- to_time(outflow)

#split to calibration and validation periods
inflow1_cal <- subset(inflow1, year %in% 2016:2018)
inflow2_cal <- subset(inflow2, year %in% 2016:2018)
inflow3_cal <- subset(inflow3, year %in% 2016:2018)

met_cal     <- subset(met, year %in% 2016:2018)
outflow_cal <- subset(outflow, year %in% 2016:2018)

# validation
inflow1_val <- subset(inflow1, year == 2019)
inflow2_val <- subset(inflow2, year == 2019)
inflow3_val <- subset(inflow3, year == 2019)

met_val     <- subset(met, year == 2019)
outflow_val <- subset(outflow, year == 2019)

# check time alignment
stopifnot(
  all(inflow1_cal$time == met_cal$time),
  all(inflow1_cal$time == outflow_cal$time)
)

stopifnot(
  all(inflow1_val$time == met_val$time),
  all(inflow1_val$time == outflow_val$time)
)
