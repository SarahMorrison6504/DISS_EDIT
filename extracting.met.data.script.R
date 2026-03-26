# code used for extracting nc. files to csv.
# using tutorial from for Jan 2016 hurs values- https://pjbartlein.github.io/REarthSysSci/netcdf_terra.html-----
# set working directory to where the downloaded chess-scape files are downloaded
setwd()

# load libraries
library(ncdf4)
library(terra)
library(dplyr)
library(sf)
library(raster)


# read netCDF file
r <- rast('hurs/chess-scape_rcp45_01_hurs_uk_1km_daily_20160101-20160130.nc')

# make function for extracting hurs values at loch leven from nc. files
# extract hurs values
hurs <- rast('hurs/chess-scape_rcp45_01_hurs_uk_1km_daily_20160101-20160130.nc', subds = 'hurs')

# select coordinates to extract from (Loch Leven)
points <- data.frame(
  lon = -3.37783,
  lat = 56.18951
)

# get hurs values from Loch Leven coordinates
values <- extract(hurs, points)

# get dataframe with just hurs values for each days
out <- data.frame(
  time = time(r),
  values = as.numeric(values[1,-1])
)
# make into csv for month of january for hurs
write.csv(out, 'location_timeseries.csv', row.names = FALSE)

# make into function for multiple files (hurs)----
extract_hurs <- function(nc_file){
  hurs <- rast(nc_file, subds = 'hurs')
  
  # set loch leven coordinates
  points <- data.frame(
    lon = -3.37783,
    lat = 56.18951
  )
  
  values <- extract(hurs, points)
  
  out <- data.frame(
    file = basename(nc_file),
    time = time(hurs),
    hurs = as.numeric(values[1,-1])
  )
  
  out
  
}

files <- c('hurs/chess-scape_rcp45_01_hurs_uk_1km_daily_20160101-20160130.nc',
                    'hurs/chess-scape_rcp45_01_hurs_uk_1km_daily_20160201-20160230.nc',
                    'hurs/chess-scape_rcp45_01_hurs_uk_1km_daily_20160301-20160330.nc',
                    'hurs/chess-scape_rcp45_01_hurs_uk_1km_daily_20160401-20160430.nc',
                    'hurs/chess-scape_rcp45_01_hurs_uk_1km_daily_20160501-20160530.nc',
                    'hurs/chess-scape_rcp45_01_hurs_uk_1km_daily_20160601-20160630.nc',
                    'hurs/chess-scape_rcp45_01_hurs_uk_1km_daily_20160701-20160730.nc',
                    'hurs/chess-scape_rcp45_01_hurs_uk_1km_daily_20160801-20160830.nc',
                    'hurs/chess-scape_rcp45_01_hurs_uk_1km_daily_20160901-20160930.nc',
                    'hurs/chess-scape_rcp45_01_hurs_uk_1km_daily_20161001-20161030.nc',
                    'hurs/chess-scape_rcp45_01_hurs_uk_1km_daily_20161101-20161130.nc',
                    'hurs/chess-scape_rcp45_01_hurs_uk_1km_daily_20161201-20161230.nc')

all_hurs <- do.call(rbind, lapply(files, extract_hurs))

# write csv
write.csv(all_hurs, 'le (hurven_hurs.csv', row.names = FALSE )


# apply to all other variables (pr)----
extract_pr <- function(nc_file){
  pr <- rast(nc_file, subds = 'pr')
  
  # set loch leven coordinates
  points <- data.frame(
    lon = -3.37783,
    lat = 56.18951
  )
  
  values <- extract(pr, points)
  
  out <- data.frame(
    file = basename(nc_file),
    time = time(pr),
    pr = as.numeric(values[1,-1])
  )
  
  out
  
}

files <- c('pr/chess-scape_rcp45_01_pr_uk_1km_daily_20160101-20160130.nc',
           'pr/chess-scape_rcp45_01_pr_uk_1km_daily_20160201-20160230.nc',
           'pr/chess-scape_rcp45_01_pr_uk_1km_daily_20160301-20160330.nc',
           'pr/chess-scape_rcp45_01_pr_uk_1km_daily_20160401-20160430.nc',
           'pr/chess-scape_rcp45_01_pr_uk_1km_daily_20160501-20160530.nc',
           'pr/chess-scape_rcp45_01_pr_uk_1km_daily_20160601-20160630.nc',
           'pr/chess-scape_rcp45_01_pr_uk_1km_daily_20160701-20160730.nc',
           'pr/chess-scape_rcp45_01_pr_uk_1km_daily_20160801-20160830.nc',
           'pr/chess-scape_rcp45_01_pr_uk_1km_daily_20160901-20160930.nc',
           'pr/chess-scape_rcp45_01_pr_uk_1km_daily_20161001-20161030.nc',
           'pr/chess-scape_rcp45_01_pr_uk_1km_daily_20161101-20161130.nc',
           'pr/chess-scape_rcp45_01_pr_uk_1km_daily_20161201-20161230.nc')

all_pr <- do.call(rbind, lapply(files, extract_pr))

# write csv
write.csv(all_pr, 'leven_pr.csv', row.names = FALSE )


# (rlds) - LW radiation ----
extract_rlds <- function(nc_file){
  rlds <- rast(nc_file, subds = 'rlds')
  
  # set loch leven coordinates
  points <- data.frame(
    lon = -3.37783,
    lat = 56.18951
  )
  
  values <- extract(rlds, points)
  
  out <- data.frame(
    file = basename(nc_file),
    time = time(rlds),
    rlds = as.numeric(values[1,-1])
  )
  
  out
  
}

files <- c('rlds/chess-scape_rcp45_01_rlds_uk_1km_daily_20160101-20160130.nc',
           'rlds/chess-scape_rcp45_01_rlds_uk_1km_daily_20160201-20160230.nc',
           'rlds/chess-scape_rcp45_01_rlds_uk_1km_daily_20160301-20160330.nc',
           'rlds/chess-scape_rcp45_01_rlds_uk_1km_daily_20160401-20160430.nc',
           'rlds/chess-scape_rcp45_01_rlds_uk_1km_daily_20160501-20160530.nc',
           'rlds/chess-scape_rcp45_01_rlds_uk_1km_daily_20160601-20160630.nc',
           'rlds/chess-scape_rcp45_01_rlds_uk_1km_daily_20160701-20160730.nc',
           'rlds/chess-scape_rcp45_01_rlds_uk_1km_daily_20160801-20160830.nc',
           'rlds/chess-scape_rcp45_01_rlds_uk_1km_daily_20160901-20160930.nc',
           'rlds/chess-scape_rcp45_01_rlds_uk_1km_daily_20161001-20161030.nc',
           'rlds/chess-scape_rcp45_01_rlds_uk_1km_daily_20161101-20161130.nc',
           'rlds/chess-scape_rcp45_01_rlds_uk_1km_daily_20161201-20161230.nc')

all_rlds <- do.call(rbind, lapply(files, extract_rlds))

# write csv
write.csv(all_rlds, 'leven_rlds.csv', row.names = FALSE )


# (rsds) SW radiation----
extract_rsds <- function(nc_file){
  rsds <- rast(nc_file, subds = 'rsds')
  
  # set loch leven coordinates
  points <- data.frame(
    lon = -3.37783,
    lat = 56.18951
  )
  
  values <- extract(rsds, points)
  
  out <- data.frame(
    file = basename(nc_file),
    time = time(rsds),
    rsds = as.numeric(values[1,-1])
  )
  
  out
  
}

files <- c('rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160101-20160130.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160201-20160230.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160301-20160330.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160401-20160430.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160501-20160530.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160601-20160630.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160701-20160730.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160801-20160830.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160901-20160930.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20161001-20161030.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20161101-20161130.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20161201-20161230.nc')

all_rsds <- do.call(rbind, lapply(files, extract_rsds))

# write csv
write.csv(all_rsds, 'leven_rsds.csv', row.names = FALSE )

# (sfcWind) surface wind----
extract_rsds <- function(nc_file){
  rsds <- rast(nc_file, subds = 'rsds')
  
  # set loch leven coordinates
  points <- data.frame(
    lon = -3.37783,
    lat = 56.18951
  )
  
  values <- extract(rsds, points)
  
  out <- data.frame(
    file = basename(nc_file),
    time = time(rsds),
    rsds = as.numeric(values[1,-1])
  )
  
  out
  
}

files <- c('rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160101-20160130.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160201-20160230.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160301-20160330.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160401-20160430.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160501-20160530.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160601-20160630.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160701-20160730.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160801-20160830.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20160901-20160930.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20161001-20161030.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20161101-20161130.nc',
           'rsds/chess-scape_rcp45_01_rsds_uk_1km_daily_20161201-20161230.nc')

all_rsds <- do.call(rbind, lapply(files, extract_rsds))

# write csv
write.csv(all_rsds, 'leven_rsds.csv', row.names = FALSE )

## (tas) near surface air temp----
extract_tas <- function(nc_file){
  tas <- rast(nc_file, subds = 'tas')
  
  # set loch leven coordinates
  points <- data.frame(
    lon = -3.37783,
    lat = 56.18951
  )
  
  values <- extract(tas, points)
  
  out <- data.frame(
    file = basename(nc_file),
    time = time(tas),
    tas = as.numeric(values[1,-1])
  )
  
  out
  
}

files <- c('tas/chess-scape_rcp45_01_tas_uk_1km_daily_20160101-20160130.nc',
           'tas/chess-scape_rcp45_01_tas_uk_1km_daily_20160201-20160230.nc',
           'tas/chess-scape_rcp45_01_tas_uk_1km_daily_20160301-20160330.nc',
           'tas/chess-scape_rcp45_01_tas_uk_1km_daily_20160401-20160430.nc',
           'tas/chess-scape_rcp45_01_tas_uk_1km_daily_20160501-20160530.nc',
           'tas/chess-scape_rcp45_01_tas_uk_1km_daily_20160601-20160630.nc',
           'tas/chess-scape_rcp45_01_tas_uk_1km_daily_20160701-20160730.nc',
           'tas/chess-scape_rcp45_01_tas_uk_1km_daily_20160801-20160830.nc',
           'tas/chess-scape_rcp45_01_tas_uk_1km_daily_20160901-20160930.nc',
           'tas/chess-scape_rcp45_01_tas_uk_1km_daily_20161001-20161030.nc',
           'tas/chess-scape_rcp45_01_tas_uk_1km_daily_20161101-20161130.nc',
           'tas/chess-scape_rcp45_01_tas_uk_1km_daily_20161201-20161230.nc')

all_tas <- do.call(rbind, lapply(files, extract_tas))

# write csv
write.csv(all_tas, 'leven_tas.csv', row.names = FALSE )



