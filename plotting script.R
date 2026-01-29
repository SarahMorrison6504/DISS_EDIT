## plotting
## trying with my data-----
#'using https://rdrr.io/github/USGS-R/glmtools/man/plot_var_nc.html code
getwd()
setwd("C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT")
file.exists("output/output.nc")

library(ncdf4)
library(ggplot2)
nc <- nc_open("output/output.nc")
names(nc$var)  # check variable names
nc_close(nc)  # close nc file
## for temp----
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

### for OXY_oxy----
p <- plot_var_nc(
  nc_file = ('C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/output/output.nc'),
  var_name = "OXY_oxy",
  fig_path = NULL,
  reference = "surface",
  max_depth = 10,
  legend.title = NULL,
  interval = 0.5,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  plot.title = NULL,
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0, 550 ))
p + coord_cartesian(ylim = c(5 , 0))  # limit to 5m for presentation

## for salt----
p <- plot_var_nc(
  nc_file = ('C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/output/output.nc'),
  var_name = "salt",
  fig_path = NULL,
  reference = "surface",
  max_depth = 10,
  legend.title = NULL,
  interval = 0.5,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  plot.title = NULL,
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0, 0.001 ))
p + coord_cartesian(ylim = c(25 , 0))  # limit to 5m for presentation

## phyto? ----
p <- plot_var_nc(
  nc_file = ('C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/output/output.nc'),
  var_name = "PHY_diat",
  fig_path = NULL,
  reference = "surface",
  max_depth = 10,
  legend.title = NULL,
  interval = 0.5,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  plot.title = NULL,
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0, 0.2))
p + coord_cartesian(ylim = c(5 , 0))  # limit to 5m for presentation




## similar method, from https://github.com/GLEON/glmtools/blob/main/R/plot_temp.R----
plot_temp <- function(nc_file='output.nc',fig_path = NULL, reference='surface', legend.title = NULL, 
                     interval = 0.5, text.size = 12, show.legend = TRUE, 
                     legend.position = 'right', plot.title = NULL){
  
  .Deprecated('plot_var',msg = 'Deprecated. Use `plot_var`, where default is var_name = `temp`')
  
  plot_var_nc(nc_file = nc_file, var_name = 'temp', fig_path = fig_path, reference = reference, legend.title = legend.title, 
              interval = interval, text.size = text.size, show.legend = show.legend, 
              legend.position = legend.position, plot.title = plot.title)
  
}
plot_temp(nc_file = 'output/output.nc')
