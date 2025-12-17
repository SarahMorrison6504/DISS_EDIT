## plotting
## trying with my data-----
#'using https://rdrr.io/github/USGS-R/glmtools/man/plot_var_nc.html code
getwd()
file.exists("output.nc")

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
