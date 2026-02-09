## plotting
## trying with my data-----
#'using https://rdrr.io/github/USGS-R/glmtools/man/plot_var_nc.html code
getwd()
setwd("C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT")
file.exists("output/output.nc")

library(ncdf4)
library(ggplot2)
nc <- nc_open("output/output.nc")
z <- ncvar_get(nc, "z")
z <- z[is.finite(z) & z <= 26]   # only real depths

print(nc)
names(nc$var)
depth <- ncvar_get(nc, "z")  # or maybe "depth" or "z_w"
range(depth, na.rm = TRUE)

nc_close(nc)


## for temp----

p <- plot_var_nc(
  nc_file = "C:/Users/Sarah/OneDrive/Dissertation/GLM_Examples/Examples/release/warmlake/aed2/output/output.nc",
  var_name = "temp",
  fig_path = NULL,
  max_depth = 25,
  interval = 0.5,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0, 25)
)
p + coord_cartesian(ylim = c(25, 0))
## for PHS_frp----
p <- plot_var_nc(
  nc_file = "C:/Users/Sarah/OneDrive/Dissertation/GLM_Examples/Examples/release/warmlake/aed2/output/output.nc",
  var_name = "PHS_frp",
  fig_path = NULL,
  max_depth = 25,
  interval = 0.5,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0, 25)
)
p + coord_cartesian(ylim = c(25, 0))


### for OXY_oxy----
p <- plot_var_nc(
  nc_file = ('C:/Users/Sarah/OneDrive/Dissertation/GLM_Examples/Examples/release/warmlake/aed2/output/output.nc'),
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
  zlim = c(0, 1500 ))
p + coord_cartesian(ylim = c(5 , 0))  # limit to 5m for presentation

## for salt----
p <- plot_var_nc(
  nc_file = ('C:/Users/Sarah/OneDrive/Dissertation/GLM_Examples/Examples/release/warmlake/aed2/output/output.nc'),
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
  nc_file = ('C:/Users/Sarah/OneDrive/Dissertation/GLM_Examples/Examples/release/warmlake/aed2/output/output.nc'),
  var_name = "PHY_cyno",
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
  zlim = c(0, 1))
p + coord_cartesian(ylim = c(5 , 0))  # limit to 5m for presentation

## dissolved org. phosphorus----
p <- plot_var_nc(
  nc_file = ('C:/Users/Sarah/OneDrive/Dissertation/GLM_Examples/Examples/release/warmlake/aed2/output/output.nc'),
  var_name = "OGM_dop",
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
  zlim = c(0, 0.01))
p + coord_cartesian(ylim = c(5 , 0))  # limit to 5m for presentation
getwd()
## ammonium----
p <- plot_var_nc(
  nc_file = ('C:/Users/Sarah/OneDrive/Dissertation/GLM_Examples/Examples/release/warmlake/aed2/output/output.nc'),
  var_name = "NIT_amm",
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
  zlim = NULL)
p + coord_cartesian(ylim = c(5 , 0))

## DOC----
p <- plot_var_nc(
  nc_file = ('C:/Users/Sarah/OneDrive/Dissertation/GLM_Examples/Examples/release/warmlake/aed2/output/output.nc'),
  var_name = "OGM_doc",
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
  zlim = NULL)
p + coord_cartesian(ylim = c(5 , 0))

## reactive silica----
p <- plot_var_nc(
  nc_file = ('C:/Users/Sarah/OneDrive/Dissertation/GLM_Examples/Examples/release/warmlake/aed2/output/output.nc'),
  var_name = "SIL_rsi",
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
  zlim = c(0, 0.5))
p + coord_cartesian(ylim = c(5 , 0))

## checking wq
# Check one of your WQ files
wq_data <- read.csv("output/WQ_0.csv")
head(wq_data, 20)
tail(wq_data, 20)

# Check if values are changing over time
summary(wq_data$NIT_amm)  # or whatever the ammonium column is called

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



