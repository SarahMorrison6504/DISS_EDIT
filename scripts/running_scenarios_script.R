## running scenarios
# scenarios will be ran under 0%, 10%, 40% and 90% coverages
# script also includes the plotting of scenarios
# running scenarios

# load libraries
library(GLM3r)
library(devtools)
library(glmtools)
library(lubridate)
library(tidyverse)
library(janitor)
library(ncdf4) 
library(patchwork)
library(ggplot2)

#### clear console  ####
cat("\f")
rm(list = ls())
library(devtools)

## ensure correct version of GLM is installed
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios')
remove.packages('GLM3r')
# load GLMr3
devtools::install_github("robertladwig/GLM3r", ref = "v3.1.1")
library(GLM3r)
glm_version()

## change glm3.nml to the different met files for each scenario
GLM3r::run_glm(sim_folder = 'C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed', verbose = T)

setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed')
## plotting temp for each scenario----
base_temp <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/baseline_output/output_baseline.nc",
  var_name = "temp",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(-1,15))


# Add reversed depth scale, title, and adjust title size
(base_temp <- base_temp +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("A") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      legend.position = 'none'
    ))


# Save high-resolution PNG
ggsave(
  filename = "temp_baseline.png",
  path = 'scenarios/scenario_outputs/baseline_output/',
  plot = base_temp,
  width = 8,
  height = 6,
  dpi = 600
)

## 10% coverage
temp_10 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_10_output/output_10.nc",
  var_name = "temp",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(-1,15))
# Add reversed depth scale, title, and adjust title size
(temp_10 <- temp_10 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("B") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x= element_blank(),
      axis.line = element_blank(),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "temp_scenario10.png",
  path = 'scenarios/scenario_outputs/scenario_10_output',
  plot = temp_10,
  width = 8,
  height = 6,
  dpi = 600
)

# 40% coverage
temp_40 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_40_output/output_40.nc",
  var_name = "temp",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(-1,15))

# Add reversed depth scale, title, and adjust title size
(temp_40 <- temp_40 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("C") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "temp_scenario40.png",
  path = 'scenarios/scenario_outputs/scenario_40_output/',
  plot = temp_40,
  width = 8,
  height = 6,
  dpi = 600
)
# 90% coverage
temp_90 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_90_output/output_90.nc",
  var_name = "temp",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(-1,15)
)

# Add  depth scale, title, and adjust title size
(temp_90 <- temp_90 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("D") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "temp_scenario90.png",
  path = 'scenarios/scenario_outputs/scenario_90_output/',
  plot = temp_90,
  width = 8,
  height = 6,
  dpi = 600
)
base_temp <- base_temp + theme(axis.title = element_blank())
temp_10   <- temp_10   + theme(axis.title = element_blank())
temp_40   <- temp_40   + theme(axis.title = element_blank())
temp_90   <- temp_90   + theme(axis.title = element_blank())

temp_panel <- ((base_temp | temp_10) /
                 (temp_40 | temp_90)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

temp_panel <- temp_panel +
  plot_annotation(
    theme = theme(
      plot.margin = margin(t = 5, r = 5, b = 25, l = 25), # space for axis titles
      plot.title = element_blank(),
      axis.text.x = element_text(size = 8)
    )
  )

temp_panel <- temp_panel &
  scale_x_date(
    labels = function(x) substr(format(x, '%b'), 1, 1),
    date_breaks = '1 month'
  )


(final_plot <- ggdraw(temp_panel) +
    draw_label("Date", x = 0.425, y = 0.02, vjust = 0) +
    draw_label("Depth (m)", x = 0.02, y = 0.5, angle = 90, vjust = 1))


ggsave(
  filename = "temp_scenarios_panel.png",
  path = 'scenarios/scenario_outputs/panel_bathymetric_scenario_plots/',
  plot = final_plot,
  width = 9.7,
  height = 6.61,
  dpi = 600
)


# max values
#compile data
no <- get_var('baseline_output/output_baseline.nc', var_name = 'temp', reference = 'surface' )
ten <- get_var('scenario_10_output/output_10.nc', var_name = 'temp', reference = 'surface' )
forty <- get_var('scenario_40_output/output_40.nc', var_name = 'temp', reference = 'surface' )
ninety <- get_var('scenario_90_output/output_90.nc', var_name = 'temp', reference = 'surface' )

datasets <- list(
  no = no,
  ten = ten,
  forty = forty,
  ninety = ninety
)

# rename layers
rename_temp <- function(df) {
  colnames(df) <- sapply(colnames(df), function(x){
    if(x == 'DateTime')return('DateTime')
    if(x== 'temp_0')return('Surface')
    if(startsWith(x, 'temp_2.1'))return('Middle')
    if(startsWith(x, 'temp_4.7'))return('Bottom')
    return(x)
  }) 
  return(df)
}

datasets <- lapply(datasets, rename_temp)


# add coverage
coverage_values <- c(no = 0, ten = 10, forty = 40, ninety = 90)
datasets <- Map(function(df, cov){
  df$coverage <- cov
  return(df)
}, datasets, coverage_values)

# combine data
all_data <- bind_rows(datasets)

## add seasons and clean
all_temp <- all_data %>%
  select(DateTime, coverage, Surface, Middle, Bottom) %>%
  mutate(
    scenario = c('0'= 'A', '10' = 'B', '40' = 'C', '90' = 'D')[as.character(coverage)],
    date = as.POSIXct(DateTime, tz = 'UTC'),
    month = lubridate:: month(date),
    season = case_when(
      month %in% c(12, 1, 2) ~ 'Winter',
      month %in% c(3,4,5) ~ 'Spring',
      month %in% c(6,7,8) ~ 'Summer',
      TRUE~ 'Autumn'
    )
  )
# remove initial period(jan-mid feb at beginnign of simulation)

all_temp_filtered <- all_temp%>%
  filter(date>= as.Date('2016-02-15'))

all_temp_long <- all_temp_filtered %>%
  pivot_longer(
    cols = c(Surface, Middle, Bottom),
    names_to = 'depth',
    values_to = 'Temperature'
  )
# extract seasonal max
(temp_seasonal_max <- all_temp_long %>%
    group_by(scenario, season, depth) %>%
    slice_max(order_by = Temperature, n = 1, with_ties = FALSE)%>%
    ungroup()%>%
    select(scenario, season, depth, date, Temperature))


# filter just summer for easier reading
summer_data <- temp_seasonal_max %>%
  filter(season == 'Summer')

# make into a table
(summer_table <- temp_seasonal_max %>%
    filter(season == 'Summer') %>%
    select(scenario, depth, Temperature)%>%
    pivot_wider(
      names_from = scenario,
      values_from = Temperature
    )%>%
    arrange(depth))

# all other seasons
spring_data <- temp_seasonal_max %>%
  filter(season == 'Spring')

# make into a table
(spring_table <- temp_seasonal_max %>%
    filter(season == 'Spring') %>%
    select(scenario, depth, Temperature)%>%
    pivot_wider(
      names_from = scenario,
      values_from = Temperature
    )%>%
    arrange(depth))

autumn_data <- temp_seasonal_max %>%
  filter(season == 'Autumn')

# make into a table
(autumn_table <- temp_seasonal_max %>%
    filter(season == 'Autumn') %>%
    select(scenario, depth, Temperature)%>%
    pivot_wider(
      names_from = scenario,
      values_from = Temperature
    )%>%
    arrange(depth))

winter_data <- temp_seasonal_max %>%
  filter(season == 'Winter')

# make into a table
(winter_table <- temp_seasonal_max %>%
    filter(season == 'Winter') %>%
    select(scenario, depth, Temperature)%>%
    pivot_wider(
      names_from = scenario,
      values_from = Temperature
    )%>%
    arrange(depth))
## plotting do for each scenario----
# baseline
getwd()
base_do <- plot_var_nc(
  nc_file = "baseline_output/output_baseline.nc",
  var_name = "OXY_oxy",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0,550))


# Add reversed depth scale, title, and adjust title size
(base_do <- base_do +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("A") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    legend.position = 'none'
  ))



# Save high-resolution PNG
ggsave(
  filename = "do_baseline.png",
  path = 'scenarios/scenario_outputs/baseline_output/',
  plot = base_do,
  width = 8,
  height = 6,
  dpi = 600
)

## 10% coverage
do_10 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_10_output/output_10.nc",
  var_name = "OXY_oxy",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0,550))
# Add reversed depth scale, title, and adjust title size
(do_10 <- do_10 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("B") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x= element_blank(),
    axis.line = element_blank(),
    legend.position = 'none'
  ))

# Save high-resolution PNG
ggsave(
  filename = "do_scenario10.png",
  path = 'scenarios/scenario_outputs/scenario_10_output',
  plot = do_10,
  width = 8,
  height = 6,
  dpi = 600
)
# 20% coverage
do_20 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_20_output/output_20.nc",
  var_name = "OXY_oxy",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0,550))

# Add reversed depth scale, title, and adjust title size
do_20 <- do_20 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 20%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "do_scenario20.png",
  plot = do_20,
  width = 8,
  height = 6,
  dpi = 600
)
# 40% coverage
do_40 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_40_output/output_40.nc",
  var_name = "OXY_oxy",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0,550))

# Add reversed depth scale, title, and adjust title size
(do_40 <- do_40 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("C") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = 'none'
  ))

# Save high-resolution PNG
ggsave(
  filename = "do_scenario40.png",
  path = 'scenarios/scenario_outputs/scenario_40_output/',
  plot = do_40,
  width = 8,
  height = 6,
  dpi = 600
)
# 90% coverage
do_90 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_90_output/output_90.nc",
  var_name = "OXY_oxy",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0,550)
)

# Add  depth scale, title, and adjust title size
(do_90 <- do_90 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("D") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = 'none'
  ))

# Save high-resolution PNG
ggsave(
  filename = "do_scenario90.png",
  path = 'scenarios/scenario_outputs/scenario_90_output/',
  plot = do_90,
  width = 8,
  height = 6,
  dpi = 600
)
base_do <- base_do + theme(axis.title = element_blank())
do_10   <- do_10   + theme(axis.title = element_blank())
do_40   <- do_40   + theme(axis.title = element_blank())
do_90   <- do_90   + theme(axis.title = element_blank())

do_panel <- ((base_do | do_10) /
  (do_40 | do_90)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

do_panel <- do_panel +
  plot_annotation(
    theme = theme(
      plot.margin = margin(t = 5, r = 5, b = 25, l = 25), # space for axis titles
      plot.title = element_blank(),
      axis.text.x = element_text(size = 8)
    )
  )

do_panel <- do_panel &
  scale_x_date(
    labels = function(x) substr(format(x, '%b'), 1, 1),
    date_breaks = '1 month'
  )


(final_plot <- ggdraw(do_panel) +
  draw_label("Date", x = 0.425, y = 0.02, vjust = 0) +
  draw_label("Depth (m)", x = 0.02, y = 0.5, angle = 90, vjust = 1))


ggsave(
  filename = "do_scenarios_panel.png",
  path = 'scenarios/scenario_outputs/panel_bathymetric_scenario_plots/',
  plot = final_plot,
  width = 9.7,
  height = 6.61,
  dpi = 600
)


# Save combined panel
image_write(panel_do, path = "do_scenarios_panel.png")

# caluculate bottom do min concentrations
# compile data
no <- get_var('baseline_output/output_baseline.nc', var_name = 'OXY_oxy', reference = 'surface' )
ten <- get_var('scenario_10_output/output_10.nc', var_name = 'OXY_oxy', reference = 'surface' )
forty <- get_var('scenario_40_output/output_40.nc', var_name = 'OXY_oxy', reference = 'surface' )
ninety <- get_var('scenario_90_output/output_90.nc', var_name = 'OXY_oxy', reference = 'surface' )

datasets <- list(
  no = no,
  ten = ten,
  forty = forty,
  ninety = ninety
)

# rename bottom layer
rename_do <- function(df) {
  colnames(df) <- sapply(colnames(df), function(x){
    if(x == 'DateTime')return('DateTime')
    if(startsWith(x, 'OXY_oxy_4.7'))return('Bottom')
    return(x)
  }) 
  return(df)
}

datasets <- lapply(datasets, rename_do)


# add coverage
coverage_values <- c(no = 0, ten = 10, forty = 40, ninety = 90)
datasets <- Map(function(df, cov){
  df$coverage <- cov
  return(df)
}, datasets, coverage_values)

# combine data
all_data <- bind_rows(datasets)

## add seasons and clean
all_do <- all_data %>%
  select(DateTime, coverage, Bottom) %>%
  mutate(
    scenario = c('0'= 'A', '10' = 'B', '40' = 'C', '90' = 'D')[as.character(coverage)],
    date = as.POSIXct(DateTime, tz = 'UTC'),
    month = lubridate:: month(date),
    season = case_when(
      month %in% c(12, 1, 2) ~ 'Winter',
      month %in% c(3,4,5) ~ 'Spring',
      month %in% c(6,7,8) ~ 'Summer',
      TRUE~ 'Autumn'
    )
  )
# remove initial period(jan-mid feb at beginnign of simulation)

all_do_filtered <- all_do%>%
  filter(date>= as.Date('2016-02-15'))

# extract seasonal mins
(dp_seasonal_min <- all_do_filtered %>%
  group_by(scenario, season) %>%
  slice_min(order_by = Bottom, n = 1, with_ties = FALSE)%>%
  ungroup()%>%
  select(scenario, season, date, Bottom))
# plotting NO3 for each scenario-----
# baseline
base_nit <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/baseline_output/output_baseline.nc",
  var_name = "NIT_nit",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0,210))


# Add reversed depth scale, title, and adjust title size
(base_nit <- base_nit +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("A") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      legend.position = 'none'
    ))


# Save high-resolution PNG
ggsave(
  filename = "nit_baseline.png",
  path = 'scenarios/scenario_outputs/baseline_output/',
  plot = base_nit,
  width = 8,
  height = 6,
  dpi = 600
)

## 10% coverage
nit_10 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_10_output/output_10.nc",
  var_name = "NIT_nit",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0,210))
# Add reversed depth scale, title, and adjust title size
(nit_10 <- nit_10 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("B") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x= element_blank(),
      axis.line = element_blank(),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "nit_scenario10.png",
  path = 'scenarios/scenario_outputs/scenario_10_output',
  plot = nit_10,
  width = 8,
  height = 6,
  dpi = 600
)

# 40% coverage
nit_40 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_40_output/output_40.nc",
  var_name = "NIT_nit",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0,210))

# Add reversed depth scale, title, and adjust title size
(nit_40 <- nit_40 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("C") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "nit_scenario40.png",
  path = 'scenarios/scenario_outputs/scenario_40_output/',
  plot = nit_40,
  width = 8,
  height = 6,
  dpi = 600
)
# 90% coverage
nit_90 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_90_output/output_90.nc",
  var_name = "NIT_nit",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0,210)
)

# Add  depth scale, title, and adjust title size
(nit_90 <- nit_90 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("D") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "nit_scenario90.png",
  path = 'scenarios/scenario_outputs/scenario_90_output/',
  plot = nit_90,
  width = 8,
  height = 6,
  dpi = 600
)
base_nit <- base_nit + theme(axis.title = element_blank())
nit_10   <- nit_10   + theme(axis.title = element_blank())
nit_40   <- nit_40   + theme(axis.title = element_blank())
nit_90   <- nit_90   + theme(axis.title = element_blank())

nit_panel <- ((base_nit | nit_10) /
               (nit_40 | nit_90)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

nit_panel <- nit_panel +
  plot_annotation(
    theme = theme(
      plot.margin = margin(t = 5, r = 5, b = 25, l = 25), # space for axis titles
      plot.title = element_blank(),
      axis.text.x = element_text(size = 8)
    )
  )

nit_panel <- nit_panel &
  scale_x_date(
    labels = function(x) substr(format(x, '%b'), 1, 1),
    date_breaks = '1 month'
  )


(final_plot <- ggdraw(nit_panel) +
    draw_label("Date", x = 0.425, y = 0.02, vjust = 0) +
    draw_label("Depth (m)", x = 0.02, y = 0.5, angle = 90, vjust = 1))

## max values at depth
no <- get_var('baseline_output/output_baseline.nc', var_name = 'NIT_nit', reference = 'surface' )
ten <- get_var('scenario_10_output/output_10.nc', var_name = 'NIT_nit', reference = 'surface' )
forty <- get_var('scenario_40_output/output_40.nc', var_name = 'NIT_nit', reference = 'surface' )
ninety <- get_var('scenario_90_output/output_90.nc', var_name = 'NIT_nit', reference = 'surface' )

datasets <- list(
  no = no,
  ten = ten,
  forty = forty,
  ninety = ninety
)

# rename bottom layer
rename_nit <- function(df) {
  colnames(df) <- sapply(colnames(df), function(x){
    if(x == 'DateTime')return('DateTime')
    if(startsWith(x, 'NIT_nit_4.7'))return('Bottom')
    return(x)
  }) 
  return(df)
}

datasets <- lapply(datasets, rename_nit)


# add coverage
coverage_values <- c(no = 0, ten = 10, forty = 40, ninety = 90)
datasets <- Map(function(df, cov){
  df$coverage <- cov
  return(df)
}, datasets, coverage_values)

# combine data
all_data <- bind_rows(datasets)

## add seasons and clean
all_nit <- all_data %>%
  select(DateTime, coverage, Bottom) %>%
  mutate(
    scenario = c('0'= 'A', '10' = 'B', '40' = 'C', '90' = 'D')[as.character(coverage)],
    date = as.POSIXct(DateTime, tz = 'UTC'),
    month = lubridate:: month(date),
    season = case_when(
      month %in% c(12, 1, 2) ~ 'Winter',
      month %in% c(3,4,5) ~ 'Spring',
      month %in% c(6,7,8) ~ 'Summer',
      TRUE~ 'Autumn'
    )
  )
# remove initial period(jan-mid feb at beginnign of simulation)

all_nit_filtered <- all_nit%>%
  filter(date>= as.Date('2016-02-15'))

# extract seasonal maxs
(nit_seasonal_max <- all_nit_filtered %>%
    group_by(scenario, season) %>%
    slice_max(order_by = Bottom, n = 1, with_ties = FALSE)%>%
    ungroup()%>%
    select(scenario, season, date, Bottom))

# filter by seasaon

all_do_filtered <- all_do%>%
  filter(date>= as.Date('2016-02-15'))

# extract seasonal mins
(dp_seasonal_min <- all_do_filtered %>%
    group_by(scenario, season) %>%
    slice_min(order_by = Bottom, n = 1, with_ties = FALSE)%>%
    ungroup()%>%
    select(scenario, season, date, Bottom))



ggsave(
  filename = "nit_scenarios_panel.png",
  path = 'scenarios/scenario_outputs/panel_bathymetric_scenario_plots/',
  plot = final_plot,
  width = 9.7,
  height = 6.61,
  dpi = 600
)
#plotting po4 for each scenario----
base_phs <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/baseline_output/output_baseline.nc",
  var_name = "PHS_frp",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0, 1.5))


# Add reversed depth scale, title, and adjust title size
(base_phs <- base_phs +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("A") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      legend.position = 'none'
    ))


# Save high-resolution PNG
ggsave(
  filename = "phs_baseline.png",
  path = 'scenarios/scenario_outputs/baseline_output/',
  plot = base_phs,
  width = 8,
  height = 6,
  dpi = 600
)

## 10% coverage
phs_10 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_10_output/output_10.nc",
  var_name = "PHS_frp",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0,1.5))
# Add reversed depth scale, title, and adjust title size
(phs_10 <- phs_10 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("B") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x= element_blank(),
      axis.line = element_blank(),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "phs_scenario10.png",
  path = 'scenarios/scenario_outputs/scenario_10_output',
  plot = phs_10,
  width = 8,
  height = 6,
  dpi = 600
)

# 40% coverage
phs_40 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_40_output/output_40.nc",
  var_name = "PHS_frp",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0, 1.5))

# Add reversed depth scale, title, and adjust title size
(phs_40 <- phs_40 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("C") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "phs_scenario40.png",
  path = 'scenarios/scenario_outputs/scenario_40_output/',
  plot = phs_40,
  width = 8,
  height = 6,
  dpi = 600
)
# 90% coverage
phs_90 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_90_output/output_90.nc",
  var_name = "PHS_frp",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0,1.5)
)

# Add  depth scale, title, and adjust title size
(phs_90 <- phs_90 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("D") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.position = 'none'
    ))

# find bottom max values
no <- get_var('baseline_output/output_baseline.nc', var_name = 'PHS_frp', reference = 'surface' )
ten <- get_var('scenario_10_output/output_10.nc', var_name = 'PHS_frp', reference = 'surface' )
forty <- get_var('scenario_40_output/output_40.nc', var_name = 'PHS_frp', reference = 'surface' )
ninety <- get_var('scenario_90_output/output_90.nc', var_name = 'PHS_frp', reference = 'surface' )

datasets <- list(
  no = no,
  ten = ten,
  forty = forty,
  ninety = ninety
)

# rename bottom layer
rename_phs <- function(df) {
  colnames(df) <- sapply(colnames(df), function(x){
    if(x == 'DateTime')return('DateTime')
    if(startsWith(x, 'PHS_frp_4.7'))return('Bottom')
    return(x)
  }) 
  return(df)
}

datasets <- lapply(datasets, rename_phs)


# add coverage
coverage_values <- c(no = 0, ten = 10, forty = 40, ninety = 90)
datasets <- Map(function(df, cov){
  df$coverage <- cov
  return(df)
}, datasets, coverage_values)

# combine data
all_data <- bind_rows(datasets)

## add seasons and clean
all_phs <- all_data %>%
  select(DateTime, coverage, Bottom) %>%
  mutate(
    scenario = c('0'= 'A', '10' = 'B', '40' = 'C', '90' = 'D')[as.character(coverage)],
    date = as.POSIXct(DateTime, tz = 'UTC'),
    month = lubridate:: month(date),
    season = case_when(
      month %in% c(12, 1, 2) ~ 'Winter',
      month %in% c(3,4,5) ~ 'Spring',
      month %in% c(6,7,8) ~ 'Summer',
      TRUE~ 'Autumn'
    )
  )
# remove initial period(jan-mid feb at beginnign of simulation)

all_phs_filtered <- all_phs%>%
  filter(date>= as.Date('2016-02-15'))

# extract seasonal maxs
(phs_seasonal_max <- all_phs_filtered %>%
    group_by(scenario, season) %>%
    slice_max(order_by = Bottom, n = 1, with_ties = FALSE)%>%
    ungroup()%>%
    select(scenario, season, date, Bottom))

# Save high-resolution PNG
ggsave(
  filename = "phs_scenario90.png",
  path = 'scenarios/scenario_outputs/scenario_90_output/',
  plot = phs_90,
  width = 8,
  height = 6,
  dpi = 600
)
base_phs <- base_phs + theme(axis.title = element_blank())
phs_10   <- phs_10   + theme(axis.title = element_blank())
phs_40   <- phs_40   + theme(axis.title = element_blank())
phs_90   <- phs_90   + theme(axis.title = element_blank())

phs_panel <- ((base_phs | phs_10) /
               (phs_40 | phs_90)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

phs_panel <- phs_panel +
  plot_annotation(
    theme = theme(
      plot.margin = margin(t = 5, r = 5, b = 25, l = 25), # space for axis titles
      plot.title = element_blank(),
      axis.text.x = element_text(size = 8)
    )
  )

phs_panel <- phs_panel &
  scale_x_date(
    labels = function(x) substr(format(x, '%b'), 1, 1),
    date_breaks = '1 month'
  )


(final_plot <- ggdraw(phs_panel) +
    draw_label("Date", x = 0.425, y = 0.02, vjust = 0) +
    draw_label("Depth (m)", x = 0.02, y = 0.5, angle = 90, vjust = 1))


ggsave(
  filename = "phs_scenarios_panel.png",
  path = 'scenarios/scenario_outputs/panel_bathymetric_scenario_plots/',
  plot = final_plot,
  width = 9.7,
  height = 6.61,
  dpi = 600
)

# plotting cyano----
base_cyno <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/baseline_output/output_baseline.nc",
  var_name = "PHY_cyno",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0.1, 0.3))


# Add reversed depth scale, title, and adjust title size
(base_cyno <- base_cyno +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("A") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      legend.position = 'none'
    ))


# Save high-resolution PNG
ggsave(
  filename = "cyno_baseline.png",
  path = 'scenarios/scenario_outputs/baseline_output/',
  plot = base_cyno,
  width = 8,
  height = 6,
  dpi = 600
)

## 10% coverage
cyno_10 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_10_output/output_10.nc",
  var_name = "PHY_cyno",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0.1, 0.3))
# Add reversed depth scale, title, and adjust title size
(cyno_10 <- cyno_10 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("B") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x= element_blank(),
      axis.line = element_blank(),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "cyno_scenario10.png",
  path = 'scenarios/scenario_outputs/scenario_10_output',
  plot = cyno_10,
  width = 8,
  height = 6,
  dpi = 600
)

# 40% coverage
cyno_40 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_40_output/output_40.nc",
  var_name = "PHY_cyno",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0.1, 0.3))

# Add reversed depth scale, title, and adjust title size
(cyno_40 <- cyno_40 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("C") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "cyno_scenario40.png",
  path = 'scenarios/scenario_outputs/scenario_40_output/',
  plot = cyno_40,
  width = 8,
  height = 6,
  dpi = 600
)
# 90% coverage
cyno_90 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_90_output/output_90.nc",
  var_name = "PHY_cyno",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
    zlim = c(0.1, 0.3)
)

# Add  depth scale, title, and adjust title size
(cyno_90 <- cyno_90 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("D") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "cyno_scenario90.png",
  path = 'scenarios/scenario_outputs/scenario_90_output/',
  plot = cyno_90,
  width = 8,
  height = 6,
  dpi = 600
)
base_cyno <- base_cyno + theme(axis.title = element_blank())
cyno_10   <- cyno_10   + theme(axis.title = element_blank())
cyno_40   <- cyno_40   + theme(axis.title = element_blank())
cyno_90   <- cyno_90   + theme(axis.title = element_blank())
library(patchwork)

cyno_panel <- ((base_cyno | cyno_10) /
               (cyno_40 | cyno_90)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

cyno_panel <- cyno_panel +
  plot_annotation(
    theme = theme(
      plot.margin = margin(t = 5, r = 5, b = 25, l = 25), # space for axis titles
      plot.title = element_blank(),
      axis.text.x = element_text(size = 8)
    )
  )

cyno_panel <- cyno_panel &
  scale_x_date(
    labels = function(x) substr(format(x, '%b'), 1, 1),
    date_breaks = '1 month'
  )


(final_plot_cyno <- ggdraw(cyno_panel))


ggsave(
  filename = "cyno_scenarios_panel_LOWRANGE.png",
  path = 'scenarios/scenario_outputs/panel_bathymetric_scenario_plots/',
  plot = final_plot,
  width = 9.7,
  height = 6.61,
  dpi = 600
)


# plotting diat----
base_diat <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/baseline_output/output_baseline.nc",
  var_name = "PHY_diat",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0.5, 65))


# Add reversed depth scale, title, and adjust title size
(base_diat <- base_diat +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("A") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      legend.position = 'none'
    ))


# Save high-resolution PNG
ggsave(
  filename = "diat_baseline.png",
  path = 'scenarios/scenario_outputs/baseline_output/',
  plot = base_diat,
  width = 8,
  height = 6,
  dpi = 600
)

## 10% coverage
diat_10 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_10_output/output_10.nc",
  var_name = "PHY_diat",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0.5, 65))
# Add reversed depth scale, title, and adjust title size
(diat_10 <- diat_10 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("B") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x= element_blank(),
      axis.line = element_blank(),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "diat_scenario10.png",
  path = 'scenarios/scenario_outputs/scenario_10_output',
  plot = diat_10,
  width = 8,
  height = 6,
  dpi = 600
)

# 40% coverage
diat_40 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_40_output/output_40.nc",
  var_name = "PHY_diat",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0.5,65))

# Add reversed depth scale, title, and adjust title size
(diat_40 <- diat_40 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("C") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "diat_scenario40.png",
  path = 'scenarios/scenario_outputs/scenario_40_output/',
  plot = diat_40,
  width = 8,
  height = 6,
  dpi = 600
)
# 90% coverage
diat_90 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_90_output/output_90.nc",
  var_name = "PHY_diat",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0.5, 65)
)

# Add  depth scale, title, and adjust title size
(diat_90 <- diat_90 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("D") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "diat_scenario90.png",
  path = 'scenarios/scenario_outputs/scenario_90_output/',
  plot = diat_90,
  width = 8,
  height = 6,
  dpi = 600
)
base_diat <- base_diat + theme(axis.title = element_blank())
diat_10   <- diat_10   + theme(axis.title = element_blank())
diat_40   <- diat_40   + theme(axis.title = element_blank())
diat_90   <- diat_90   + theme(axis.title = element_blank())

diat_panel <- ((base_diat | diat_10) /
               (diat_40 | diat_90)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

diat_panel <- diat_panel +
  plot_annotation(
    theme = theme(
      plot.margin = margin(t = 5, r = 5, b = 25, l = 25), # space for axis titles
      plot.title = element_blank(),
      axis.text.x = element_text(size = 8)
    )
  )

diat_panel <- diat_panel &
  scale_x_date(
    labels = function(x) substr(format(x, '%b'), 1, 1),
    date_breaks = '1 month'
  )


(final_plot_diat <- ggdraw(diat_panel) +
    draw_label("Depth (m)", x = 0.01, y = 0.5, angle = 90, vjust = 1))


ggsave(
  filename = "diat_scenarios_panelLOWRANGE.png",
  path = 'scenarios/scenario_outputs/panel_bathymetric_scenario_plots/',
  plot = final_plot,
  width = 9.7,
  height = 6.61,
  dpi = 600
)

# plotting greens----
base_green <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/baseline_output/output_baseline.nc",
  var_name = "PHY_green",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0, 85))


# Add reversed depth scale, title, and adjust title size
(base_green <- base_green +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("A") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      legend.position = 'none'
    ))


# Save high-resolution PNG
ggsave(
  filename = "green_baseline.png",
  path = 'scenarios/scenario_outputs/baseline_output/',
  plot = base_green,
  width = 8,
  height = 6,
  dpi = 600
)

## 10% coverage
green_10 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_10_output/output_10.nc",
  var_name = "PHY_green",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0, 85))
# Add reversed depth scale, title, and adjust title size
(green_10 <- green_10 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("B") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x= element_blank(),
      axis.line = element_blank(),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "green_scenario10.png",
  path = 'scenarios/scenario_outputs/scenario_10_output',
  plot = green_10,
  width = 8,
  height = 6,
  dpi = 600
)

# 40% coverage
green_40 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_40_output/output_40.nc",
  var_name = "PHY_green",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0, 85))

# Add reversed depth scale, title, and adjust title size
(green_40 <- green_40 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("C") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "green_scenario40.png",
  path = 'scenarios/scenario_outputs/scenario_40_output/',
  plot = green_40,
  width = 8,
  height = 6,
  dpi = 600
)
# 90% coverage
green_90 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_90_output/output_90.nc",
  var_name = "PHY_green",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0, 85)
)

# Add  depth scale, title, and adjust title size
(green_90 <- green_90 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("D") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.position = 'none'
    ))

# Save high-resolution PNG
ggsave(
  filename = "green_scenario90.png",
  path = 'scenarios/scenario_outputs/scenario_90_output/',
  plot = green_90,
  width = 8,
  height = 6,
  dpi = 600
)
base_green <- base_green + theme(axis.title = element_blank())
green_10   <- green_10   + theme(axis.title = element_blank())
green_40   <- green_40   + theme(axis.title = element_blank())
green_90   <- green_90   + theme(axis.title = element_blank())

green_panel <- ((base_green | green_10) /
                 (green_40 | green_90)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

green_panel <- green_panel +
  plot_annotation(
    theme = theme(
      plot.margin = margin(t = 5, r = 5, b = 25, l = 25), # space for axis titles
      plot.title = element_blank(),
      axis.text.x = element_text(size = 8)
    )
  )

green_panel <- green_panel &
  scale_x_date(
    labels = function(x) substr(format(x, '%b'), 1, 1),
    date_breaks = '1 month'
  )


(final_plot_green <- ggdraw(green_panel) +
    draw_label("Date", x = 0.425, y = 0.02, vjust = 0))

ggsave(
  filename = "green_scenarios_panel.png",
  path = 'scenarios/scenario_outputs/panel_bathymetric_scenario_plots/',
  plot = final_plot,
  width = 9.7,
  height = 6.61,
  dpi = 600
)


# making panel of phytoplankton----
panel <- final_plot_cyno + final_plot_diat + final_plot_green+
  plot_layout(ncol = 1)
panel
getwd()
ggsave(
  filename = "three_cyno.png",
  path = 'panel_bathymetric_scenario_plots',
  plot = panel,
  width = 10,
  height = 12,
  dpi = 600
)
