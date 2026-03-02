## running scenarios
# scenarios will be ran under 10%, 20%, 40% and 90% coverages

# running scenarios
# load libraries
library(GLM3r)
library(devtools)
library(glmtools)
library(GLMr)
library(lubridate)
library(tidyr)
library(janitor)
library(ncdf4)
library(ggplot2)

#### clear console  ####
cat("\f")
rm(list = ls())
library(devtools)
# will set the WD relative to where ever this script is stored
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios')
remove.packages('GLM3r')
# load GLMr3
devtools::install_github("robertladwig/GLM3r", ref = "v3.1.1")
library(GLM3r)
glm_version()

## change glm3.nml to the different met files for each scenario
GLM3r::run_glm(sim_folder = 'C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed', verbose = T)


## plotting do for each scenario----
# baseline
base_do <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/baseline_output/output_baseline.nc",
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
  ggtitle("Baseline: 0% FPV") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  ))

# Save high-resolution PNG
ggsave(
  filename = "do_baseline.png",
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
do_10 <- do_10 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 10%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "do_scenario10.png",
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
do_40 <- do_40 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 40%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "do_scenario40.png",
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
do_90 <- do_90 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 90%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "do_scenario90.png",
  plot = do_90,
  width = 8,
  height = 6,
  dpi = 600
)
# 4 panel comparison plot
library(magick)

img5 <- image_read("scenarios/scenario_outputs/baseline_output/do_baseline.png")
img6 <- image_read("scenarios/scenario_outputs/scenario_10_output/do_scenario10.png")
img7 <- image_read("scenarios/scenario_outputs/scenario_40_output/do_scenario40.png")
img8 <- image_read("scenarios/scenario_outputs/scenario_90_output/do_scenario90.png")

# Combine 2x2: first combine rows, then combine rows vertically
row3 <- image_append(c(img5, img6))  # horizontal append
row4 <- image_append(c(img7, img8))  # horizontal append
panel_do <- image_append(c(row3, row4), stack = TRUE)  # vertical append

# Save combined panel
image_write(panel_do, path = "do_scenarios_panel.png")
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
    ggtitle("Baseline: 0% FPV") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12)
    ))

# Save high-resolution PNG
ggsave(
  filename = "nit_baseline.png",
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
nit_10 <- nit_10 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 10%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "nit_scenario10.png",
  plot = nit_10,
  width = 8,
  height = 6,
  dpi = 600
)
# 20% coverage
nit_20 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_20_output/output_20.nc",
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
nit_20 <- nit_20 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 20%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "nit_scenario20.png",
  plot = nit_20,
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
nit_40 <- nit_40 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 40%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "nit_scenario40.png",
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
nit_90 <- nit_90 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 90%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "nit_scenario90.png",
  plot = nit_90,
  width = 8,
  height = 6,
  dpi = 600
)
# 4 panel comparison plot
library(magick)

img9 <- image_read("scenarios/scenario_outputs/baseline_output/nit_baseline.png")
img10 <- image_read("scenarios/scenario_outputs/scenario_10_output/nit_scenario10.png")
img11 <- image_read("scenarios/scenario_outputs/scenario_40_output/nit_scenario40.png")
img12 <- image_read("scenarios/scenario_outputs/scenario_90_output/nit_scenario90.png")

# Combine 2x2: first combine rows, then combine rows vertically
row5 <- image_append(c(img9, img10))  # horizontal append
row6 <- image_append(c(img11, img12))  # horizontal append
panel_nit <- image_append(c(row5, row6), stack = TRUE)  # vertical append
image_write(panel_nit, path = "nit_scenarios_panel.png")

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
base_temp <- base_temp +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Baseline: 0% FPV") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "temp_baseline.png",
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
temp_10 <- temp_10 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 10%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "temp_scenario10.png",
  plot = temp_10,
  width = 8,
  height = 6,
  dpi = 600
)
# 20% coverage
temp_20 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_20_output/output_20.nc",
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
temp_20 <- temp_20 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 20%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "temp_scenario20.png",
  plot = temp_20,
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
temp_40 <- temp_40 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 40%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "temp_scenario40.png",
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
temp_90 <- temp_90 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 90%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "temp_scenario90.png",
  plot = temp_90,
  width = 8,
  height = 6,
  dpi = 600
)
# 4 panel comparison plot
library(magick)

img1 <- image_read("scenarios/scenario_outputs/baseline_output/temp_baseline.png")
img2 <- image_read("scenarios/scenario_outputs/scenario_10_output/temp_scenario10.png")
img3 <- image_read("scenarios/scenario_outputs/scenario_40_output/temp_scenario40.png")
img4 <- image_read("scenarios/scenario_outputs/scenario_90_output/temp_scenario90.png")

# Combine 2x2: first combine rows, then combine rows vertically
row1 <- image_append(c(img1, img2))  # horizontal append
row2 <- image_append(c(img3, img4))  # horizontal append
panel_temp <- image_append(c(row1, row2), stack = TRUE)  # vertical append

# Save combined panel
image_write(panel_temp, path = "temp_scenarios_panel.png")
 
#plotting po4 for each scenario----
# baseline
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
  zlim = c(0,1.5))

# Add reversed depth scale, title, and adjust title size
(base_phs <- base_phs +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("Baseline: 0% FPV") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12)
    ))

# Save high-resolution PNG
ggsave(
  filename = "phs_baseline.png",
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
phs_10 <- phs_10 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 10%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "phs_scenario10.png",
  plot = phs_10,
  width = 8,
  height = 6,
  dpi = 600
)
# 20% coverage
phs_20 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_20_output/output_20.nc",
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
phs_20 <- phs_20 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 20%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "phs_scenario20.png",
  plot = phs_20,
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
  zlim = c(0,1.5))

# Add reversed depth scale, title, and adjust title size
phs_40 <- phs_40 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 40%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "phs_scenario40.png",
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
  ggtitle("Scenario: FPV 90%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  ))

# Save high-resolution PNG
ggsave(
  filename = "phs_scenario90.png",
  plot = phs_90,
  width = 8,
  height = 6,
  dpi = 600
)
# 4 panel comparison plot
library(magick)

img13 <- image_read("phs_baseline.png")
img14 <- image_read("phs_scenario10.png")
img15 <- image_read("phs_scenario40.png")
img16 <- image_read("phs_scenario90.png")

# Combine 2x2: first combine rows, then combine rows vertically
row7 <- image_append(c(img13, img14))  # horizontal append
row8 <- image_append(c(img15, img16))  # horizontal append
panel_phs <- image_append(c(row7, row8), stack = TRUE)  # vertical append
image_write(panel_phs, path = "phs_scenarios_panel.png")
 
# plotting cyano----

# baseline
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
  zlim = c(0.1,0.3))

# Add reversed depth scale, title, and adjust title size
(base_cyno <- base_cyno +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("Baseline: 0% FPV") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12)
    ))

# Save high-resolution PNG
ggsave(
  filename = "cyno_baseline.png",
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
  ggtitle("Scenario: FPV 10%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  ))

# Save high-resolution PNG
ggsave(
  filename = "cyno_scenario10.png",
  plot = cyno_10,
  width = 8,
  height = 6,
  dpi = 600
)
# 20% coverage
cyno_20 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_20_output/output_20.nc",
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
  zlim = c(0.1,0.3))

# Add reversed depth scale, title, and adjust title size
cyno_20 <- cyno_20 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 20%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "cyno_scenario20.png",
  plot = cyno_20,
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
  zlim = c(0.1,0.3))

# Add reversed depth scale, title, and adjust title size
cyno_40 <- cyno_40 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 40%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "cyno_scenario40.png",
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
  zlim = c(0.1,0.3)
)

# Add  depth scale, title, and adjust title size
(cyno_90 <- cyno_90 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("Scenario: FPV 90%") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12)
    ))

# Save high-resolution PNG
ggsave(
  filename = "cyno_scenario90.png",
  plot = cyno_90,
  width = 8,
  height = 6,
  dpi = 600
)
# 4 panel comparison plot
library(magick)

img13 <- image_read("cyno_baseline.png")
img14 <- image_read("cyno_scenario10.png")
img15 <- image_read("cyno_scenario40.png")
img16 <- image_read("cyno_scenario90.png")

# Combine 2x2: first combine rows, then combine rows vertically
row7 <- image_append(c(img13, img14))  # horizontal append
row8 <- image_append(c(img15, img16))  # horizontal append
panel_cyno <- image_append(c(row7, row8), stack = TRUE)  # vertical append
image_write(panel_cyno, path = "cyno_scenarios_panel.png")

# plotting diat----

# baseline
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
  zlim = c(0.25,1.35))

# Add reversed depth scale, title, and adjust title size
(base_diat <- base_diat +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("Baseline: 0% FPV") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12)
    ))

# Save high-resolution PNG
ggsave(
  filename = "diat_baseline.png",
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
  zlim = c(0.25, 1.35))
# Add reversed depth scale, title, and adjust title size
(diat_10 <- diat_10 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("Scenario: FPV 10%") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12)
    ))

# Save high-resolution PNG
ggsave(
  filename = "diat_scenario10.png",
  plot = diat_10,
  width = 8,
  height = 6,
  dpi = 600
)
# 20% coverage
diat_20 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_20_output/output_20.nc",
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
  zlim = c(0.25,1.35))

# Add reversed depth scale, title, and adjust title size
diat_20 <- diat_20 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 20%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "diat_scenario20.png",
  plot = diat_20,
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
  zlim = c(0.25,1.35))

# Add reversed depth scale, title, and adjust title size
diat_40 <- diat_40 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 40%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "diat_scenario40.png",
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
  zlim = c(0.25,1.35)
)


# Add  depth scale, title, and adjust title size
(diat_90 <- diat_90 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("Scenario: FPV 90%") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12)
    ))

# Save high-resolution PNG
ggsave(
  filename = "diat_scenario90.png",
  plot = diat_90,
  width = 8,
  height = 6,
  dpi = 600
)
# 4 panel comparison plot
library(magick)

img13 <- image_read("diat_baseline.png")
img14 <- image_read("diat_scenario10.png")
img15 <- image_read("diat_scenario40.png")
img16 <- image_read("diat_scenario90.png")

# Combine 2x2: first combine rows, then combine rows vertically
row7 <- image_append(c(img13, img14))  # horizontal append
row8 <- image_append(c(img15, img16))  # horizontal append
panel_diat <- image_append(c(row7, row8), stack = TRUE)  # vertical append
image_write(panel_diat, path = "diat_scenarios_panel.png")

## 
# plotting greens----
# baseline
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
  zlim = c(0,85))

# Add reversed depth scale, title, and adjust title size
(base_green <- base_green +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("Baseline: 0% FPV") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12)
    ))

# Save high-resolution PNG
ggsave(
  filename = "green_baseline.png",
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
    ggtitle("Scenario: FPV 10%") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12)
    ))

# Save high-resolution PNG
ggsave(
  filename = "green_scenario10.png",
  plot = green_10,
  width = 8,
  height = 6,
  dpi = 600
)
# 20% coverage
green_20 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_20_output/output_20.nc",
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
  zlim = c(0,85))

# Add reversed depth scale, title, and adjust title size
green_20 <- green_20 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 20%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "green_scenario20.png",
  plot = green_20,
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
  zlim = c(0,85))

# Add reversed depth scale, title, and adjust title size
green_40 <- green_40 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 40%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "green_scenario40.png",
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
  zlim = c(0,85)
)


# Add  depth scale, title, and adjust title size
(green_90 <- green_90 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("Scenario: FPV 90%") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12)
    ))

# Save high-resolution PNG
ggsave(
  filename = "green_scenario90.png",
  plot = green_90,
  width = 8,
  height = 6,
  dpi = 600
)
# 4 panel comparison plot
library(magick)

img13 <- image_read("green_baseline.png")
img14 <- image_read("green_scenario10.png")
img15 <- image_read("green_scenario40.png")
img16 <- image_read("green_scenario90.png")

# Combine 2x2: first combine rows, then combine rows vertically
row7 <- image_append(c(img13, img14))  # horizontal append
row8 <- image_append(c(img15, img16))  # horizontal append
panel_green <- image_append(c(row7, row8), stack = TRUE)  # vertical append
image_write(panel_green, path = "green_scenarios_panel.png")

# plotting bga----
base_bga <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/baseline_output/output_baseline.nc",
  var_name = "PHY_bga",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0,3))

# Add reversed depth scale, title, and adjust title size
(base_bga <- base_bga +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("Baseline: 0% FPV") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12)
    ))

# Save high-resolution PNG
ggsave(
  filename = "bga_baseline.png",
  plot = base_bga,
  width = 8,
  height = 6,
  dpi = 600
)

## 10% coverage
bga_10 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_10_output/output_10.nc",
  var_name = "PHY_bga",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0, 3))
# Add reversed depth scale, title, and adjust title size
(bga_10 <- bga_10 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("Scenario: FPV 10%") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12)
    ))

# Save high-resolution PNG
ggsave(
  filename = "bga_scenario10.png",
  plot = bga_10,
  width = 8,
  height = 6,
  dpi = 600
)
# 20% coverage
bga_20 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_20_output/output_20.nc",
  var_name = "PHY_bga",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0,3))

# Add reversed depth scale, title, and adjust title size
bga_20 <- bga_20 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 20%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "bga_scenario20.png",
  plot = bga_20,
  width = 8,
  height = 6,
  dpi = 600
)
# 40% coverage
bga_40 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_40_output/output_40.nc",
  var_name = "PHY_bga",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0,3))

# Add reversed depth scale, title, and adjust title size
bga_40 <- bga_40 +
  scale_y_reverse(limits = c(5.0, 0)) +
  ggtitle("Scenario: FPV 40%") +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Save high-resolution PNG
ggsave(
  filename = "bga_scenario40.png",
  plot = bga_40,
  width = 8,
  height = 6,
  dpi = 600
)
# 90% coverage
bga_90 <- plot_var_nc(
  nc_file = "C:/Users/sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs/scenario_90_output/output_90.nc",
  var_name = "PHY_bga",
  fig_path = NULL,
  min_depth = 0.0,
  max_depth = 6,
  interval = 0.1,
  text.size = 12,
  show.legend = TRUE,
  legend.position = "right",
  color.palette = "RdYlBu",
  color.direction = -1,
  zlim = c(0,3)
)


# Add  depth scale, title, and adjust title size
(bga_90 <- bga_90 +
    scale_y_reverse(limits = c(5.0, 0)) +
    ggtitle("Scenario: FPV 90%") +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12)
    ))

# Save high-resolution PNG
ggsave(
  filename = "bga_scenario90.png",
  plot = bga_90,
  width = 8,
  height = 6,
  dpi = 600
)
# 4 panel comparison plot
library(magick)

img13 <- image_read("bga_baseline.png")
img14 <- image_read("bga_scenario10.png")
img15 <- image_read("bga_scenario40.png")
img16 <- image_read("bga_scenario90.png")

# Combine 2x2: first combine rows, then combine rows vertically
row7 <- image_append(c(img13, img14))  # horizontal append
row8 <- image_append(c(img15, img16))  # horizontal append
panel_bga <- image_append(c(row7, row8), stack = TRUE)  # vertical append
image_write(panel_bga, path = "bga_scenarios_panel.png")
