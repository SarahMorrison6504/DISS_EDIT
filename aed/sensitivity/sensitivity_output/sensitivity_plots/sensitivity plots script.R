## plotting max vs min to determine sensitivity of FPV variables solar and wind


## solar- change in temp----
library(ncdf4)
library(ggplot2)
library(glmtools)
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

## get surface temp data from nc. file
temp_0 <- get_var('output_solar0.nc',
                  var_name = 'temp',
                  reference = 'surface')

temp_max <- get_var('output_solarMAX.nc',
                    var_name = 'temp',
                    reference = 'surface')

# check column names
colnames(temp_0)
colnames(temp_max)
## keep only temp and date
temp_zero <- temp_0[, c("DateTime", "temp_0")]
temp_max  <- temp_max[, c("DateTime", "temp_0")]

# merge datasets
compare <- merge(temp_0, temp_max, by = 'DateTime')
colnames(compare) <- c('DateTime', 'T_zero', 'T_max')

# calcualte change in temperature
compare$deltaT <- compare$T_max - compare$T_zero

# plot

(temp_solar_surface <- ggplot(compare, aes(x = DateTime, y = deltaT)) +
  geom_line(colour = 'red',linewidth = 1) +
  labs(y = expression(Delta*T~(degree*C)),
       title = "Surface") +
    coord_cartesian(ylim = c(0, 15))+
  theme_classic())


## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

temp_0 <- get_var('output_solar0.nc',
                  var_name = 'temp',
                  reference = 'surface')

temp_max <- get_var('output_solarMAX.nc',
                    var_name = 'temp',
                    reference = 'surface')
colnames(temp_0)
colnames(temp_max)
## keep only temp and date
temp_zero_mid <- temp_0[, c("DateTime", "temp_2.63435122780277")]
temp_max_mid  <- temp_max[, c("DateTime", "temp_2.63435122780277")]

# merge datasets
compare_temp_mid <- merge(temp_zero_mid, temp_max_mid, by = 'DateTime')
colnames(compare_temp_mid) <- c('DateTime', 'T_zero', 'T_max')

# calcualte change in temperature
compare_temp_mid$deltaT <- compare_temp_mid$T_max - compare_temp_mid$T_zero

(temp_solar_mid <- ggplot(compare_temp_mid, aes(x = DateTime, y = deltaT)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*T~(degree*C), x = NULL,),
         title = "Middle") +
    coord_cartesian(ylim = c(0,15))+
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

temp_0_bottom <- get_var('output_solar0.nc',
                  var_name = 'temp',
                  reference = 'surface')

temp_max_bottom <- get_var('output_solarMAX.nc',
                    var_name = 'temp',
                    reference = 'surface')
colnames(temp_0_bottom)
colnames(temp_max_bottom)

## keep only temp and date
temp_zero_bottom <- temp_0_bottom[, c("DateTime", "temp_4.74183221004499")]
temp_max_bottom  <- temp_max_bottom[, c("DateTime", "temp_4.74183221004499")]

# merge datasets
compare_temp_bottom <- merge(temp_zero_bottom, temp_max_bottom, by = 'DateTime')
colnames(compare_temp_bottom) <- c('DateTime', 'T_zero', 'T_max')

# calcualte change in temperature
compare_temp_bottom$deltaT <- compare_temp_bottom$T_max - compare_temp_bottom$T_zero

(temp_solar_bottom <- ggplot(compare_temp_bottom, aes(x = DateTime, y = deltaT)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*T~(degree*C)),
         title = "Bottom") +
    coord_cartesian(ylim = c(0,15))+
    theme_classic())

## panel plot
#remove axis of surface and middle
temp_solar_surface <- temp_solar_surface +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

temp_solar_mid <- temp_solar_mid +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())
library(patchwork)

final_plot <- temp_solar_surface /
  temp_solar_mid /
  temp_solar_bottom +
  plot_annotation(
    title = "Thermal Sensitivity to Solar Radiation Extremes"
  )

final_plot
ggsave(
  filename = "thermal_solar.png",
  plot = final_plot,
  width = 8,
  height = 6,
  dpi = 600
)
# Combine your three depths into one dataframe
deltaT_all <- data.frame(
  DateTime = compare$DateTime,
  surface = compare$deltaT,
  middle  = compare_temp_mid$deltaT,
  bottom  = compare_temp_bottom$deltaT
)

# Compute summary per timestep
deltaT_summary <- deltaT_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

# Plot single summary figure
thermal_solar <- ggplot(deltaT_summary, aes(x = DateTime)) +
  geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "orange", alpha = 0.3) +
  geom_line(aes(y = delta_avg), color = "red", size = 1) +
  labs(
    y = expression(Delta*T~(degree*C)),
    x = "DateTime",
    title = "Thermal Sensitivity to Solar Radiation Extremes"
  ) +
  coord_cartesian(ylim = c(0, 15)) +
  theme_classic()


# saving the plot of 3
png('Solar Sensitivity Temp.png', width = 8, height = 5)
print(temp_solar_surface)
print(temp_solar_mid)
print(temp_solar_bottom)
dev.off()
## solar impact on NO3----
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

## get surface temp data from nc. file
temp_0 <- get_var('output_solar0.nc',
                  var_name = 'NIT_nit',
                  reference = 'surface')

temp_max <- get_var('output_solarMAX.nc',
                    var_name = 'NIT_nit',
                    reference = 'surface')

# check column names
colnames(temp_0)
colnames(temp_max)
## keep only temp and date
temp_zero <- temp_0[, c("DateTime", "NIT_nit_0")]
temp_max  <- temp_max[, c("DateTime", "NIT_nit_0")]

# merge datasets
compare <- merge(temp_0, temp_max, by = 'DateTime')
colnames(compare) <- c('DateTime', 'NIT_zero', 'NIT_max')

# calcualte change in temperature
compare$deltaNIT <- compare$NIT_max - compare$NIT_zero

# plot

(NIT_solar_surface <- ggplot(compare, aes(x = DateTime, y = deltaNIT)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*NO3), x = NULL) +
    theme_classic())


## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

nit_0 <- get_var('output_solar0.nc',
                  var_name = 'NIT_nit',
                  reference = 'surface')

nit_max <- get_var('output_solarMAX.nc',
                    var_name = 'NIT_nit',
                    reference = 'surface')
colnames(temp_0)
colnames(temp_max)
## keep only temp and date
nit_zero_mid <- nit_0[, c("DateTime", "NIT_nit_2.63435122780277")]
nit_max_mid  <- nit_max[, c("DateTime", "NIT_nit_2.63435122780277")]

# merge datasets
compare_nit_mid <- merge(nit_zero_mid, nit_max_mid, by = 'DateTime')
colnames(compare_nit_mid) <- c('DateTime', 'NIT_zero', 'NIT_max')

# calcualte change in temperature
compare_nit_mid$deltaNIT <- compare_nit_mid$NIT_max - compare_nit_mid$NIT_zero

(nit_solar_mid <- ggplot(compare_nit_mid, aes(x = DateTime, y = deltaNIT)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*NO3), x = NULL) +
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

NIT_0_bottom <- get_var('output_solar0.nc',
                         var_name = 'NIT_nit',
                         reference = 'surface')

NIT_max_bottom <- get_var('output_solarMAX.nc',
                           var_name = 'NIT_nit',
                           reference = 'surface')
colnames(NIT_0_bottom)
colnames(NIT_max_bottom)
## keep only temp and date
NIT_zero_bottom <- NIT_0_bottom[, c("DateTime", "NIT_nit_4.74183221004499")]
NIT_max_bottom  <- NIT_max_bottom[, c("DateTime", "NIT_nit_4.74183221004499")]

# merge datasets
compare_NIT_bottom <- merge(NIT_zero_bottom, NIT_max_bottom, by = 'DateTime')
colnames(compare_NIT_bottom) <- c('DateTime', 'NIT_zero', 'NIT_max')

# calcualte change in temperature
compare_NIT_bottom$deltaNIT <- compare_NIT_bottom$NIT_max - compare_NIT_bottom$NIT_zero

(NIT_solar_bottom <- ggplot(compare_NIT_bottom, aes(x = DateTime, y = deltaNIT)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*NO3)) +
    theme_classic())

# remove axis titles
NIT_solar_surface <- NIT_solar_surface +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

nit_solar_mid <- nit_solar_mid +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())
library(patchwork)

final_plot_nit <- NIT_solar_surface /
  nit_solar_mid /
  NIT_solar_bottom +
  plot_annotation(
    title = "Nitrate Sensitivity to Solar Radiation Extremes"
  )

final_plot_nit
ggsave(
  filename = "nit_solar.png",
  plot = final_plot_nit,
  width = 8,
  height = 6,
  dpi = 600
)

library(dplyr)
library(ggplot2)

# Combine your three depths into one dataframe
deltaNIT_all <- data.frame(
  DateTime = compare$DateTime,
  surface = compare$deltaNIT,
  middle  = compare_nit_mid$deltaNIT,
  bottom  = compare_NIT_bottom$deltaNIT
)

# Compute summary per timestep
deltaNIT_summary <- deltaNIT_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

# Plot single summary figure
(nit_solar <- ggplot(deltaNIT_summary, aes(x = DateTime)) +
  geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "orange", alpha = 0.3) +
  geom_line(aes(y = delta_avg), color = "red", size = 1) +
  labs(
    y = expression(Delta*NIT),
    x = "DateTime",
    title = "NO3 Sensitivity to Solar Radiation Extremes"
  ) +
  coord_cartesian(ylim = c(-110, 50)) +
  theme_classic())

pdf('Solar Sensitivity NIT.pdf', width = 8, height = 5)
print(NIT_solar_surface)
print(nit_solar_mid)
print(NIT_solar_bottom)
dev.off()
## solar impact on phosphate----
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

## get surface temp data from nc. file
phs_0 <- get_var('output_solar0.nc',
                  var_name = 'PHS_frp',
                  reference = 'surface')

phs_max <- get_var('output_solarMAX.nc',
                    var_name = 'PHS_frp',
                    reference = 'surface')

# check column names
colnames(phs_0)
colnames(phs_max)
## keep only temp and date
phs_zero <- phs_0[, c("DateTime", "PHS_frp_0")]
phs_max  <- phs_max[, c("DateTime", "PHS_frp_0")]

# merge datasets
comparePHS <- merge(phs_0, phs_max, by = 'DateTime')
colnames(comparePHS) <- c('DateTime', 'phs_zero', 'phs_max')

# calcualte change in temperature
comparePHS$deltaPHS <- comparePHS$phs_max - comparePHS$phs_zero

# plot

(phs_solar_surface <- ggplot(comparePHS, aes(x = DateTime, y = deltaPHS)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*PO4)) +
    theme_classic())


## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

phs_0 <- get_var('output_solar0.nc',
                 var_name = 'PHS_frp',
                 reference = 'surface')

phs_max <- get_var('output_solarMAX.nc',
                   var_name = 'PHS_frp',
                   reference = 'surface')
colnames(phs_0)
colnames(phs_max)
## keep only temp and date
phs_zero_mid <- phs_0[, c("DateTime", "PHS_frp_2.63435122780277")]
phs_max_mid  <- phs_max[, c("DateTime", "PHS_frp_2.63435122780277")]

# merge datasets
compare_phs_mid <- merge(phs_zero_mid, phs_max_mid, by = 'DateTime')
colnames(compare_phs_mid) <- c('DateTime', 'phs_zero', 'phs_max')

# calcualte change in temperature
compare_phs_mid$deltaPHS <- compare_phs_mid$phs_max - compare_phs_mid$phs_zero

(phs_solar_mid <- ggplot(compare_phs_mid, aes(x = DateTime, y = deltaPHS)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*PO4))+
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

phs_0_bottom <- get_var('output_solar0.nc',
                        var_name = 'PHS_frp',
                        reference = 'surface')

phs_max_bottom <- get_var('output_solarMAX.nc',
                          var_name = 'PHS_frp',
                          reference = 'surface')
colnames(phs_0_bottom)
colnames(phs_max_bottom)
## keep only temp and date
phs_zero_bottom <- phs_0_bottom[, c("DateTime", "PHS_frp_4.74183221004499")]
phs_max_bottom  <- phs_max_bottom[, c("DateTime", "PHS_frp_4.74183221004499")]

# merge datasets
compare_phs_bottom <- merge(phs_zero_bottom, phs_max_bottom, by = 'DateTime')
colnames(compare_phs_bottom) <- c('DateTime', 'phs_zero', 'phs_max')

# calcualte change in temperature
compare_phs_bottom$deltaPHS <- compare_phs_bottom$phs_max - compare_phs_bottom$phs_zero

(phs_solar_bottom <- ggplot(compare_phs_bottom, aes(x = DateTime, y = deltaPHS)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*PO4))+
    theme_classic())

  # remove axis titles
phs_solar_surface <- phs_solar_surface +
theme(axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank())
  
phs_solar_mid <- phs_solar_mid +
theme(axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank())
library(patchwork)
  
(final_plot_phs <- phs_solar_surface /
  phs_solar_mid /
  phs_solar_bottom +
    plot_annotation(
      title = "Phosphate Sensitivity to Solar Radiation Extremes"
    ))
ggsave(
  filename = "phs_solar.png",
  plot = final_plot_phs,
  width = 8,
  height = 6,
  dpi = 600
)

# Combine your three depths into one dataframe
deltaPHS_all <- data.frame(
  DateTime = comparePHS$DateTime,
  surface = comparePHS$deltaPHS,
  middle  = compare_phs_mid$deltaPHS,
  bottom  = compare_phs_bottom$deltaPHS
)

# Compute summary per timestep
deltaPHS_summary <- deltaPHS_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

# Plot single summary figure
(phs_solar <- ggplot(deltaPHS_summary, aes(x = DateTime)) +
    geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "orange", alpha = 0.3) +
    geom_line(aes(y = delta_avg), color = "red", size = 1) +
    labs(
      y = expression(Delta*PHS),
      x = "DateTime",
      title = "PO4 Sensitivity to Solar Radiation Extremes"
    ) +
    coord_cartesian(ylim = c(-1.0, 1)) +
    theme_classic())


pdf('Solar SensitivityPO4.pdf', width = 8, height = 5)
print(phs_solar_surface)
print(phs_solar_mid)
print(phs_solar_bottom)
dev.off()

## solar impact on DO----
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

## get surface temp data from nc. file
do_0 <- get_var('output_solar0.nc',
                 var_name = 'OXY_oxy',
                 reference = 'surface')

do_max <- get_var('output_solarMAX.nc',
                   var_name = 'OXY_oxy',
                   reference = 'surface')

# check column names
colnames(do_0)
colnames(do_max)
## keep only temp and date
do_0 <- do_0[, c("DateTime", "OXY_oxy_0")]
do_max  <- do_max[, c("DateTime", "OXY_oxy_0")]

# merge datasets
comparedo <- merge(do_0, do_max, by = 'DateTime')
colnames(comparedo) <- c('DateTime', 'do_0', 'do_max')

# calcualte change in temperature
comparedo$deltado <- comparedo$do_max - comparedo$do_0

# plot

(do_solar_surface <- ggplot(comparedo, aes(x = DateTime, y = deltado)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*DO))+
    theme_classic())


## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

do_0 <- get_var('output_solar0.nc',
                 var_name = 'OXY_oxy',
                 reference = 'surface')

do_max <- get_var('output_solarMAX.nc',
                   var_name = 'OXY_oxy',
                   reference = 'surface')
colnames(do_0)
colnames(do_max)
## keep only temp and date
do_zero_mid <- do_0[, c("DateTime", "OXY_oxy_2.63435122780277")]
do_max_mid  <- do_max[, c("DateTime", "OXY_oxy_2.63435122780277")]

# merge datasets
compare_do_mid <- merge(do_zero_mid, do_max_mid, by = 'DateTime')
colnames(compare_do_mid) <- c('DateTime', 'do_zero', 'do_max')

# calcualte change in temperature
compare_do_mid$deltaDO <- compare_do_mid$do_max - compare_do_mid$do_zero

(do_solar_mid <- ggplot(compare_do_mid, aes(x = DateTime, y = deltaDO)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*DO))+
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

do_0_bottom <- get_var('output_solar0.nc',
                        var_name = 'OXY_oxy',
                        reference = 'surface')

do_max_bottom <- get_var('output_solarMAX.nc',
                          var_name = 'OXY_oxy',
                          reference = 'surface')
colnames(do_0_bottom)
colnames(do_max_bottom)
## keep only temp and date
do_zero_bottom <- do_0_bottom[, c("DateTime", "OXY_oxy_4.74183221004499")]
do_max_bottom  <- do_max_bottom[, c("DateTime", "OXY_oxy_4.74183221004499")]

# merge datasets
compare_do_bottom <- merge(do_zero_bottom, do_max_bottom, by = 'DateTime')
colnames(compare_do_bottom) <- c('DateTime', 'do_zero', 'do_max')

# calcualte change in temperature
compare_do_bottom$deltado <- compare_do_bottom$do_max - compare_do_bottom$do_zero

(do_solar_bottom <- ggplot(compare_do_bottom, aes(x = DateTime, y = deltado)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*DO))+
    theme_classic())

# remove axis titles
do_solar_surface <- do_solar_surface +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

do_solar_mid <- do_solar_mid +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())
library(patchwork)

(final_plot_do <- do_solar_surface /
    do_solar_mid /
    do_solar_bottom +
    plot_annotation(
      title = "Dissolved Oxygen Sensitivity to Solar Radiation Extremes"
    ))
ggsave(
  filename = "do_solar.png",
  plot = final_plot_do,
  width = 8,
  height = 6,
  dpi = 600
)

# Combine your three depths into one dataframe
deltado_all <- data.frame(
  DateTime = comparedo$DateTime,
  surface = comparedo$deltado,
  middle  = compare_do_mid$deltaDO,
  bottom  = compare_do_bottom$deltado
)

# Compute summary per timestep
deltado_summary <- deltado_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

# Plot single summary figure
(do_solar <- ggplot(deltado_summary, aes(x = DateTime)) +
    geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "orange", alpha = 0.3) +
    geom_line(aes(y = delta_avg), color = "red", size = 1) +
    labs(
      y = expression(Delta*DO),
      x = "DateTime",
      title = "Dissolved Oxygen Sensitivity to Solar Radiation Extremes"
    ) +
    coord_cartesian(ylim = c(-800, 850)) +
    theme_classic())


pdf('Solar SensitivityPO4.pdf', width = 8, height = 5)
print(phs_solar_surface)
print(phs_solar_mid)
print(phs_solar_bottom)
dev.off()

## solar impact on cyano----
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

## get surface temp data from nc. file
cyno_0 <- get_var('output_solar0.nc',
                 var_name = 'PHY_cyno',
                 reference = 'surface')

cyno_max <- get_var('output_solarMAX.nc',
                   var_name = 'PHY_cyno',
                   reference = 'surface')

# check column names
colnames(cyno_0)
colnames(cyno_max)
## keep only temp and date
cyno_zero <- cyno_0[, c("DateTime", "PHY_cyno_0")]
cyno_max  <- cyno_max[, c("DateTime", "PHY_cyno_0")]

# merge datasets
compare <- merge(cyno_0, cyno_max, by = 'DateTime')
colnames(compare) <- c('DateTime', 'cyno_zero', 'cyno_max')

# calcualte change in temperature
compare$deltaCYNO <- compare$cyno_max - compare$cyno_zero

# plot

(cyno_solar_surface <- ggplot(compare, aes(x = DateTime, y = deltaCYNO)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*CYANO))+
    theme_classic())


## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

cyno_0 <- get_var('output_solar0.nc',
                 var_name = 'PHY_cyno',
                 reference = 'surface')

cyno_max <- get_var('output_solarMAX.nc',
                   var_name = 'PHY_cyno',
                   reference = 'surface')
colnames(cyno_0)
colnames(cyno_max)
## keep only temp and date
cyno_zero_mid <- cyno_0[, c("DateTime", "PHY_cyno_2.63435122780277")]
cyno_max_mid  <- cyno_max[, c("DateTime", "PHY_cyno_2.63435122780277")]

# merge datasets
compare_cyno_mid <- merge(cyno_zero_mid, cyno_max_mid, by = 'DateTime')
colnames(compare_cyno_mid) <- c('DateTime', 'cyno_zero', 'cyno_max')

# calcualte change in temperature
compare_cyno_mid$deltaCYNO <- compare_cyno_mid$cyno_max - compare_cyno_mid$cyno_zero

(cyno_solar_mid <- ggplot(compare_cyno_mid, aes(x = DateTime, y = deltaCYNO)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*CYNO))+
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

cyno_0_bottom <- get_var('output_solar0.nc',
                        var_name = 'PHY_cyno',
                        reference = 'surface')

cyno_max_bottom <- get_var('output_solarMAX.nc',
                          var_name = 'PHY_cyno',
                          reference = 'surface')
colnames(cyno_0_bottom)
colnames(cyno_max_bottom)
## keep only temp and date
cyno_zero_bottom <- cyno_0_bottom[, c("DateTime", "PHY_cyno_4.74183221004499")]
cyno_max_bottom  <- cyno_max_bottom[, c("DateTime", "PHY_cyno_4.74183221004499")]

# merge datasets
compare_cyno_bottom <- merge(cyno_zero_bottom, cyno_max_bottom, by = 'DateTime')
colnames(compare_cyno_bottom) <- c('DateTime', 'cyno_zero', 'cyno_max')

# calcualte change in temperature
compare_cyno_bottom$deltaCYNO <- compare_cyno_bottom$cyno_max - compare_cyno_bottom$cyno_zero

(cyno_solar_bottom <- ggplot(compare_cyno_bottom, aes(x = DateTime, y = deltaCYNO)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*CYNO))+
    theme_classic())

# remove axis titles
cyno_solar_surface <- cyno_solar_surface +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

cyno_solar_mid <- cyno_solar_mid +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())
library(patchwork)

(final_plot_cyno <- cyno_solar_surface /
    cyno_solar_mid /
    cyno_solar_bottom +
    plot_annotation(
      title = "Cynobacteria Sensitivity to Solar Radiation Extremes"
    ))
ggsave(
  filename = "cyno_solar.png",
  plot = final_plot_cyno,
  width = 8,
  height = 6,
  dpi = 600
)

# Combine your three depths into one dataframe
deltaCYNO_all <- data.frame(
  DateTime = compare$DateTime,
  surface = compare$deltaCYNO,
  middle  = compare_cyno_mid$deltaCYNO,
  bottom  = compare_cyno_bottom$deltaCYNO
)

# Compute summary per timestep
deltaCYNO_summary <- deltaCYNO_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

# Plot single summary figure
(cyno_solar <- ggplot(deltaCYNO_summary, aes(x = DateTime)) +
    geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "orange", alpha = 0.3) +
    geom_line(aes(y = delta_avg), color = "red", size = 1) +
    labs(
      y = expression(Delta*CYNO),
      x = "DateTime",
      title = "Cyno Sensitivity to Solar Radiation Extremes"
    ) +
    coord_cartesian(ylim = c(0, 40)) +
    theme_classic())

pdf('Solar SensitivityCYNO.pdf', width = 8, height = 5)
print(cyno_solar_surface)
print(cyno_solar_mid)
print(cyno_solar_bottom)
dev.off()

## solar impact on diatoms----
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

## get surface temp data from nc. file
diat_0 <- get_var('output_solar0.nc',
                  var_name = 'PHY_diat',
                  reference = 'surface')

diat_max <- get_var('output_solarMAX.nc',
                    var_name = 'PHY_diat',
                    reference = 'surface')

# check column names
colnames(diat_0)
colnames(diat_max)
## keep only temp and date
diat_zero <- diat_0[, c("DateTime", "PHY_diat_0")]
diat_max  <- diat_max[, c("DateTime", "PHY_diat_0")]

# merge datasets
compare <- merge(diat_0, diat_max, by = 'DateTime')
colnames(compare) <- c('DateTime', 'diat_zero', 'diat_max')

# calcualte change in temperature
compare$deltadiat <- compare$diat_max - compare$diat_zero

# plot

(diat_solar_surface <- ggplot(compare, aes(x = DateTime, y = deltadiat)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*DIAT))+
    theme_classic())


## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

diat_0 <- get_var('output_solar0.nc',
                  var_name = 'PHY_diat',
                  reference = 'surface')

diat_max <- get_var('output_solarMAX.nc',
                    var_name = 'PHY_diat',
                    reference = 'surface')
colnames(diat_0)
colnames(diat_max)
## keep only temp and date
diat_zero_mid <- diat_0[, c("DateTime", "PHY_diat_2.63435122780277")]
diat_max_mid  <- diat_max[, c("DateTime", "PHY_diat_2.63435122780277")]

# merge datasets
compare_diat_mid <- merge(diat_zero_mid, diat_max_mid, by = 'DateTime')
colnames(compare_diat_mid) <- c('DateTime', 'diat_zero', 'diat_max')

# calcualte change in temperature
compare_diat_mid$deltadiat <- compare_diat_mid$diat_max - compare_diat_mid$diat_zero

(diat_solar_mid <- ggplot(compare_diat_mid, aes(x = DateTime, y = deltadiat)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*DIAT))+
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

diat_0_bottom <- get_var('output_solar0.nc',
                         var_name = 'PHY_diat',
                         reference = 'surface')

diat_max_bottom <- get_var('output_solarMAX.nc',
                           var_name = 'PHY_diat',
                           reference = 'surface')
colnames(diat_0_bottom)
colnames(diat_max_bottom)
## keep only temp and date
diat_zero_bottom <- diat_0_bottom[, c("DateTime", "PHY_diat_4.74183221004499")]
diat_max_bottom  <- diat_max_bottom[, c("DateTime", "PHY_diat_4.74183221004499")]

# merge datasets
compare_diat_bottom <- merge(diat_zero_bottom, diat_max_bottom, by = 'DateTime')
colnames(compare_diat_bottom) <- c('DateTime', 'diat_zero', 'diat_max')

# calcualte change in temperature
compare_diat_bottom$deltadiat <- compare_diat_bottom$diat_max - compare_diat_bottom$diat_zero

(diat_solar_bottom <- ggplot(compare_diat_bottom, aes(x = DateTime, y = deltadiat)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*DIAT))+
    theme_classic())

# remove axis titles
diat_solar_surface <- diat_solar_surface +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

diat_solar_mid <- diat_solar_mid +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())
library(patchwork)

(final_plot_diat <- diat_solar_surface /
    diat_solar_mid /
    diat_solar_bottom +
    plot_annotation(
      title = "Diatom Sensitivity to Solar Radiation Extremes"
    ))
ggsave(
  filename = "diat_solar.png",
  plot = final_plot_diat,
  width = 8,
  height = 6,
  dpi = 600
)

# Combine your three depths into one dataframe
deltadiat_all <- data.frame(
  DateTime = compare$DateTime,
  surface = compare$deltadiat,
  middle  = compare_diat_mid$deltadiat,
  bottom  = compare_diat_bottom$deltadiat
)

# Compute summary per timestep
deltadiat_summary <- deltadiat_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

# Plot single summary figure
(diat_solar <- ggplot(deltadiat_summary, aes(x = DateTime)) +
    geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "orange", alpha = 0.3) +
    geom_line(aes(y = delta_avg), color = "red", size = 1) +
    labs(
      y = expression(Delta*DIAT),
      x = "DateTime",
      title = "Diat Sensitivity to Solar Radiation Extremes"
    ) +
    coord_cartesian(ylim = c(-100, 10)) +
    theme_classic())

pdf('Solar SensitivityDiat.pdf', width = 8, height = 5)
print(diat_solar_surface)
print(diat_solar_mid)
print(diat_solar_bottom)
dev.off()

## solar impact on blue/green alge----
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

## get surface temp data from nc. file
bga_0 <- get_var('output_solar0.nc',
                  var_name = 'PHY_bga',
                  reference = 'surface')

bga_max <- get_var('output_solarMAX.nc',
                    var_name = 'PHY_bga',
                    reference = 'surface')

# check column names
colnames(bga_0)
colnames(bga_max)
## keep only temp and date
bga_zero <- bga_0[, c("DateTime", "PHY_bga_0")]
bga_max  <- bga_max[, c("DateTime", "PHY_bga_0")]

# merge datasets
compare <- merge(bga_0, bga_max, by = 'DateTime')
colnames(compare) <- c('DateTime', 'bga_zero', 'bga_max')

# calcualte change in temperature
compare$deltabga <- compare$bga_max - compare$bga_zero

# plot

(bga_solar_surface <- ggplot(compare, aes(x = DateTime, y = deltabga)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*BGA))+
    theme_classic())


## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

bga_0 <- get_var('output_solar0.nc',
                  var_name = 'PHY_bga',
                  reference = 'surface')

bga_max <- get_var('output_solarMAX.nc',
                    var_name = 'PHY_bga',
                    reference = 'surface')
colnames(bga_0)
colnames(bga_max)
## keep only temp and date
bga_zero_mid <- bga_0[, c("DateTime", "PHY_bga_2.63435122780277")]
bga_max_mid  <- bga_max[, c("DateTime", "PHY_bga_2.63435122780277")]

# merge datasets
compare_bga_mid <- merge(bga_zero_mid, bga_max_mid, by = 'DateTime')
colnames(compare_bga_mid) <- c('DateTime', 'bga_zero', 'bga_max')

# calcualte change in temperature
compare_bga_mid$deltabga <- compare_bga_mid$bga_max - compare_bga_mid$bga_zero

(bga_solar_mid <- ggplot(compare_bga_mid, aes(x = DateTime, y = deltabga)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*BGA))+
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

bga_0_bottom <- get_var('output_solar0.nc',
                         var_name = 'PHY_bga',
                         reference = 'surface')

bga_max_bottom <- get_var('output_solarMAX.nc',
                           var_name = 'PHY_bga',
                           reference = 'surface')
colnames(bga_0_bottom)
colnames(bga_max_bottom)
## keep only temp and date
bga_zero_bottom <- bga_0_bottom[, c("DateTime", "PHY_bga_4.74183221004499")]
bga_max_bottom  <- bga_max_bottom[, c("DateTime", "PHY_bga_4.74183221004499")]

# merge datasets
compare_bga_bottom <- merge(bga_zero_bottom, bga_max_bottom, by = 'DateTime')
colnames(compare_bga_bottom) <- c('DateTime', 'bga_zero', 'bga_max')

# calcualte change in temperature
compare_bga_bottom$deltabga <- compare_bga_bottom$bga_max - compare_bga_bottom$bga_zero

(bga_solar_bottom <- ggplot(compare_bga_bottom, aes(x = DateTime, y = deltabga)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*BGA))+
    theme_classic())
# remove axis titles
bga_solar_surface <- bga_solar_surface +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

bga_solar_mid <- bga_solar_mid +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())
library(patchwork)

(final_plot_bga <- bga_solar_surface /
    bga_solar_mid /
    bga_solar_bottom +
    plot_annotation(
      title = "Blue Green Algae Sensitivity to Solar Radiation Extremes"
    ))
ggsave(
  filename = "bga_solar.png",
  plot = final_plot_bga,
  width = 8,
  height = 6,
  dpi = 600
)


# Combine your three depths into one dataframe
deltabga_all <- data.frame(
  DateTime = compare$DateTime,
  surface = compare$deltabga,
  middle  = compare_bga_mid$deltabga,
  bottom  = compare_bga_bottom$deltabga
)

# Compute summary per timestep
deltabga_summary <- deltabga_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

# Plot single summary figure
(bga_solar <- ggplot(deltabga_summary, aes(x = DateTime)) +
    geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "orange", alpha = 0.3) +
    geom_line(aes(y = delta_avg), color = "red", size = 1) +
    labs(
      y = expression(Delta*BGA),
      x = "DateTime",
      title = "Blue Green Algae Sensitivity to Solar Radiation Extremes"
    ) +
    coord_cartesian(ylim = c(-2, 1)) +
    theme_classic())
pdf('Solar SensitivityBGA.pdf', width = 8, height = 5)
print(bga_solar_surface)
print(bga_solar_mid)
print(bga_solar_bottom)
dev.off()

## solar impact on green----
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

## get surface temp data from nc. file
green_0 <- get_var('output_solar0.nc',
                 var_name = 'PHY_green',
                 reference = 'surface')

green_max <- get_var('output_solarMAX.nc',
                   var_name = 'PHY_green',
                   reference = 'surface')

# check column names
colnames(green_0)
colnames(green_max)
## keep only temp and date
green_zero <- green_0[, c("DateTime", "PHY_green_0")]
green_max  <- green_max[, c("DateTime", "PHY_green_0")]

# merge datasets
compare <- merge(green_0, green_max, by = 'DateTime')
colnames(compare) <- c('DateTime', 'green_zero', 'green_max')

# calcualte change in temperature
compare$deltagreen <- compare$green_max - compare$green_zero

# plot

(green_solar_surface <- ggplot(compare, aes(x = DateTime, y = deltagreen)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*GREEN))+
    theme_classic())


## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

green_0 <- get_var('output_solar0.nc',
                 var_name = 'PHY_green',
                 reference = 'surface')

green_max <- get_var('output_solarMAX.nc',
                   var_name = 'PHY_green',
                   reference = 'surface')
colnames(green_0)
colnames(green_max)
## keep only temp and date
green_zero_mid <- green_0[, c("DateTime", "PHY_green_2.63435122780277")]
green_max_mid  <- green_max[, c("DateTime", "PHY_green_2.63435122780277")]

# merge datasets
compare_green_mid <- merge(green_zero_mid, green_max_mid, by = 'DateTime')
colnames(compare_green_mid) <- c('DateTime', 'green_zero', 'green_max')

# calcualte change in temperature
compare_green_mid$deltagreen <- compare_green_mid$green_max - compare_green_mid$green_zero

(green_solar_mid <- ggplot(compare_green_mid, aes(x = DateTime, y = deltagreen)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*GREEN))+
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

green_0_bottom <- get_var('output_solar0.nc',
                        var_name = 'PHY_green',
                        reference = 'surface')

green_max_bottom <- get_var('output_solarMAX.nc',
                          var_name = 'PHY_green',
                          reference = 'surface')
colnames(green_0_bottom)
colnames(green_max_bottom)
## keep only temp and date
green_zero_bottom <- green_0_bottom[, c("DateTime", "PHY_green_4.74183221004499")]
green_max_bottom  <- green_max_bottom[, c("DateTime", "PHY_green_4.74183221004499")]

# merge datasets
compare_green_bottom <- merge(green_zero_bottom, green_max_bottom, by = 'DateTime')
colnames(compare_green_bottom) <- c('DateTime', 'green_zero', 'green_max')

# calcualte change in temperature
compare_green_bottom$deltagreen <- compare_green_bottom$green_max - compare_green_bottom$green_zero

(green_solar_bottom <- ggplot(compare_green_bottom, aes(x = DateTime, y = deltagreen)) +
    geom_line(colour = 'red',linewidth = 1) +
    labs(y = expression(Delta*GREEN))+
    theme_classic())

# remove axis titles
green_solar_surface <- green_solar_surface +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

green_solar_mid <- green_solar_mid +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())
library(patchwork)

(final_plot_green <- green_solar_surface /
    green_solar_mid /
    green_solar_bottom +
    plot_annotation(
      title = "Greens Sensitivity to Solar Radiation Extremes"
    ))
ggsave(
  filename = "phs_solar.png",
  plot = final_plot_green,
  width = 8,
  height = 6,
  dpi = 600
)
# Combine your three depths into one dataframe
deltagreen_all <- data.frame(
  DateTime = compare$DateTime,
  surface = compare$deltagreen,
  middle  = compare_green_mid$deltagreen,
  bottom  = compare_green_bottom$deltagreen
)

# Compute summary per timestep
deltagreen_summary <- deltagreen_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

# Plot single summary figure
(green_solar <- ggplot(deltagreen_summary, aes(x = DateTime)) +
    geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "orange", alpha = 0.3) +
    geom_line(aes(y = delta_avg), color = "red", size = 1) +
    labs(
      y = expression(Delta*GREEN),
      x = "DateTime",
      title = "Greens Sensitivity to Solar Radiation Extremes"
    ) +
    coord_cartesian(ylim = c(-5, 75)) +
    theme_classic())


pdf('Solar Sensitivitygreen.pdf', width = 8, height = 5)
print(green_solar_surface)
print(green_solar_mid)
print(green_solar_bottom)
dev.off()

## panel for solar----
library(magick)
install.packages('magick')
# Read your PNG images
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output/sensitivity_plots/solar_delta_plots')
img1 <- image_read("temp_solar.png")
img2 <- image_read("do_solar.png")
img3 <- image_read("no3_solar.png")
img4 <- image_read("po4_solar.png")

# Combine 2x2: first combine rows, then combine rows vertically
row1 <- image_append(c(img1, img2))  # horizontal append
row2 <- image_append(c(img3, img4))  # horizontal append
panel_wq <- image_append(c(row1, row2), stack = TRUE)  # vertical append

# Save combined panel
image_write(panel, path = "wq_solar_panel.png")


## wind impact on temp----
nc <- nc_open('output_windMIN.nc')
print(nc)
names(nc$var)

wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'temp',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'temp',
                    reference = 'surface')

# check column names
colnames(wind_0)
colnames(wind_max)
## keep only phs_phs and date
wind_zero <- wind_0[, c("DateTime", "temp_0")]
wind_max  <- wind_max[, c("DateTime", "temp_0")]

# merge datasets
compare_wind <- merge(wind_0, wind_max, by = 'DateTime')
colnames(compare_wind) <- c('DateTime', 'temp_zero', 'temp_max')

# calcualte change in phs_phserature
compare_wind$deltatemp <- compare_wind$temp_max - compare_wind$temp_zero

# plot

(temp_wind_surface <- ggplot(compare_wind, aes(x = DateTime, y = deltatemp)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*T~(degree*C)))+
    theme_classic())
## CHANGE ALL RELATIVE TO TEMP !!!

## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')



wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'temp',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'temp',
                    reference = 'surface')
colnames(wind_0)
colnames(wind_max)
## keep only temp and date
wind_zero_mid <- wind_0[, c("DateTime", "temp_2.63472105263158")]
wind_max_mid  <- wind_max[, c("DateTime", "temp_2.63296309693378")]

# merge datasets
compare_temp_mid <- merge(wind_zero_mid, wind_max_mid, by = 'DateTime')
colnames(compare_temp_mid) <- c('DateTime', 'temp_zero', 'temp_max')

# calcualte change in temperature
compare_temp_mid$deltatemp <- compare_temp_mid$temp_max - compare_temp_mid$temp_zero

(temp_wind_mid <- ggplot(compare_temp_mid, aes(x = DateTime, y = deltatemp)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*T~(degree*C)))+
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

temp_0_bottom <- get_var('output_windMIN.nc',
                        var_name = 'temp',
                        reference = 'surface')

temp_max_bottom <- get_var('output_windMAX.nc',
                          var_name = 'temp',
                          reference = 'surface')
colnames(temp_0_bottom)
colnames(temp_max_bottom)
## keep only temp and date
temp_zero_bottom <- temp_0_bottom[, c("DateTime", "temp_4.74249789473684")]
temp_max_bottom  <- temp_max_bottom[, c("DateTime", "temp_4.7393335744808")]

# merge datasets
compare_temp_bottom <- merge(temp_zero_bottom, temp_max_bottom, by = 'DateTime')
colnames(compare_temp_bottom) <- c('DateTime', 'temp_zero', 'temp_max')

# calcualte change in temperature
compare_temp_bottom$deltatemp <- compare_temp_bottom$temp_max - compare_temp_bottom$temp_zero

(temp_wind_bottom <- ggplot(compare_temp_bottom, aes(x = DateTime, y = deltatemp)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*T~(degree*C)))+
    theme_classic())
# remove axis titles
temp_wind_surface <- temp_wind_surface +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

temp_wind_mid <- temp_wind_mid +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())
library(patchwork)

(final_temp_wind <- temp_wind_surface /
    temp_wind_mid /
    temp_wind_bottom +
    plot_annotation(
      title = "Thermal Sensitivity to Windspeed Extremes"
    ))
ggsave(
  filename = "thermal_wind.png",
  plot = final_temp_wind,
  width = 8,
  height = 6,
  dpi = 600
)
deltatemp_all <- data.frame(
  DateTime = compare_wind$DateTime,
  surface = compare_wind$deltatemp,
  middle  = compare_temp_mid$deltatemp,
  bottom  = compare_temp_bottom$deltatemp
)

# Compute summary per timestep
deltatemp_summary <- deltatemp_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

# Combine your three depths into one dataframe
deltatemp_all <- data.frame(
  DateTime = compare$DateTime,
  surface = compare$deltatemp,
  middle  = compare_temp_mid$deltatemp,
  bottom  = compare_temp_bottom$deltatemp
)

# Compute summary per timestep
deltatemp_summary <- deltatemp_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

# Plot single summary figure
(thermal_solar <- ggplot(deltatemp_summary, aes(x = DateTime)) +
  geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "lightblue", alpha = 0.3) +
  geom_line(aes(y = delta_avg), color = "blue", size = 1) +
  labs(
    y = expression(Delta*T~(degree*C)),
    x = "DateTime",
    title = "Thermal Sensitivity to Windspeed Extremes"
  ) +
  coord_cartesian(ylim = c(-7, 15)) +
  theme_classic())

 
## wind impact on NO3----
## get surface wind data from nc. file
wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'NIT_nit',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'NIT_nit',
                    reference = 'surface')

# check column names
colnames(wind_0)
colnames(wind_max)
## keep only NIT_nit and date
wind_zero <- wind_0[, c("DateTime", "NIT_nit_0")]
wind_max  <- wind_max[, c("DateTime", "NIT_nit_0")]

# merge datasets
compare_wind <- merge(wind_0, wind_max, by = 'DateTime')
colnames(compare_wind) <- c('DateTime', 'NIT_zero', 'NIT_max')

# calcualte change in NIT_niterature
compare_wind$deltaNIT <- compare_wind$NIT_max - compare_wind$NIT_zero

# plot

(nit_wind_surface <- ggplot(compare_wind, aes(x = DateTime, y = deltaNIT)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*NO3))+
    theme_classic())
## CHANGE ALL RELATIVE TO TEMP !!!

## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')



wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'NIT_nit',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'NIT_nit',
                    reference = 'surface')
colnames(wind_0)
colnames(wind_max)
## keep only temp and date
wind_zero_mid <- wind_0[, c("DateTime", "NIT_nit_2.63472105263158")]
wind_max_mid  <- wind_max[, c("DateTime", "NIT_nit_2.63296309693378")]

# merge datasets
compare_nit_mid <- merge(wind_zero_mid, wind_max_mid, by = 'DateTime')
colnames(compare_nit_mid) <- c('DateTime', 'NIT_zero', 'NIT_max')

# calcualte change in temperature
compare_nit_mid$deltaNIT <- compare_nit_mid$NIT_max - compare_nit_mid$NIT_zero

(nit_wind_mid <- ggplot(compare_nit_mid, aes(x = DateTime, y = deltaNIT)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*NO3))+
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

nit_0_bottom <- get_var('output_windMIN.nc',
                         var_name = 'NIT_nit',
                         reference = 'surface')

nit_max_bottom <- get_var('output_windMAX.nc',
                           var_name = 'NIT_nit',
                           reference = 'surface')
colnames(nit_0_bottom)
colnames(nit_max_bottom)
## keep only temp and date
nit_zero_bottom <- nit_0_bottom[, c("DateTime", "NIT_nit_4.74249789473684")]
nit_max_bottom  <- nit_max_bottom[, c("DateTime", "NIT_nit_4.7393335744808")]

# merge datasets
compare_nit_bottom <- merge(nit_zero_bottom, nit_max_bottom, by = 'DateTime')
colnames(compare_nit_bottom) <- c('DateTime', 'nit_zero', 'nit_max')

# calcualte change in temperature
compare_nit_bottom$deltaNIT <- compare_nit_bottom$nit_max - compare_nit_bottom$nit_zero

(nit_wind_bottom <- ggplot(compare_nit_bottom, aes(x = DateTime, y = deltaNIT)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*NO3))+
    theme_classic())

# remove axis titles
nit_wind_surface <- nit_wind_surface +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

nit_wind_mid <- nit_wind_mid +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())
library(patchwork)

(final_nit_wind <- nit_wind_surface /
    nit_wind_mid /
    nit_wind_bottom +
    plot_annotation(
      title = "Nitrate Sensitivity to Windspeed Extremes"
    ))
ggsave(
  filename = "nitrate_wind.png",
  plot = final_nit_wind,
  width = 8,
  height = 6,
  dpi = 600
)
# Combine your three depths into one dataframe
deltanit_all <- data.frame(
  DateTime = compare_wind$DateTime,
  surface = compare_wind$deltaNIT,
  middle  = compare_nit_mid$deltaNIT,
  bottom  = compare_nit_bottom$deltaNIT
)
library(dplyr)
# Compute summary per timestep
deltaNIT_summary <- deltanit_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

# Plot single summary figure
(nit_wind <- ggplot(deltaNIT_summary, aes(x = DateTime)) +
    geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "lightblue", alpha = 0.3) +
    geom_line(aes(y = delta_avg), color = "blue", size = 1) +
    labs(
      y = expression(Delta*NO3),
      x = "DateTime",
      title = "NO3 Sensitivity to Wind Speed Extremes"
    ) +
    coord_cartesian(ylim = c(-25, 150)) +
    theme_classic())

pdf("Wind_Sensitivity_Plots_NO3.pdf", width = 8, height = 5)

print(nit_wind_surface)
print(nit_wind_mid)
print(nit_wind_bottom)

dev.off()
## wind speed effect on PO4----
wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'PHS_frp',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'PHS_frp',
                    reference = 'surface')


# check column names
colnames(wind_0)
colnames(wind_max)

## keep only phs_phs and date
wind_zero <- wind_0[, c("DateTime", "PHS_frp_0")]
wind_max  <- wind_max[, c("DateTime", "PHS_frp_0")]

# merge datasets
compare_wind <- merge(wind_0, wind_max, by = 'DateTime')
colnames(compare_wind) <- c('DateTime', 'phs_zero', 'phs_max')

# calcualte change in phs_phserature
compare_wind$deltaphs <- compare_wind$phs_max - compare_wind$phs_zero

# plot

(phs_wind_surface <- ggplot(compare_wind, aes(x = DateTime, y = deltaphs)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*PO4))+
    theme_classic())
## CHANGE ALL RELATIVE TO TEMP !!!

## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')



wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'PHS_frp',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'PHS_frp',
                    reference = 'surface')
colnames(wind_0)
colnames(wind_max)
## keep only temp and date
wind_zero_mid <- wind_0[, c("DateTime", "PHS_frp_2.63472105263158")]
wind_max_mid  <- wind_max[, c("DateTime", "PHS_frp_2.63296309693378")]

# merge datasets
compare_phs_mid <- merge(wind_zero_mid, wind_max_mid, by = 'DateTime')
colnames(compare_phs_mid) <- c('DateTime', 'phs_zero', 'phs_max')

# calcualte change in temperature
compare_phs_mid$deltaphs <- compare_phs_mid$phs_max - compare_phs_mid$phs_zero

(phs_wind_mid <- ggplot(compare_phs_mid, aes(x = DateTime, y = deltaphs)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*PO4))+
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

phs_0_bottom <- get_var('output_windMIN.nc',
                        var_name = 'PHS_frp',
                        reference = 'surface')

phs_max_bottom <- get_var('output_windMAX.nc',
                          var_name = 'PHS_frp',
                          reference = 'surface')
colnames(phs_0_bottom)
colnames(phs_max_bottom)
## keep only temp and date
phs_zero_bottom <- phs_0_bottom[, c("DateTime", "PHS_frp_4.74249789473684")]
phs_max_bottom  <- phs_max_bottom[, c("DateTime", "PHS_frp_4.7393335744808")]

# merge datasets
compare_phs_bottom <- merge(phs_zero_bottom, phs_max_bottom, by = 'DateTime')
colnames(compare_phs_bottom) <- c('DateTime', 'phs_zero', 'phs_max')

# calcualte change in temperature
compare_phs_bottom$deltaphs <- compare_phs_bottom$phs_max - compare_phs_bottom$phs_zero

(phs_wind_bottom <- ggplot(compare_phs_bottom, aes(x = DateTime, y = deltaphs)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*PO4))+
    theme_classic())

# remove axis titles
phs_wind_surface <- phs_wind_surface +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

phs_wind_mid <- phs_wind_mid +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())
library(patchwork)

(final_phs_wind <- phs_wind_surface /
    phs_wind_mid /
    phs_wind_bottom +
    plot_annotation(
      title = "Phosphate Sensitivity to Windspeed Extremes"
    ))
ggsave(
  filename = "phs_wind.png",
  plot = final_phs_wind,
  width = 8,
  height = 6,
  dpi = 600
)
# Combine your three depths into one dataframe
deltaphs_all <- data.frame(
  DateTime = compare_wind$DateTime,
  surface = compare_wind$deltaphs,
  middle  = compare_phs_mid$deltaphs,
  bottom  = compare_phs_bottom$deltaphs
)

# Compute summary per timestep
deltaphs_summary <- deltaphs_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

# Plot single summary figure
(phs_wind <- ggplot(deltaphs_summary, aes(x = DateTime)) +
    geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "lightblue", alpha = 0.3) +
    geom_line(aes(y = delta_avg), color = "blue", size = 1) +
    labs(
      y = expression(Delta*PO4),
      x = "DateTime",
      title = "PO4 Sensitivity to Windspeed Extremes"
    ) +
    coord_cartesian(ylim = c(-2, 1)) +
    theme_classic())

pdf("Wind_Sensitivity_Plots_PO4.pdf", width = 8, height = 5)

print(phs_wind_surface)
print(phs_wind_mid)
print(phs_wind_bottom)
dev.off()

## wind speed effect on do----
wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'OXY_oxy',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'OXY_oxy',
                    reference = 'surface')

# check column names
colnames(wind_0)
colnames(wind_max)
## keep only phs_phs and date
wind_zero <- wind_0[, c("DateTime", "OXY_oxy_0")]
wind_max  <- wind_max[, c("DateTime", "OXY_oxy_0")]

# merge datasets
compare_wind <- merge(wind_0, wind_max, by = 'DateTime')
colnames(compare_wind) <- c('DateTime', 'ox_zero', 'ox_max')

# calcualte change in phs_phserature
compare_wind$deltaox <- compare_wind$ox_max - compare_wind$ox_zero


# plot

(ox_wind_surface <- ggplot(compare_wind, aes(x = DateTime, y = deltaox)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*DO))+
    theme_classic())
## CHANGE ALL RELATIVE TO TEMP !!!

## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')



wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'OXY_oxy',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'OXY_oxy',
                    reference = 'surface')
colnames(wind_0)
colnames(wind_max)
## keep only temp and date
wind_zero_mid <- wind_0[, c("DateTime", "OXY_oxy_2.63472105263158")]
wind_max_mid  <- wind_max[, c("DateTime", "OXY_oxy_2.63296309693378")]

# merge datasets
compare_ox_mid <- merge(wind_zero_mid, wind_max_mid, by = 'DateTime')
colnames(compare_ox_mid) <- c('DateTime', 'ox_zero', 'ox_max')

# calcualte change in temperature
compare_ox_mid$deltaox <- compare_ox_mid$ox_max - compare_ox_mid$ox_zero

(oz_wind_mid <- ggplot(compare_ox_mid, aes(x = DateTime, y = deltaox)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*DO))+
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

ox_0_bottom <- get_var('output_windMIN.nc',
                        var_name = 'OXY_oxy',
                        reference = 'surface')

ox_max_bottom <- get_var('output_windMAX.nc',
                          var_name = 'OXY_oxy',
                          reference = 'surface')
colnames(ox_0_bottom)
colnames(ox_max_bottom)
## keep only temp and date
ox_zero_bottom <- ox_0_bottom[, c("DateTime", "OXY_oxy_4.74249789473684")]
ox_max_bottom  <- ox_max_bottom[, c("DateTime", "OXY_oxy_4.7393335744808")]

# merge datasets
compare_ox_bottom <- merge(ox_zero_bottom, ox_max_bottom, by = 'DateTime')
colnames(compare_ox_bottom) <- c('DateTime', 'ox_zero', 'ox_max')

# calcualte change in temperature
compare_ox_bottom$deltaox <- compare_ox_bottom$ox_max - compare_ox_bottom$ox_zero

(ox_wind_bottom <- ggplot(compare_ox_bottom, aes(x = DateTime, y = deltaox)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*DO))+
    theme_classic())
# remove axis titles
ox_wind_surface <- ox_wind_surface +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

oz_wind_mid <- oz_wind_mid +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())
library(patchwork)

(final_ox_wind <- ox_wind_surface /
    oz_wind_mid /
    ox_wind_bottom +
    plot_annotation(
      title = "Dissolved oxygen Sensitivity to Windspeed Extremes"
    ))
ggsave(
  filename = "do_wind.png",
  plot = final_ox_wind,
  width = 8,
  height = 6,
  dpi = 600
)

# Combine your three depths into one dataframe
deltaox_all <- data.frame(
  DateTime = compare_wind$DateTime,
  surface = compare_wind$deltaox,
  middle  = compare_ox_mid$deltaox,
  bottom  = compare_ox_bottom$deltaox
)

# Compute summary per timestep
deltaox_summary <- deltaox_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

# Plot single summary figure
(ox_wind <- ggplot(deltaox_summary, aes(x = DateTime)) +
    geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "lightblue", alpha = 0.3) +
    geom_line(aes(y = delta_avg), color = "blue", size = 1) +
    labs(
      y = expression(Delta*DO),
      x = "DateTime",
      title = "DO Sensitivity to Windspeed Extremes"
    ) +
    coord_cartesian(ylim = c(-1, 400)) +
    theme_classic())


pdf("Wind_Sensitivity_Plots_PO4.pdf", width = 8, height = 5)

print(phs_wind_surface)
print(phs_wind_mid)
print(phs_wind_bottom)
dev.off()
## wind speed effect on cyano----
wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'PHY_cyno',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'PHY_cyno',
                    reference = 'surface')

# check column names
colnames(wind_0)
colnames(wind_max)
## keep only phs_phs and date
wind_zero <- wind_0[, c("DateTime", "PHY_cyno_0")]
wind_max  <- wind_max[, c("DateTime", "PHY_cyno_0")]

# merge datasets
compare_wind <- merge(wind_0, wind_max, by = 'DateTime')
colnames(compare_wind) <- c('DateTime', 'cyno_zero', 'cyno_max')

# calcualte change in phs_phserature
compare_wind$deltacyno <- compare_wind$cyno_max - compare_wind$cyno_zero

# plot

(cyno_wind_surface <- ggplot(compare_wind, aes(x = DateTime, y = deltacyno)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*CYNO))+
    theme_classic())
## CHANGE ALL RELATIVE TO TEMP !!!

## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')



wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'PHY_cyno',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'PHY_cyno',
                    reference = 'surface')
colnames(wind_0)
colnames(wind_max)
## keep only temp and date
wind_zero_mid <- wind_0[, c("DateTime", "PHY_cyno_2.63472105263158")]
wind_max_mid  <- wind_max[, c("DateTime", "PHY_cyno_2.63296309693378")]

# merge datasets
compare_cyno_mid <- merge(wind_zero_mid, wind_max_mid, by = 'DateTime')
colnames(compare_cyno_mid) <- c('DateTime', 'cyno_zero', 'cyno_max')

# calcualte change in temperature
compare_cyno_mid$deltacyno <- compare_cyno_mid$cyno_max - compare_cyno_mid$cyno_zero

(cyno_wind_mid <- ggplot(compare_cyno_mid, aes(x = DateTime, y = deltacyno)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*CYNO))+
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

cyno_0_bottom <- get_var('output_windMIN.nc',
                        var_name = 'PHY_cyno',
                        reference = 'surface')

cyno_max_bottom <- get_var('output_windMAX.nc',
                          var_name = 'PHY_cyno',
                          reference = 'surface')
colnames(cyno_0_bottom)
colnames(cyno_max_bottom)
## keep only temp and date
cyno_zero_bottom <- cyno_0_bottom[, c("DateTime", "PHY_cyno_4.74249789473684")]
cyno_max_bottom  <- cyno_max_bottom[, c("DateTime", "PHY_cyno_4.7393335744808")]

# merge datasets
compare_cyno_bottom <- merge(cyno_zero_bottom, cyno_max_bottom, by = 'DateTime')
colnames(compare_cyno_bottom) <- c('DateTime', 'cyno_zero', 'cyno_max')

# calcualte change in temperature
compare_cyno_bottom$deltacyno <- compare_cyno_bottom$cyno_max - compare_cyno_bottom$cyno_zero

(cyno_wind_bottom <- ggplot(compare_cyno_bottom, aes(x = DateTime, y = deltacyno)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*CYNO))+
    theme_classic())


# remove axis titles
cyno_wind_surface <- cyno_wind_surface +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

cyno_wind_mid <- cyno_wind_mid +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())
library(patchwork)

(final_cyno_wind <- cyno_wind_surface /
    cyno_wind_mid /
    cyno_wind_bottom +
    plot_annotation(
      title = "Cynobacteria Sensitivity to Windspeed Extremes"
    ))
ggsave(
  filename = "cyno_wind.png",
  plot = final_cyno_wind,
  width = 8,
  height = 6,
  dpi = 600
)

# Combine your three depths into one dataframe
deltacyno_all <- data.frame(
  DateTime = compare_wind$DateTime,
  surface = compare_wind$deltacyno,
  middle  = compare_cyno_mid$deltacyno,
  bottom  = compare_cyno_bottom$deltacyno
)

# Compute summary per timestep
deltacyno_summary <- deltacyno_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

# Plot single summary figure
(cyno_wind <- ggplot(deltacyno_summary, aes(x = DateTime)) +
    geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "lightblue", alpha = 0.3) +
    geom_line(aes(y = delta_avg), color = "blue", size = 1) +
    labs(
      y = expression(Delta*CYNO),
      x = "DateTime",
      title = "Cyno Sensitivity to Windspeed Extremes"
    ) +
    coord_cartesian(ylim = c(-1, 1)) +
    theme_classic())


pdf("Wind_Sensitivity_Plots_CYNO.pdf", width = 8, height = 5)

print(cyno_wind_surface)
print(cyno_wind_mid)
print(cyno_wind_bottom)
dev.off()
## wind speed effect on diatoms---- 
wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'PHY_diat',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'PHY_diat',
                    reference = 'surface')

# check column names
colnames(wind_0)
colnames(wind_max)
## keep only phs_phs and date
wind_zero <- wind_0[, c("DateTime", "PHY_diat_0")]
wind_max  <- wind_max[, c("DateTime", "PHY_diat_0")]

# merge datasets
compare_wind <- merge(wind_0, wind_max, by = 'DateTime')
colnames(compare_wind) <- c('DateTime', 'diat_zero', 'diat_max')

# calcualte change in phs_phserature
compare_wind$deltadiat <- compare_wind$diat_max - compare_wind$diat_zero

# plot

(diat_wind_surface <- ggplot(compare_wind, aes(x = DateTime, y = deltadiat)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*DIAT))+
    theme_classic())
## CHANGE ALL RELATIVE TO TEMP !!!

## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')



wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'PHY_diat',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'PHY_diat',
                    reference = 'surface')
colnames(wind_0)
colnames(wind_max)
## keep only temp and date
wind_zero_mid <- wind_0[, c("DateTime", "PHY_diat_2.63472105263158")]
wind_max_mid  <- wind_max[, c("DateTime", "PHY_diat_2.63296309693378")]

# merge datasets
compare_diat_mid <- merge(wind_zero_mid, wind_max_mid, by = 'DateTime')
colnames(compare_diat_mid) <- c('DateTime', 'diat_zero', 'diat_max')

# calcualte change in temperature
compare_diat_mid$deltadiat <- compare_diat_mid$diat_max - compare_diat_mid$diat_zero

(diat_wind_mid <- ggplot(compare_diat_mid, aes(x = DateTime, y = deltadiat)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*DIAT))+
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

diat_0_bottom <- get_var('output_windMIN.nc',
                         var_name = 'PHY_diat',
                         reference = 'surface')

diat_max_bottom <- get_var('output_windMAX.nc',
                           var_name = 'PHY_diat',
                           reference = 'surface')
colnames(diat_0_bottom)
colnames(diat_max_bottom)
## keep only temp and date
diat_zero_bottom <- diat_0_bottom[, c("DateTime", "PHY_diat_4.74249789473684")]
diat_max_bottom  <- diat_max_bottom[, c("DateTime", "PHY_diat_4.7393335744808")]

# merge datasets
compare_diat_bottom <- merge(diat_zero_bottom, diat_max_bottom, by = 'DateTime')
colnames(compare_diat_bottom) <- c('DateTime', 'diat_zero', 'diat_max')

# calcualte change in temperature
compare_diat_bottom$deltadiat <- compare_diat_bottom$diat_max - compare_diat_bottom$diat_zero

(diat_wind_bottom <- ggplot(compare_diat_bottom, aes(x = DateTime, y = deltadiat)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*DIAT))+
    theme_classic())

# remove axis titles
diat_wind_surface <- diat_wind_surface +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

diat_wind_mid <- diat_wind_mid +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())
library(patchwork)

(final_diat_wind <- diat_wind_surface /
    diat_wind_mid /
    diat_wind_bottom +
    plot_annotation(
      title = "Diatom Sensitivity to Windspeed Extremes"
    ))
ggsave(
  filename = "diat_wind.png",
  plot = final_diat_wind,
  width = 8,
  height = 6,
  dpi = 600
)


# Combine your three depths into one dataframe
deltadiat_all <- data.frame(
  DateTime = compare_wind$DateTime,
  surface = compare_wind$deltadiat,
  middle  = compare_diat_mid$deltadiat,
  bottom  = compare_diat_bottom$deltadiat
)

# Compute summary per timestep
deltadiat_summary <- deltadiat_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

(diat_wind <- ggplot(deltadiat_summary, aes(x = DateTime)) +
    geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "lightblue", alpha = 0.3) +
    geom_line(aes(y = delta_avg), color = "blue", size = 1) +
    labs(
      y = expression(Delta*DIAT),
      x = "DateTime",
      title = "Diat Sensitivity to Windspeed Extremes"
    ) +
    coord_cartesian(ylim = c(-100, 2)) +
    theme_classic())

pdf("Wind_Sensitivity_Plots_diat.pdf", width = 8, height = 5)

print(diat_wind_surface)
print(diat_wind_mid)
print(diat_wind_bottom)
dev.off()
## windspeed effect on bga----
library(dplyr)
library(ncdf4)
library(ggplot2)
library(glmtools)
wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'PHY_bga',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'PHY_bga',
                    reference = 'surface')


# check column names
colnames(wind_0)
colnames(wind_max)
## keep only phs_phs and date
wind_zero <- wind_0[, c("DateTime", "PHY_bga_0")]
wind_max  <- wind_max[, c("DateTime", "PHY_bga_0")]

# merge datasets
compare_wind <- merge(wind_0, wind_max, by = 'DateTime')
colnames(compare_wind) <- c('DateTime', 'bga_zero', 'bga_max')

# calcualte change in phs_phserature
compare_wind$deltabga <- compare_wind$bga_max - compare_wind$bga_zero

# plot

(bga_wind_surface <- ggplot(compare_wind, aes(x = DateTime, y = deltabga)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*BGA))+
    theme_classic())
## CHANGE ALL RELATIVE TO TEMP !!!

## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')



wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'PHY_bga',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'PHY_bga',
                    reference = 'surface')
colnames(wind_0)
colnames(wind_max)
## keep only temp and date
wind_zero_mid <- wind_0[, c("DateTime", "PHY_bga_2.63472105263158")]
wind_max_mid  <- wind_max[, c("DateTime", "PHY_bga_2.63296309693378")]

# merge datasets
compare_bga_mid <- merge(wind_zero_mid, wind_max_mid, by = 'DateTime')
colnames(compare_bga_mid) <- c('DateTime', 'bga_zero', 'bga_max')

# calcualte change in temperature
compare_bga_mid$deltabga <- compare_bga_mid$bga_max - compare_bga_mid$bga_zero

(bga_wind_mid <- ggplot(compare_bga_mid, aes(x = DateTime, y = deltabga)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*BGA))+
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

bga_0_bottom <- get_var('output_windMIN.nc',
                         var_name = 'PHY_bga',
                         reference = 'surface')

bga_max_bottom <- get_var('output_windMAX.nc',
                           var_name = 'PHY_bga',
                           reference = 'surface')
colnames(bga_0_bottom)
colnames(bga_max_bottom)
## keep only temp and date
bga_zero_bottom <- bga_0_bottom[, c("DateTime", "PHY_bga_4.74249789473684")]
bga_max_bottom  <- bga_max_bottom[, c("DateTime", "PHY_bga_4.7393335744808")]

# merge datasets
compare_bga_bottom <- merge(bga_zero_bottom, bga_max_bottom, by = 'DateTime')
colnames(compare_bga_bottom) <- c('DateTime', 'bga_zero', 'bga_max')

# calcualte change in temperature
compare_bga_bottom$deltabga <- compare_bga_bottom$bga_max - compare_bga_bottom$bga_zero

(bga_wind_bottom <- ggplot(compare_bga_bottom, aes(x = DateTime, y = deltabga)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*BGA))+
    theme_classic())

# remove axis titles
bga_wind_surface <- bga_wind_surface +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

bga_wind_mid <- bga_wind_mid +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())
library(patchwork)

(final_bga_wind <- bga_wind_surface /
    bga_wind_mid /
    bga_wind_bottom +
    plot_annotation(
      title = "Blue Green Algae Sensitivity to Windspeed Extremes"
    ))
ggsave(
  filename = "bga_wind.png",
  plot = final_bga_wind,
  width = 8,
  height = 6,
  dpi = 600
)

deltabga_all <- data.frame(
  DateTime = compare_wind$DateTime,
  surface = compare_wind$deltabga,
  middle  = compare_bga_mid$deltabga,
  bottom  = compare_bga_bottom$deltabga
)

# Compute summary per timestep
deltabga_summary <- deltabga_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

(bga_wind <- ggplot(deltabga_summary, aes(x = DateTime)) +
    geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "lightblue", alpha = 0.3) +
    geom_line(aes(y = delta_avg), color = "blue", size = 1) +
    labs(
      y = expression(Delta*BGA),
      x = "DateTime",
      title = "Blue Green algae Sensitivity to Windspeed Extremes"
    ) +
    coord_cartesian(ylim = c(-3, 1)) +
    theme_classic())

pdf("Wind_Sensitivity_Plots_bga.pdf", width = 8, height = 5)

print(bga_wind_surface)
print(bga_wind_mid)
print(bga_wind_bottom)
dev.off()
## wind speed effect on greens----
wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'PHY_green',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'PHY_green',
                    reference = 'surface')

# check column names
colnames(wind_0)
colnames(wind_max)
## keep only phs_phs and date
wind_zero <- wind_0[, c("DateTime", "PHY_green_0")]
wind_max  <- wind_max[, c("DateTime", "PHY_green_0")]

# merge datasets
compare_wind <- merge(wind_0, wind_max, by = 'DateTime')
colnames(compare_wind) <- c('DateTime', 'green_zero', 'green_max')

# calcualte change in phs_phserature
compare_wind$deltagreen <- compare_wind$green_max - compare_wind$green_zero

# plot

(green_wind_surface <- ggplot(compare_wind, aes(x = DateTime, y = deltagreen)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*GREEN))+
    theme_classic())
## CHANGE ALL RELATIVE TO TEMP !!!

## now for middle of loch
# CLEAR ENVIRONMENT
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')



wind_0 <- get_var('output_windMIN.nc',
                  var_name = 'PHY_green',
                  reference = 'surface')

wind_max <- get_var('output_windMAX.nc',
                    var_name = 'PHY_green',
                    reference = 'surface')
colnames(wind_0)
colnames(wind_max)
## keep only temp and date
wind_zero_mid <- wind_0[, c("DateTime", "PHY_green_2.63472105263158")]
wind_max_mid  <- wind_max[, c("DateTime", "PHY_green_2.63296309693378")]

# merge datasets
compare_green_mid <- merge(wind_zero_mid, wind_max_mid, by = 'DateTime')
colnames(compare_green_mid) <- c('DateTime', 'green_zero', 'green_max')

# calcualte change in temperature
compare_green_mid$deltagreen <- compare_green_mid$green_max - compare_green_mid$green_zero

(green_wind_mid <- ggplot(compare_green_mid, aes(x = DateTime, y = deltagreen)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*GREEN))+
    theme_classic())

# bottom of loch
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')

green_0_bottom <- get_var('output_windMIN.nc',
                         var_name = 'PHY_green',
                         reference = 'surface')

green_max_bottom <- get_var('output_windMAX.nc',
                           var_name = 'PHY_green',
                           reference = 'surface')
colnames(green_0_bottom)
colnames(green_max_bottom)
## keep only temp and date
green_zero_bottom <- green_0_bottom[, c("DateTime", "PHY_green_4.74249789473684")]
green_max_bottom  <- green_max_bottom[, c("DateTime", "PHY_green_4.7393335744808")]

# merge datasets
compare_green_bottom <- merge(green_zero_bottom, green_max_bottom, by = 'DateTime')
colnames(compare_green_bottom) <- c('DateTime', 'green_zero', 'green_max')

# calcualte change in temperature
compare_green_bottom$deltagreen <- compare_green_bottom$green_max - compare_green_bottom$green_zero

(green_wind_bottom <- ggplot(compare_green_bottom, aes(x = DateTime, y = deltagreen)) +
    geom_line(colour = 'blue',linewidth = 1) +
    labs(y = expression(Delta*GREEN))+
    theme_classic())

# remove axis titles
green_wind_surface <- green_wind_surface +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

green_wind_mid <- green_wind_mid +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())
library(patchwork)

(final_green_wind <- green_wind_surface /
    green_wind_mid /
    green_wind_bottom +
    plot_annotation(
      title = "Green Algae Sensitivity to Windspeed Extremes"
    ))
ggsave(
  filename = "greens_wind.png",
  plot = final_green_wind,
  width = 8,
  height = 6,
  dpi = 600
)

# Combine your three depths into one dataframe
deltagreen_all <- data.frame(
  DateTime = compare_wind$DateTime,
  surface = compare_wind$deltagreen,
  middle  = compare_green_mid$deltagreen,
  bottom  = compare_green_bottom$deltagreen
)

# Compute summary per timestep
deltagreen_summary <- deltagreen_all %>%
  rowwise() %>%
  mutate(
    delta_avg = mean(c(surface, middle, bottom), na.rm = TRUE),
    delta_min = min(c(surface, middle, bottom), na.rm = TRUE),
    delta_max = max(c(surface, middle, bottom), na.rm = TRUE)
  )

(green_wind <- ggplot(deltagreen_summary, aes(x = DateTime)) +
    geom_ribbon(aes(ymin = delta_min, ymax = delta_max), fill = "lightblue", alpha = 0.3) +
    geom_line(aes(y = delta_avg), color = "blue", size = 1) +
    labs(
      y = expression(Delta*DIAT),
      x = "DateTime",
      title = "Greens Sensitivity to Windspeed Extremes"
    ) +
    coord_cartesian(ylim = c(-75, 75)) +
    theme_classic())


pdf("Wind_Sensitivity_Plots_green.pdf", width = 8, height = 5)

print(green_wind_surface)
print(green_wind_mid)
print(green_wind_bottom)
dev.off()


install.packages('pdftools')
library(pdftools)
pdf_combine(
  input = c("Solar Sensitivity NIT.pdf",
            "Solar Sensitivity Temp.pdf",
            "Solar SensitivityBGA.pdf",
            "Solar SensitivityCYNO.pdf",
            "Solar SensitivityDiat.pdf",
            "Solar SensitivityPO4.pdf",
            "Solar Sensitivitygreen.pdf",
            "Wind_Sensitivity_Plots_Temp.pdf",
            "Wind_Sensitivity_Plots_NO3.pdf",
            "Wind_Sensitivity_Plots_PO4.pdf",
            "Wind_Sensitivity_Plots_CYNO.pdf",
            "Wind_Sensitivity_Plots_diat.pdf",
            "Wind_Sensitivity_Plots_bga.pdf",
            "Wind_Sensitivity_Plots_green.pdf"),

  output = "All_Sensitivity_Results.pdf"
)

 
## panel for windspeed----
library(magick)
# Read your PNG images
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')
img5 <- image_read("temp_wind.png")
img6 <- image_read("do_wind.png")
img7 <- image_read("no3_wind.png")
img8 <- image_read("po4_wind.png")

# Combine 2x2: first combine rows, then combine rows vertically
row3 <- image_append(c(img5, img6))  # horizontal append
row4 <- image_append(c(img7, img8))  # horizontal append
panel2_wq <- image_append(c(row3, row4), stack = TRUE)  # vertical append

# Save combined panel
image_write(pane2l_wq, path = "wq_wind_panel.png")

## panel for solar phytoplankton----
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output')
img9 <- image_read("sensitivity_plots/solar_delta_plots/bga_solar.png")
img10 <- image_read("sensitivity_plots/solar_delta_plots/cyno_solar.png")
img11 <- image_read("sensitivity_plots/solar_delta_plots/diat_solar.png")
img12 <- image_read("sensitivity_plots/solar_delta_plots/greens_solar.png")

# Combine 2x2: first combine rows, then combine rows vertically
row5 <- image_append(c(img9, img10))  # horizontal append
row6 <- image_append(c(img11, img12))  # horizontal append
panel_phyto_solar <- image_append(c(row5, row6), stack = TRUE)  # vertical append

# Save combined panel
image_write(panel_phyto_solar, path = "phyto_solar_panel.png")

## panel for phytoplankton (wind)----
img13 <- image_read("sensitivity_plots/wind_delta_plots/bga_wind.png")
img14 <- image_read("sensitivity_plots/wind_delta_plots/cyno_wind.png")
img15 <- image_read("sensitivity_plots/wind_delta_plots/diat_wind.png")
img16 <- image_read("sensitivity_plots/wind_delta_plots/greens_wind.png")

# Combine 2x2: first combine rows, then combine rows vertically
row7 <- image_append(c(img13, img14))  # horizontal append
row8 <- image_append(c(img15, img16))  # horizontal append
panel_phyto_wind <- image_append(c(row7, row8), stack = TRUE)  # vertical append

# Save combined panel
image_write(panel_phyto_wind, path = "phyto_wind_panel.png")
