## code for delta values and plots to analyse sensitivity----

## solar_temp----
## load libraries
library(ncdf4)
library(ggplot2)
library(glmtools)
library(dplyr)
library(patchwork)

setwd("C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output")

solar_min <- 0.5     # min solar
solar_max <- 250   # max solar
solar_range <- solar_max - solar_min

temp_min <- get_var("output_solar0.nc", var_name = "temp", reference = "surface")
temp_max <- get_var("output_solarMAX.nc", var_name = "temp", reference = "surface")

calc_deltaT <- function(depth_col) {
  
  df_min <- temp_min[, c("DateTime", depth_col)]
  df_max <- temp_max[, c("DateTime", depth_col)]
  
  colnames(df_min)[2] <- "T_min"
  colnames(df_max)[2] <- "T_max"
  
  df <- merge(df_min, df_max, by = "DateTime")
  
  df$deltaT <- df$T_max - df$T_min
  
  # Sensitivity coefficient
  df$sensitivity <- df$deltaT / solar_range
  
  return(df)
}

# apply to each depth
surface <- calc_deltaT("temp_0")
middle  <- calc_deltaT("temp_2.63435122780277")
bottom  <- calc_deltaT("temp_4.74183221004499")

delta_all <- data.frame(
  DateTime = surface$DateTime,
  surface = surface$deltaT,
  middle  = middle$deltaT,
  bottom  = bottom$deltaT
)

# calculat mean delta values for surface middle and bottom depths
(delta_temp_surface <- mean((surface$deltaT), na.rm = TRUE))
(delta_temp_middle  <- mean((middle$deltaT), na.rm = TRUE))
(delta_temp_bottom <- mean((bottom$deltaT), na.rm = TRUE))


# seasonal 
library(lubridate)
library(dplyr)

add_season <- function(df){
  df %>%
    mutate(
      month = month(DateTime),
      season = case_when(
        month %in% c(12,1,2)  ~ "Winter",
        month %in% c(3,4,5)   ~ "Spring",
        month %in% c(6,7,8)   ~ "Summer",
        month %in% c(9,10,11) ~ "Autumn"
      )
    )
}

surface <- add_season(surface)
middle  <- add_season(middle)
bottom  <- add_season(bottom)

# calculate mean delta for seasons
(seasonal_delta_surface <- surface %>%
  group_by(season) %>%
  summarise(mean_deltaT = mean(deltaT, na.rm = TRUE)))

(seasonal_delta_middle <- middle %>%
  group_by(season) %>%
  summarise(mean_deltaT = mean(deltaT, na.rm = TRUE)))

(seasonal_delta_bottom <- bottom %>%
  group_by(season) %>%
  summarise(mean_deltaT = mean(deltaT, na.rm = TRUE)))


# plot annual trends
library(ggplot2)
library(patchwork)
# Example with fixed y-axis
y_range <- range(c(surface$deltaT, middle$deltaT, bottom$deltaT), na.rm = TRUE)

p_surface <- ggplot(surface, aes(x = DateTime, y = deltaT)) +
  geom_line(color = 'deeppink1', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Surface", x = NULL, y = NULL)

p_middle <- ggplot(middle, aes(x = DateTime, y = deltaT)) +
  geom_line(color = "maroon", size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Middle", x = NULL, y = "ΔTemp (°C)")

p_bottom <- ggplot(bottom, aes(x = DateTime, y = deltaT)) +
  geom_line(color = "darkred", size = 1) +
  ylim(y_range) +
  theme_classic() +
  labs(title = "Bottom", x = "Date", y = NULL)

  (p_surface / p_middle / p_bottom) +
  plot_annotation(
    title = "Thermal Sensitivity to Shortwave Radiation Extremes")

## solar_no3----
solar_min <- 0.5     # min solar
solar_max <- 250   # max solar
solar_range <- solar_max - solar_min
nit_min <- get_var("output_solar0.nc", var_name = "NIT_nit", reference = "surface")
nit_max <- get_var("output_solarMAX.nc", var_name = "NIT_nit", reference = "surface")

calc_deltanit <- function(depth_col) {
  
  df_min <- nit_min[, c("DateTime", depth_col)]
  df_max <- nit_max[, c("DateTime", depth_col)]
  
  colnames(df_min)[2] <- "nit_min"
  colnames(df_max)[2] <- "nit_max"
  
  df <- merge(df_min, df_max, by = "DateTime")
  
  df$deltanit <- df$nit_max - df$nit_min
  
  # Sensitivity coefficient
  df$sensitivity <- df$deltanit / solar_range
  
  return(df)
}

# apply to each depth
surface <- calc_deltanit("NIT_nit_0")
middle  <- calc_deltanit("NIT_nit_2.63435122780277")
bottom  <- calc_deltanit("NIT_nit_4.74183221004499")

delta_all <- data.frame(
  DateTime = surface$DateTime,
  surface = surface$deltanit,
  middle  = middle$deltanit,
  bottom  = bottom$deltanit
)
(delta_nit_surface <- mean((surface$deltanit), na.rm = TRUE))
(delta_nit_middle  <- mean((middle$deltanit), na.rm = TRUE))
(delta_nit_bottom <- mean((bottom$deltanit), na.rm = TRUE))



## add for season
add_season <- function(df){
  df %>%
    mutate(
      month = month(DateTime),
      season = case_when(
        month %in% c(12,1,2)  ~ "Winter",
        month %in% c(3,4,5)   ~ "Spring",
        month %in% c(6,7,8)   ~ "Summer",
        month %in% c(9,10,11) ~ "Autumn"
      )
    )
}

surface <- add_season(surface)
middle  <- add_season(middle)
bottom  <- add_season(bottom)

# calculate delta for seasons
(seasonal_delta_surface <- surface %>%
    group_by(season) %>%
    summarise(mean_deltanit = mean(deltanit, na.rm = TRUE)))

(seasonal_delta_middle <- middle %>%
    group_by(season) %>%
    summarise(mean_deltanit = mean(deltanit, na.rm = TRUE)))

(seasonal_delta_bottom <- bottom %>%
    group_by(season) %>%
    summarise(mean_deltanit = mean(deltanit, na.rm = TRUE)))


# plot
library(ggplot2)
library(patchwork)
# Example with fixed y-axis
y_range <- range(c(surface$deltanit, middle$deltanit, bottom$deltanit), na.rm = TRUE)

nit_solar_surface <- ggplot(surface, aes(x = DateTime, y = deltanit)) +
  geom_line(color = 'deeppink1', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Surface", x = NULL, y = NULL)

nit_solar_middle <- ggplot(middle, aes(x = DateTime, y = deltanit)) +
  geom_line(color = "maroon", size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Middle", x = NULL, y = "ΔNO3 (mmol/m³)")

nit_solar_bottom <- ggplot(bottom, aes(x = DateTime, y = deltanit)) +
  geom_line(color = "darkred", size = 1) +
  ylim(y_range) +
  theme_classic() +
  labs(title = "Bottom", x = "Date", y = NULL)

(nit_solar_surface / nit_solar_middle / nit_solar_bottom) +
  plot_annotation(
    title = "Nitrate Sensitivity to Shortwave Radiation Extremes")

## solar_phs----
solar_min <- 0.5     # min solar
solar_max <- 250   # max solar
solar_range <- solar_max - solar_min
phs_min <- get_var("output_solar0.nc", var_name = "PHS_frp", reference = "surface")
phs_max <- get_var("output_solarMAX.nc", var_name = "PHS_frp", reference = "surface")

calc_deltaphs <- function(depth_col) {
  
  df_min <- phs_min[, c("DateTime", depth_col)]
  df_max <- phs_max[, c("DateTime", depth_col)]
  
  colnames(df_min)[2] <- "phs_min"
  colnames(df_max)[2] <- "phs_max"
  
  df <- merge(df_min, df_max, by = "DateTime")
  
  df$deltaphs <- df$phs_max - df$phs_min
  
  # Sensitivity coefficient
  df$sensitivity <- df$deltaphs / solar_range
  
  return(df)
}

# apply to each depth
surface <- calc_deltaphs("PHS_frp_0")
middle  <- calc_deltaphs("PHS_frp_2.63435122780277")
bottom  <- calc_deltaphs("PHS_frp_4.74183221004499")

delta_all <- data.frame(
  DateTime = surface$DateTime,
  surface = surface$deltaphs,
  middle  = middle$deltaphs,
  bottom  = bottom$deltaphs
)

# calculate delta
(delta_phs_surface <- mean((surface$deltaphs), na.rm = TRUE))
(delta_phs_middle  <- mean((middle$deltaphs), na.rm = TRUE))
(delta_phs_bottom <- mean((bottom$deltaphs), na.rm = TRUE))

# add for season
add_season <- function(df){
  df %>%
    mutate(
      month = month(DateTime),
      season = case_when(
        month %in% c(12,1,2)  ~ "Winter",
        month %in% c(3,4,5)   ~ "Spring",
        month %in% c(6,7,8)   ~ "Summer",
        month %in% c(9,10,11) ~ "Autumn"
      )
    )
}

surface <- add_season(surface)
middle  <- add_season(middle)
bottom  <- add_season(bottom)

# calculate delta for seasons
(seasonal_delta_surface <- surface %>%
    group_by(season) %>%
    summarise(mean_deltaphs = mean(deltaphs, na.rm = TRUE)))

(seasonal_delta_middle <- middle %>%
    group_by(season) %>%
    summarise(mean_deltaphs = mean(deltaphs, na.rm = TRUE)))

(seasonal_delta_bottom <- bottom %>%
    group_by(season) %>%
    summarise(mean_deltaphs = mean(deltaphs, na.rm = TRUE)))

# plot
library(ggplot2)
library(patchwork)
# Example with fixed y-axis
y_range <- range(c(surface$deltaphs, middle$deltaphs, bottom$deltaphs), na.rm = TRUE)

phs_solar_surface <- ggplot(surface, aes(x = DateTime, y = deltaphs)) +
  geom_line(color = 'deeppink1', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Surface", x = NULL, y = NULL)

phs_solar_middle <- ggplot(middle, aes(x = DateTime, y = deltaphs)) +
  geom_line(color = "maroon", size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Middle", x = NULL, y = "ΔSoluble Phosphorus (mmol/m³)")

phs_solar_bottom <- ggplot(bottom, aes(x = DateTime, y = deltaphs)) +
  geom_line(color = "darkred", size = 1) +
  ylim(y_range) +
  theme_classic() +
  labs(title = "Bottom", x = "Date", y = NULL)

(phs_solar_surface / phs_solar_middle / phs_solar_bottom) +
  plot_annotation(
    title = "Soluble Phosphorus Sensitivity to Shortwave Radiation Extremes")

##solar_do----
solar_min <- 0.5     # min solar
solar_max <- 250   # max solar
solar_range <- solar_max - solar_min
do_min <- get_var("output_solar0.nc", var_name = "OXY_oxy", reference = "surface")
do_max <- get_var("output_solarMAX.nc", var_name = "OXY_oxy", reference = "surface")

calc_deltado <- function(depth_col) {
  
  df_min <- do_min[, c("DateTime", depth_col)]
  df_max <- do_max[, c("DateTime", depth_col)]
  
  colnames(df_min)[2] <- "do_min"
  colnames(df_max)[2] <- "do_max"
  
  df <- merge(df_min, df_max, by = "DateTime")
  
  df$deltado <- df$do_max - df$do_min
  
  # Sensitivity coefficient
  df$sensitivity <- df$deltado / solar_range
  
  return(df)
}

# apply to each depth
surface <- calc_deltado("OXY_oxy_0")
middle  <- calc_deltado("OXY_oxy_2.63435122780277")
bottom  <- calc_deltado("OXY_oxy_4.74183221004499")

delta_all <- data.frame(
  DateTime = surface$DateTime,
  surface = surface$deltado,
  middle  = middle$deltado,
  bottom  = bottom$deltado
)
(delta_do_surface <- mean((surface$deltado), na.rm = TRUE))
(delta_do_middle  <- mean((middle$deltado), na.rm = TRUE))
(delta_do_bottom <- mean((bottom$deltado), na.rm = TRUE))

# add for season
add_season <- function(df){
  df %>%
    mutate(
      month = month(DateTime),
      season = case_when(
        month %in% c(12,1,2)  ~ "Winter",
        month %in% c(3,4,5)   ~ "Spring",
        month %in% c(6,7,8)   ~ "Summer",
        month %in% c(9,10,11) ~ "Autumn"
      )
    )
}

surface <- add_season(surface)
middle  <- add_season(middle)
bottom  <- add_season(bottom)

# calculate delta for seasons
(seasonal_delta_surface <- surface %>%
    group_by(season) %>%
    summarise(mean_deltado = mean(deltado, na.rm = TRUE)))

(seasonal_delta_middle <- middle %>%
    group_by(season) %>%
    summarise(mean_deltado = mean(deltado, na.rm = TRUE)))

(seasonal_delta_bottom <- bottom %>%
    group_by(season) %>%
    summarise(mean_deltado = mean(deltado, na.rm = TRUE)))



# plot
library(ggplot2)
library(patchwork)
# Example with fixed y-axis
y_range <- range(c(surface$deltado, middle$deltado, bottom$deltado), na.rm = TRUE)

do_solar_surface <- ggplot(surface, aes(x = DateTime, y = deltado)) +
  geom_line(color = 'deeppink1', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Surface", x = NULL, y = NULL)

do_solar_middle <- ggplot(middle, aes(x = DateTime, y = deltado)) +
  geom_line(color = "maroon", size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Middle", x = NULL, y = "ΔDissolved Oxygen (mmol/m³)")

do_solar_bottom <- ggplot(bottom, aes(x = DateTime, y = deltado)) +
  geom_line(color = "darkred", size = 1) +
  ylim(y_range) +
  theme_classic() +
  labs(title = "Bottom", x = "Date", y = NULL)

(do_solar_surface / do_solar_middle / do_solar_bottom) +
  plot_annotation(
    title = "Dissolved Oxygen Sensitivity to Shortwave Radiation Extremes")
## solar_cyno----
solar_min <- 0.5     # min solar
solar_max <- 250   # max solar
solar_range <- solar_max - solar_min
cyno_min <- get_var("output_solar0.nc", var_name = "PHY_cyno", reference = "surface")
cyno_max <- get_var("output_solarMAX.nc", var_name = "PHY_cyno", reference = "surface")

calc_deltacyno <- function(depth_col) {
  
  df_min <- cyno_min[, c("DateTime", depth_col)]
  df_max <- cyno_max[, c("DateTime", depth_col)]
  
  colnames(df_min)[2] <- "cyno_min"
  colnames(df_max)[2] <- "cyno_max"
  
  df <- merge(df_min, df_max, by = "DateTime")
  
  df$deltacyno <- df$cyno_max - df$cyno_min
  
  # Sensitivity coefficient
  df$sensitivity <- df$deltacyno / solar_range
  
  return(df)
}

# apply to each depth
surface <- calc_deltacyno("PHY_cyno_0")
middle  <- calc_deltacyno("PHY_cyno_2.63435122780277")
bottom  <- calc_deltacyno("PHY_cyno_4.74183221004499")

delta_all <- data.frame(
  DateTime = surface$DateTime,
  surface = surface$deltacyno,
  middle  = middle$deltacyno,
  bottom  = bottom$deltacyno
)
(delta_cyno_surface <- mean((surface$deltacyno), na.rm = TRUE))
(delta_cyno_middle  <- mean((middle$deltacyno), na.rm = TRUE))
(delta_cyno_bottom <- mean((bottom$deltacyno), na.rm = TRUE))

# add for season
add_season <- function(df){
  df %>%
    mutate(
      month = month(DateTime),
      season = case_when(
        month %in% c(12,1,2)  ~ "Winter",
        month %in% c(3,4,5)   ~ "Spring",
        month %in% c(6,7,8)   ~ "Summer",
        month %in% c(9,10,11) ~ "Autumn"
      )
    )
}

surface <- add_season(surface)
middle  <- add_season(middle)
bottom  <- add_season(bottom)

# calculate delta for seasons
(seasonal_delta_surface <- surface %>%
    group_by(season) %>%
    summarise(mean_deltacyno = mean(deltacyno, na.rm = TRUE)))

(seasonal_delta_middle <- middle %>%
    group_by(season) %>%
    summarise(mean_deltacyno = mean(deltacyno, na.rm = TRUE)))

(seasonal_delta_bottom <- bottom %>%
    group_by(season) %>%
    summarise(mean_deltacyno = mean(deltacyno, na.rm = TRUE)))



# plot
library(ggplot2)
library(patchwork)
# Example with fixed y-axis
y_range <- range(c(surface$deltacyno, middle$deltacyno, bottom$deltacyno), na.rm = TRUE)

cyno_solar_surface <- ggplot(surface, aes(x = DateTime, y = deltacyno)) +
  geom_line(color = 'deeppink1', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Surface", x = NULL, y = NULL)

cyno_solar_middle <- ggplot(middle, aes(x = DateTime, y = deltacyno)) +
  geom_line(color = "maroon", size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Middle", x = NULL, y = "ΔCyanobacteria (mmol/m³)")

cyno_solar_bottom <- ggplot(bottom, aes(x = DateTime, y = deltacyno)) +
  geom_line(color = "darkred", size = 1) +
  ylim(y_range) +
  theme_classic() +
  labs(title = "Bottom", x = "Date", y = NULL)

(cyno_solar_surface / cyno_solar_middle / cyno_solar_bottom) +
  plot_annotation(
    title = "Cyanobacteria Sensitivity to Shortwave Radiation Extremes")


##solar_diat----
solar_min <- 0.5     # min solar
solar_max <- 250   # max solar
solar_range <- solar_max - solar_min
diat_min <- get_var("output_solar0.nc", var_name = "PHY_diat", reference = "surface")
diat_max <- get_var("output_solarMAX.nc", var_name = "PHY_diat", reference = "surface")

calc_deltadiat <- function(depth_col) {
  
  df_min <- diat_min[, c("DateTime", depth_col)]
  df_max <- diat_max[, c("DateTime", depth_col)]
  
  colnames(df_min)[2] <- "diat_min"
  colnames(df_max)[2] <- "diat_max"
  
  df <- merge(df_min, df_max, by = "DateTime")
  
  df$deltadiat <- df$diat_max - df$diat_min
  
  # Sensitivity coefficient
  df$sensitivity <- df$deltadiat / solar_range
  
  return(df)
}

# apply to each depth
surface <- calc_deltadiat("PHY_diat_0")
middle  <- calc_deltadiat("PHY_diat_2.63435122780277")
bottom  <- calc_deltadiat("PHY_diat_4.74183221004499")

delta_all <- data.frame(
  DateTime = surface$DateTime,
  surface = surface$deltadiat,
  middle  = middle$deltadiat,
  bottom  = bottom$deltadiat
)
(delta_diat_surface <- mean((surface$deltadiat), na.rm = TRUE))
(delta_diat_middle  <- mean((middle$deltadiat), na.rm = TRUE))
(delta_diat_bottom <- mean((bottom$deltadiat), na.rm = TRUE))


# add for season
add_season <- function(df){
  df %>%
    mutate(
      month = month(DateTime),
      season = case_when(
        month %in% c(12,1,2)  ~ "Winter",
        month %in% c(3,4,5)   ~ "Spring",
        month %in% c(6,7,8)   ~ "Summer",
        month %in% c(9,10,11) ~ "Autumn"
      )
    )
}

surface <- add_season(surface)
middle  <- add_season(middle)
bottom  <- add_season(bottom)

# calculate delta for seasons
(seasonal_delta_surface <- surface %>%
    group_by(season) %>%
    summarise(mean_deltadiat = mean(deltadiat, na.rm = TRUE)))

(seasonal_delta_middle <- middle %>%
    group_by(season) %>%
    summarise(mean_deltadiat = mean(deltadiat, na.rm = TRUE)))

(seasonal_delta_bottom <- bottom %>%
    group_by(season) %>%
    summarise(mean_deltadiat = mean(deltadiat, na.rm = TRUE)))



# plot
library(ggplot2)
library(patchwork)
# Example with fixed y-axis
y_range <- range(c(surface$deltadiat, middle$deltadiat, bottom$deltadiat), na.rm = TRUE)

diat_solar_surface <- ggplot(surface, aes(x = DateTime, y = deltadiat)) +
  geom_line(color = 'deeppink1', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Surface", x = NULL, y = NULL)

diat_solar_middle <- ggplot(middle, aes(x = DateTime, y = deltadiat)) +
  geom_line(color = "maroon", size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Middle", x = NULL, y = "ΔDiatom (mmol/m³)")

diat_solar_bottom <- ggplot(bottom, aes(x = DateTime, y = deltadiat)) +
  geom_line(color = "darkred", size = 1) +
  ylim(y_range) +
  theme_classic() +
  labs(title = "Bottom", x = "Date", y = NULL)

(diat_solar_surface / diat_solar_middle / diat_solar_bottom) +
  plot_annotation(
    title = "Diatom Sensitivity to Shortwave Radiation Extremes")

## solar_greens ----
solar_min <- 0.5     # min solar
solar_max <- 250   # max solar
solar_range <- solar_max - solar_min
green_min <- get_var("output_solar0.nc", var_name = "PHY_green", reference = "surface")
green_max <- get_var("output_solarMAX.nc", var_name = "PHY_green", reference = "surface")

calc_deltagreen <- function(depth_col) {
  
  df_min <- green_min[, c("DateTime", depth_col)]
  df_max <- green_max[, c("DateTime", depth_col)]
  
  colnames(df_min)[2] <- "green_min"
  colnames(df_max)[2] <- "green_max"
  
  df <- merge(df_min, df_max, by = "DateTime")
  
  df$deltagreen <- df$green_max - df$green_min
  
  # Sensitivity coefficient
  df$sensitivity <- df$deltagreen / solar_range
  
  return(df)
}

# apply to each depth
surface <- calc_deltagreen("PHY_green_0")
middle  <- calc_deltagreen("PHY_green_2.63435122780277")
bottom  <- calc_deltagreen("PHY_green_4.74183221004499")

delta_all <- data.frame(
  DateTime = surface$DateTime,
  surface = surface$deltagreen,
  middle  = middle$deltagreen,
  bottom  = bottom$deltagreen
)
(delta_green_surface <- mean((surface$deltagreen), na.rm = TRUE))
(delta_green_middle  <- mean((middle$deltagreen), na.rm = TRUE))
(delta_green_bottom <- mean((bottom$deltagreen), na.rm = TRUE))


# add for season
add_season <- function(df){
  df %>%
    mutate(
      month = month(DateTime),
      season = case_when(
        month %in% c(12,1,2)  ~ "Winter",
        month %in% c(3,4,5)   ~ "Spring",
        month %in% c(6,7,8)   ~ "Summer",
        month %in% c(9,10,11) ~ "Autumn"
      )
    )
}

surface <- add_season(surface)
middle  <- add_season(middle)
bottom  <- add_season(bottom)

# calculate delta for seasons
(seasonal_delta_surface <- surface %>%
    group_by(season) %>%
    summarise(mean_deltagreen = mean(deltagreen, na.rm = TRUE)))

(seasonal_delta_middle <- middle %>%
    group_by(season) %>%
    summarise(mean_deltagreen = mean(deltagreen, na.rm = TRUE)))

(seasonal_delta_bottom <- bottom %>%
    group_by(season) %>%
    summarise(mean_deltagreen = mean(deltagreen, na.rm = TRUE)))


# plot
library(ggplot2)
library(patchwork)
# Example with fixed y-axis
y_range <- range(c(surface$deltagreen, middle$deltagreen, bottom$deltagreen), na.rm = TRUE)

green_solar_surface <- ggplot(surface, aes(x = DateTime, y = deltagreen)) +
  geom_line(color = 'deeppink1', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Surface", x = NULL, y = NULL)

green_solar_middle <- ggplot(middle, aes(x = DateTime, y = deltagreen)) +
  geom_line(color = "maroon", size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Middle", x = NULL, y = "ΔGreen (mmol/m³)")

green_solar_bottom <- ggplot(bottom, aes(x = DateTime, y = deltagreen)) +
  geom_line(color = "darkred", size = 1) +
  ylim(y_range) +
  theme_classic() +
  labs(title = "Bottom", x = "Date", y = NULL)

(green_solar_surface / green_solar_middle / green_solar_bottom) +
  plot_annotation(
    title = "Green Algae Sensitivity to Shortwave Radiation Extremes")

## wind_thermal----
## load libraries
library(ncdf4)
library(ggplot2)
library(glmtools)
library(dplyr)
library(patchwork)

setwd("C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output")
wind_min_val <- 0   # min wind
wind_max_val <- 15    # max wind
wind_range   <- wind_max_val - wind_min_val
temp_min <- get_var("output_windMIN.nc", var_name = "temp", reference = "surface")
temp_max <- get_var("output_windMAX.nc", var_name = "temp", reference = "surface")

calc_deltaT <- function(depth_col_min, depth_col_max) {
  
  df_min <- temp_min[, c("DateTime", depth_col_min)]
  df_max <- temp_max[, c("DateTime", depth_col_max)]
  
  colnames(df_min)[2] <- "temp_min"
  colnames(df_max)[2] <- "temp_max"
  
  df <- merge(df_min, df_max, by = "DateTime")
  
  df$deltaT <- df$temp_max - df$temp_min
  
  # Sensitivity coefficient
  df$sensitivity <- df$deltaT / wind_range
  
  return(df)
}

# apply to each depth
surface_wind <- calc_deltaT("temp_0", "temp_0")

middle_wind  <- calc_deltaT("temp_2.63472105263158", 
                       "temp_2.63296309693378")

bottom_wind  <- calc_deltaT("temp_4.74249789473684", 
                       "temp_4.7393335744808")

delta_all <- data.frame(
  DateTime = surface_wind$DateTime,
  surface_wind = surface_wind$deltaT,
  middle_wind  = middle_wind$deltaT,
  bottom_wind  = bottom_wind$deltaT
)

(delta_temp_surface <- mean((surface_wind$deltaT), na.rm = TRUE))
(delta_temp_middle  <- mean((middle_wind$deltaT), na.rm = TRUE))
(delta_temp_bottom <- mean((bottom_wind$deltaT), na.rm = TRUE))

# add for season
add_season <- function(df){
  df %>%
    mutate(
      month = month(DateTime),
      season = case_when(
        month %in% c(12,1,2)  ~ "Winter",
        month %in% c(3,4,5)   ~ "Spring",
        month %in% c(6,7,8)   ~ "Summer",
        month %in% c(9,10,11) ~ "Autumn"
      )
    )
}

surface <- add_season(surface_wind)
middle  <- add_season(middle_wind)
bottom  <- add_season(bottom_wind)

# calculate delta for seasons
(seasonal_delta_surface <- surface %>%
    group_by(season) %>%
    summarise(mean_deltaT = mean(deltaT, na.rm = TRUE)))

(seasonal_delta_middle <- middle %>%
    group_by(season) %>%
    summarise(mean_deltaT = mean(deltaT, na.rm = TRUE)))

(seasonal_delta_bottom <- bottom %>%
    group_by(season) %>%
    summarise(mean_deltaT = mean(deltaT, na.rm = TRUE)))


# plot
library(ggplot2)
library(patchwork)
# Example with fixed y-axis
y_range <- range(c(surface_wind$deltaT, middle_wind$deltaT, bottom_wind$deltaT), na.rm = TRUE)

p_surface <- ggplot(surface_wind, aes(x = DateTime, y = deltaT)) +
  geom_line(color = '#339999', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Surface", x = NULL, y = NULL)

p_middle <- ggplot(middle_wind, aes(x = DateTime, y = deltaT)) +
  geom_line(color = '#336666', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Middle", x = NULL, y = "ΔTemp (°C)")

p_bottom <- ggplot(bottom_wind, aes(x = DateTime, y = deltaT)) +
  geom_line(color = "darkblue", size = 1) +
  ylim(y_range) +
  theme_classic() +
  labs(title = "Bottom", x = "Date", y = NULL)

(p_surface / p_middle / p_bottom) +
  plot_annotation(
    title = "Thermal Sensitivity to Windspeed Extremes")


## wind_no3----
setwd("C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output")
wind_min_val <- 0   # min wind
wind_max_val <- 15    # max wind
wind_range   <- wind_max_val - wind_min_val
nit_min <- get_var("output_windMIN.nc", var_name = "NIT_nit", reference = "surface")
nit_max <- get_var("output_windMAX.nc", var_name = "NIT_nit", reference = "surface")

calc_delta_nit <- function(depth_col_min, depth_col_max) {
  
  df_min <- nit_min[, c("DateTime", depth_col_min)]
  df_max <- nit_max[, c("DateTime", depth_col_max)]
  
  colnames(df_min)[2] <- "nit_min"
  colnames(df_max)[2] <- "nit_max"
  
  df <- merge(df_min, df_max, by = "DateTime")
  
  df$deltanit <- df$nit_max - df$nit_min
  
  # Sensitivity coefficient
  df$sensitivity <- df$deltanit / wind_range
  
  return(df)
}



# apply to each depth
surface_nit <- calc_delta_nit("NIT_nit_0", "NIT_nit_0")

middle_nit  <- calc_delta_nit("NIT_nit_2.63472105263158", 
                            "NIT_nit_2.63296309693378")

bottom_nit  <- calc_delta_nit("NIT_nit_4.74249789473684", 
                            "NIT_nit_4.7393335744808")

delta_all <- data.frame(
  DateTime = surface_nit$DateTime,
  surface_nit = surface_nit$deltanit,
  middle_nit  = middle_nit$deltanit,
  bottom_nit  = bottom_nit$deltanit
)

(delta_nit_surface <- mean((surface_nit$deltanit), na.rm = TRUE))
(delta_nit_middle  <- mean((middle_nit$deltanit), na.rm = TRUE))
(delta_nit_bottom <- mean((bottom_nit$deltanit), na.rm = TRUE))


# add for season
add_season <- function(df){
  df %>%
    mutate(
      month = month(DateTime),
      season = case_when(
        month %in% c(12,1,2)  ~ "Winter",
        month %in% c(3,4,5)   ~ "Spring",
        month %in% c(6,7,8)   ~ "Summer",
        month %in% c(9,10,11) ~ "Autumn"
      )
    )
}

surface <- add_season(surface_nit)
middle  <- add_season(middle_nit)
bottom  <- add_season(bottom_nit)

# calculate delta for seasons
(seasonal_delta_surface <- surface %>%
    group_by(season) %>%
    summarise(mean_deltanit = mean(deltanit, na.rm = TRUE)))

(seasonal_delta_middle <- middle %>%
    group_by(season) %>%
    summarise(mean_deltanit = mean(deltanit, na.rm = TRUE)))

(seasonal_delta_bottom <- bottom %>%
    group_by(season) %>%
    summarise(mean_deltanit = mean(deltanit, na.rm = TRUE)))


# plot
library(ggplot2)
library(patchwork)
# Example with fixed y-axis
y_range <- range(c(surface_nit$deltanit, middle_nit$deltanit, bottom_nit$deltanit), na.rm = TRUE)

p_surface <- ggplot(surface_nit, aes(x = DateTime, y = deltanit)) +
  geom_line(color = '#339999', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Surface", x = NULL, y = NULL)

p_middle <- ggplot(middle_nit, aes(x = DateTime, y = deltanit)) +
  geom_line(color = '#336666', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Middle", x = NULL, y = "ΔNitrate (mmol/m³)")

p_bottom <- ggplot(bottom_nit, aes(x = DateTime, y = deltanit)) +
  geom_line(color = "darkblue", size = 1) +
  ylim(y_range) +
  theme_classic() +
  labs(title = "Bottom", x = "Date", y = NULL)

(p_surface / p_middle / p_bottom) +
  plot_annotation(
    title = "Nitrate Sensitivity to Windspeed Extremes")

# wind_phs----
setwd("C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output")
wind_min_val <- 0   # min wind
wind_max_val <- 15    # max wind
wind_range   <- wind_max_val - wind_min_val
phs_min <- get_var("output_windMIN.nc", var_name = "PHS_frp", reference = "surface")
phs_max <- get_var("output_windMAX.nc", var_name = "PHS_frp", reference = "surface")

calc_delta_phs <- function(depth_col_min, depth_col_max) {
  
  df_min <- phs_min[, c("DateTime", depth_col_min)]
  df_max <- phs_max[, c("DateTime", depth_col_max)]
  
  colnames(df_min)[2] <- "phs_min"
  colnames(df_max)[2] <- "phs_max"
  
  df <- merge(df_min, df_max, by = "DateTime")
  
  df$deltaphs <- df$phs_max - df$phs_min
  
  # Sensitivity coefficient
  df$sensitivity <- df$deltaphs / wind_range
  
  return(df)
}



# apply to each depth
surface_phs <- calc_delta_phs("PHS_frp_0", "PHS_frp_0")

middle_phs  <- calc_delta_phs("PHS_frp_2.63472105263158", 
                              "PHS_frp_2.63296309693378")

bottom_phs  <- calc_delta_phs("PHS_frp_4.74249789473684", 
                              "PHS_frp_4.7393335744808")

delta_all <- data.frame(
  DateTime = surface_phs$DateTime,
  surface_phs = surface_phs$deltaphs,
  middle_phs  = middle_phs$deltaphs,
  bottom_phs  = bottom_phs$deltaphs
)

(delta_phs_surface <- mean((surface_phs$deltaphs), na.rm = TRUE))
(delta_phs_middle  <- mean((middle_phs$deltaphs), na.rm = TRUE))
(delta_phs_bottom <- mean((bottom_phs$deltaphs), na.rm = TRUE))



# add for season
add_season <- function(df){
  df %>%
    mutate(
      month = month(DateTime),
      season = case_when(
        month %in% c(12,1,2)  ~ "Winter",
        month %in% c(3,4,5)   ~ "Spring",
        month %in% c(6,7,8)   ~ "Summer",
        month %in% c(9,10,11) ~ "Autumn"
      )
    )
}

surface <- add_season(surface_phs)
middle  <- add_season(middle_phs)
bottom  <- add_season(bottom_phs)

# calculate delta for seasons
(seasonal_delta_surface <- surface %>%
    group_by(season) %>%
    summarise(mean_deltaphs = mean(deltaphs, na.rm = TRUE)))

(seasonal_delta_middle <- middle %>%
    group_by(season) %>%
    summarise(mean_deltaphs = mean(deltaphs, na.rm = TRUE)))

(seasonal_delta_bottom <- bottom %>%
    group_by(season) %>%
    summarise(mean_deltaphs = mean(deltaphs, na.rm = TRUE)))



# plot
library(ggplot2)
library(patchwork)
# Example with fixed y-axis
y_range <- range(c(surface_phs$deltaphs, middle_phs$deltaphs, bottom_phs$deltaphs), na.rm = TRUE)

p_surface <- ggplot(surface_phs, aes(x = DateTime, y = deltaphs)) +
  geom_line(color = '#339999', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Surface", x = NULL, y = NULL)

p_middle <- ggplot(middle_phs, aes(x = DateTime, y = deltaphs)) +
  geom_line(color = '#336666', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Middle", x = NULL, y = "ΔSoluble Phosphorus (mmol/m³)")

p_bottom <- ggplot(bottom_phs, aes(x = DateTime, y = deltaphs)) +
  geom_line(color = "darkblue", size = 1) +
  ylim(y_range) +
  theme_classic() +
  labs(title = "Bottom", x = "Date", y = NULL)

(p_surface / p_middle / p_bottom) +
  plot_annotation(
    title = "Soluble Phosphorus Sensitivity to Windspeed Extremes")

# wind_do----
setwd("C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output")
wind_min_val <- 0   # min wind
wind_max_val <- 15    # max wind
wind_range   <- wind_max_val - wind_min_val
do_min <- get_var("output_windMIN.nc", var_name = "OXY_oxy", reference = "surface")
do_max <- get_var("output_windMAX.nc", var_name = "OXY_oxy", reference = "surface")

calc_delta_do <- function(depth_col_min, depth_col_max) {
  
  df_min <- do_min[, c("DateTime", depth_col_min)]
  df_max <- do_max[, c("DateTime", depth_col_max)]
  
  colnames(df_min)[2] <- "do_min"
  colnames(df_max)[2] <- "do_max"
  
  df <- merge(df_min, df_max, by = "DateTime")
  
  df$deltado <- df$do_max - df$do_min
  
  # Sensitivity coefficient
  df$sensitivity <- df$deltado / wind_range
  
  return(df)
}



# apply to each depth
surface_do <- calc_delta_do("OXY_oxy_0", "OXY_oxy_0")

middle_do  <- calc_delta_do("OXY_oxy_2.63472105263158", 
                              "OXY_oxy_2.63296309693378")

bottom_do  <- calc_delta_do("OXY_oxy_4.74249789473684", 
                              "OXY_oxy_4.7393335744808")

delta_all <- data.frame(
  DateTime = surface_do$DateTime,
  surface_do = surface_do$deltado,
  middle_do  = middle_do$deltado,
  bottom_do  = bottom_do$deltado
)

(delta_do_surface <- mean((surface_do$deltado), na.rm = TRUE))
(delta_do_middle  <- mean((middle_do$deltado), na.rm = TRUE))
(delta_do_bottom <- mean((bottom_do$deltado), na.rm = TRUE))

# add for season
add_season <- function(df){
  df %>%
    mutate(
      month = month(DateTime),
      season = case_when(
        month %in% c(12,1,2)  ~ "Winter",
        month %in% c(3,4,5)   ~ "Spring",
        month %in% c(6,7,8)   ~ "Summer",
        month %in% c(9,10,11) ~ "Autumn"
      )
    )
}



surface <- add_season(surface_do)
middle  <- add_season(middle_do)
bottom  <- add_season(bottom_do)

# calculate delta for seasons
(seasonal_delta_surface <- surface %>%
    group_by(season) %>%
    summarise(mean_deltado = mean(deltado, na.rm = TRUE)))

(seasonal_delta_middle <- middle %>%
    group_by(season) %>%
    summarise(mean_deltado = mean(deltado, na.rm = TRUE)))

(seasonal_delta_bottom <- bottom %>%
    group_by(season) %>%
    summarise(mean_deltado = mean(deltado, na.rm = TRUE)))




# plot
library(ggplot2)
library(patchwork)
# Example with fixed y-axis
y_range <- range(c(surface_do$deltado, middle_do$deltado, bottom_do$deltado), na.rm = TRUE)

p_surface <- ggplot(surface_do, aes(x = DateTime, y = deltado)) +
  geom_line(color = '#339999', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Surface", x = NULL, y = NULL)

p_middle <- ggplot(middle_do, aes(x = DateTime, y = deltado)) +
  geom_line(color = '#336666', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Middle", x = NULL, y = "ΔDissolved Oxygen (mmol/m³)")

p_bottom <- ggplot(bottom_do, aes(x = DateTime, y = deltado)) +
  geom_line(color = "darkblue", size = 1) +
  ylim(y_range) +
  theme_classic() +
  labs(title = "Bottom", x = "Date", y = NULL)

(p_surface / p_middle / p_bottom) +
  plot_annotation(
    title = "Dissolved Oxygen Sensitivity to Windspeed Extremes")

# cyno_wind----
setwd("C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/sensitivity/sensitivity_output")
wind_min_val <- 0   # min wind
wind_max_val <- 15    # max wind
wind_range   <- wind_max_val - wind_min_val
cyno_min <- get_var("output_windMIN.nc", var_name = "PHY_cyno", reference = "surface")
cyno_max <- get_var("output_windMAX.nc", var_name = "PHY_cyno", reference = "surface")

calc_delta_cyno <- function(depth_col_min, depth_col_max) {
  
  df_min <- cyno_min[, c("DateTime", depth_col_min)]
  df_max <- cyno_max[, c("DateTime", depth_col_max)]
  
  colnames(df_min)[2] <- "cyno_min"
  colnames(df_max)[2] <- "cyno_max"
  
  df <- merge(df_min, df_max, by = "DateTime")
  
  df$deltacyno <- df$cyno_max - df$cyno_min
  
  # Sensitivity coefficient
  df$sensitivity <- df$deltacyno / wind_range
  
  return(df)
}



# apply to each depth
colnames(cyno_min)
colnames(cyno_max)
surface_cyno <- calc_delta_cyno("PHY_cyno_0", "PHY_cyno_0")

middle_cyno  <- calc_delta_cyno("PHY_cyno_2.63472105263158", 
                              "PHY_cyno_2.63296309693378")

bottom_cyno  <- calc_delta_cyno("PHY_cyno_4.74249789473684", 
                              "PHY_cyno_4.7393335744808")

delta_all <- data.frame(
  DateTime = surface_cyno$DateTime,
  surface_cyno = surface_cyno$deltacyno,
  middle_cyno  = middle_cyno$deltacyno,
  bottom_cyno  = bottom_cyno$deltacyno
)

(delta_cyno_surface <- mean((surface_cyno$deltacyno), na.rm = TRUE))
(delta_cyno_middle  <- mean((middle_cyno$deltacyno), na.rm = TRUE))
(delta_cyno_bottom <- mean((bottom_cyno$deltacyno), na.rm = TRUE))

# add for season
add_season <- function(df){
  df %>%
    mutate(
      month = month(DateTime),
      season = case_when(
        month %in% c(12,1,2)  ~ "Winter",
        month %in% c(3,4,5)   ~ "Spring",
        month %in% c(6,7,8)   ~ "Summer",
        month %in% c(9,10,11) ~ "Autumn"
      )
    )
}

surface <- add_season(surface_cyno)
middle  <- add_season(middle_cyno)
bottom  <- add_season(bottom_cyno)

# calculate delta for seasons
(seasonal_delta_surface <- surface %>%
    group_by(season) %>%
    summarise(mean_deltacyno = mean(deltacyno, na.rm = TRUE)))

(seasonal_delta_middle <- middle %>%
    group_by(season) %>%
    summarise(mean_deltacyno = mean(deltacyno, na.rm = TRUE)))

(seasonal_delta_bottom <- bottom %>%
    group_by(season) %>%
    summarise(mean_deltacyno = mean(deltacyno, na.rm = TRUE)))


# plot
library(ggplot2)
library(patchwork)
# Example with fixed y-axis
y_range <- range(c(surface_cyno$deltacyno, middle_cyno$deltacyno, bottom_cyno$deltacyno), na.rm = TRUE)

p_surface <- ggplot(surface_cyno, aes(x = DateTime, y = deltacyno)) +
  geom_line(color = '#339999', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Surface", x = NULL, y = NULL)

p_middle <- ggplot(middle_cyno, aes(x = DateTime, y = deltacyno)) +
  geom_line(color = '#336666', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Middle", x = NULL, y = "ΔCyanobacteria (mmol/m³)")

p_bottom <- ggplot(bottom_cyno, aes(x = DateTime, y = deltacyno)) +
  geom_line(color = "darkblue", size = 1) +
  ylim(y_range) +
  theme_classic() +
  labs(title = "Bottom", x = "Date", y = NULL)

(p_surface / p_middle / p_bottom) +
  plot_annotation(
    title = "Cyanobacteria Sensitivity to Windspeed Extremes")

## diat_wind----
wind_min_val <- 0   # min wind
wind_max_val <- 15    # max wind
wind_range   <- wind_max_val - wind_min_val
diat_min <- get_var("output_windMIN.nc", var_name = "PHY_diat", reference = "surface")
diat_max <- get_var("output_windMAX.nc", var_name = "PHY_diat", reference = "surface")

calc_delta_diat <- function(depth_col_min, depth_col_max) {
  
  df_min <- diat_min[, c("DateTime", depth_col_min)]
  df_max <- diat_max[, c("DateTime", depth_col_max)]
  
  colnames(df_min)[2] <- "diat_min"
  colnames(df_max)[2] <- "diat_max"
  
  df <- merge(df_min, df_max, by = "DateTime")
  
  df$deltadiat <- df$diat_max - df$diat_min
  
  # Sensitivity coefficient
  df$sensitivity <- df$deltadiat / wind_range
  
  return(df)
}



# apply to each depth
colnames(diat_min)
colnames(diat_max)
surface_diat <- calc_delta_diat("PHY_diat_0", "PHY_diat_0")

middle_diat  <- calc_delta_diat("PHY_diat_2.63472105263158", 
                                "PHY_diat_2.63296309693378")

bottom_diat  <- calc_delta_diat("PHY_diat_4.74249789473684", 
                                "PHY_diat_4.7393335744808")

delta_all <- data.frame(
  DateTime = surface_diat$DateTime,
  surface_diat = surface_diat$deltadiat,
  middle_diat  = middle_diat$deltadiat,
  bottom_diat  = bottom_diat$deltadiat
)

(delta_diat_surface <- mean((surface_diat$deltadiat), na.rm = TRUE))
(delta_diat_middle  <- mean((middle_diat$deltadiat), na.rm = TRUE))
(delta_diat_bottom <- mean((bottom_diat$deltadiat), na.rm = TRUE))

# add for season
add_season <- function(df){
  df %>%
    mutate(
      month = month(DateTime),
      season = case_when(
        month %in% c(12,1,2)  ~ "Winter",
        month %in% c(3,4,5)   ~ "Spring",
        month %in% c(6,7,8)   ~ "Summer",
        month %in% c(9,10,11) ~ "Autumn"
      )
    )
}

surface <- add_season(surface_diat)
middle  <- add_season(middle_diat)
bottom  <- add_season(bottom_diat)

# calculate delta for seasons
(seasonal_delta_surface <- surface %>%
    group_by(season) %>%
    summarise(mean_deltadiat = mean(deltadiat, na.rm = TRUE)))

(seasonal_delta_middle <- middle %>%
    group_by(season) %>%
    summarise(mean_deltadiat = mean(deltadiat, na.rm = TRUE)))

(seasonal_delta_bottom <- bottom %>%
    group_by(season) %>%
    summarise(mean_deltadiat = mean(deltadiat, na.rm = TRUE)))



# plot
library(ggplot2)
library(patchwork)
# Example with fixed y-axis
y_range <- range(c(surface_diat$deltadiat, middle_diat$deltadiat, bottom_diat$deltadiat), na.rm = TRUE)

p_surface <- ggplot(surface_diat, aes(x = DateTime, y = deltadiat)) +
  geom_line(color = '#339999', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Surface", x = NULL, y = NULL)

p_middle <- ggplot(middle_diat, aes(x = DateTime, y = deltadiat)) +
  geom_line(color = '#336666', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Middle", x = NULL, y = "ΔDiatom (mmol/m³)")

p_bottom <- ggplot(bottom_diat, aes(x = DateTime, y = deltadiat)) +
  geom_line(color = "darkblue", size = 1) +
  ylim(y_range) +
  theme_classic() +
  labs(title = "Bottom", x = "Date", y = NULL)

(p_surface / p_middle / p_bottom) +
  plot_annotation(
    title = "Diatom Sensitivity to Windspeed Extremes")

# greens_wind----
wind_min_val <- 0   # min wind
wind_max_val <- 15    # max wind
wind_range   <- wind_max_val - wind_min_val
green_min <- get_var("output_windMIN.nc", var_name = "PHY_green", reference = "surface")
green_max <- get_var("output_windMAX.nc", var_name = "PHY_green", reference = "surface")

calc_delta_green <- function(depth_col_min, depth_col_max) {
  
  df_min <- green_min[, c("DateTime", depth_col_min)]
  df_max <- green_max[, c("DateTime", depth_col_max)]
  
  colnames(df_min)[2] <- "green_min"
  colnames(df_max)[2] <- "green_max"
  
  df <- merge(df_min, df_max, by = "DateTime")
  
  df$deltagreen <- df$green_max - df$green_min
  
  # Sensitivity coefficient
  df$sensitivity <- df$deltagreen / wind_range
  
  return(df)
}



# apply to each depth
colnames(green_min)
colnames(green_max)
surface_green <- calc_delta_green("PHY_green_0", "PHY_green_0")

middle_green  <- calc_delta_green("PHY_green_2.63472105263158", 
                              "PHY_green_2.63296309693378")

bottom_green  <- calc_delta_green("PHY_green_4.74249789473684", 
                              "PHY_green_4.7393335744808")

delta_all <- data.frame(
  DateTime = surface_green$DateTime,
  surface_green = surface_green$deltagreen,
  middle_green  = middle_green$deltagreen,
  bottom_green  = bottom_green$deltagreen
)

(delta_green_surface <- mean((surface_green$deltagreen), na.rm = TRUE))
(delta_green_middle  <- mean((middle_green$deltagreen), na.rm = TRUE))
(delta_green_bottom <- mean((bottom_green$deltagreen), na.rm = TRUE))



# add for season
add_season <- function(df){
  df %>%
    mutate(
      month = month(DateTime),
      season = case_when(
        month %in% c(12,1,2)  ~ "Winter",
        month %in% c(3,4,5)   ~ "Spring",
        month %in% c(6,7,8)   ~ "Summer",
        month %in% c(9,10,11) ~ "Autumn"
      )
    )
}

surface <- add_season(surface_green)
middle  <- add_season(middle_green)
bottom  <- add_season(bottom_green)

# calculate delta for seasons
(seasonal_delta_surface <- surface %>%
    group_by(season) %>%
    summarise(mean_deltagreen = mean(deltagreen, na.rm = TRUE)))

(seasonal_delta_middle <- middle %>%
    group_by(season) %>%
    summarise(mean_deltagreen = mean(deltagreen, na.rm = TRUE)))

(seasonal_delta_bottom <- bottom %>%
    group_by(season) %>%
    summarise(mean_deltagreen = mean(deltagreen, na.rm = TRUE)))


# plot
library(ggplot2)
library(patchwork)
# Example with fixed y-axis
y_range <- range(c(surface_green$deltagreen, middle_green$deltagreen, bottom_green$deltagreen), na.rm = TRUE)

p_surface <- ggplot(surface_green, aes(x = DateTime, y = deltagreen)) +
  geom_line(color = '#339999', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Surface", x = NULL, y = NULL)

p_middle <- ggplot(middle_green, aes(x = DateTime, y = deltagreen)) +
  geom_line(color = '#336666', size = 1) +
  ylim(y_range) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Middle", x = NULL, y = "ΔGreen (mmol/m³)")

p_bottom <- ggplot(bottom_green, aes(x = DateTime, y = deltagreen)) +
  geom_line(color = "darkblue", size = 1) +
  ylim(y_range) +
  theme_classic() +
  labs(title = "Bottom", x = "Date", y = NULL)

(p_surface / p_middle / p_bottom) +
  plot_annotation(
    title = "Green Sensitivity to Windspeed Extremes")
