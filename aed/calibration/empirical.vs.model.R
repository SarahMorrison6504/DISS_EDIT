# plotting empirical data vs modelled and calculating rmse for:
# nitrate, soluble phosphorus, surface temp


rm(list = ls())
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed')
library(glmtools)
library(ncdf4)
library(ggplot2)

## NITRATE----

# get modelled surface values

ncfile <- 'aed/output/output.nc'
model_nit <- get_var(ncfile, var_name = 'NIT_nit', reference = 'surface')
model_nit <- model_nit[, c('DateTime', 'NIT_nit_0')] # keep only necessary columns
model_nit$Date <- as.Date(model_nit$DateTime) # drop time
model_nit$DateTime <- NULL

# clean obs data
obs_nit <- read.csv("aed/calibration/nit_obs.csv")

# Make sure Depth is 0 only
obs_nit <- obs_nit[obs_nit$Depth == 0, ]

# Convert DateTime properly
obs_nit$DateTime <- as.POSIXct(obs_nit$DateTime, tz = "UTC")

# Convert to Date (drop time component)
obs_nit$Date <- as.Date(obs_nit$DateTime)

# Keep only what we need
obs_nit <- obs_nit[, c("Date", "NIT_nit")]

compare <- merge(model_nit, obs_nit, by = "Date")

# Rename columns for clarity
colnames(compare) <- c("Date", "Model", "Observed")

#plot
ggplot(compare, aes(x = Date)) +
  geom_line(aes(y = Model, colour = "Model"), linewidth = 1) +
  geom_point(aes(y = Observed, colour = "Observed"), size = 2) +
  scale_colour_manual(values = c("Model" = "blue",
                                 "Observed" = "red")) +
  labs(y = "Surface NIT_nit (mmol/m3)",
       colour = "",
       title = "Surface Nitrate: Model vs Observed") +
  theme_minimal() +
  theme(legend.position = "top")

# calculate rmse
rmse <- sqrt(mean((compare$Model - compare$Observed)^2, 
                  na.rm = TRUE))
# show rmse
rmse

## SOLUBLE PHOSPHORUS----
ncfile <- "output/output_baseline.nc"   

model_phs <- get_var(ncfile, var_name = "PHS_frp", reference = "surface")

# Keep only DateTime + variable
model_phs <- model_phs[, c("DateTime", "PHS_frp_0")]

# Convert to Date (remove time component)
model_phs$Date <- as.Date(model_phs$DateTime)

# Remove DateTime column
model_phs$DateTime <- NULL
model_phs
## observed data
obs_phs <- read.csv("calibration/PHSfrp.obs.csv")  # adjust filename if needed

# Keep only surface
obs_phs <- obs_phs[obs_phs$Depth == 0, ]

# Convert DateTime properly
obs_phs$DateTime <- as.POSIXct(obs_phs$DateTime, tz = "UTC")

# Convert to Date
obs_phs$Date <- as.Date(obs_phs$DateTime)

# Keep only Date + PHS_frp
obs_phs <- obs_phs[, c("Date", "PHS_frp")]
obs_phs
# merge
compare_phs <- merge(model_phs, obs_phs, by = "Date")

# Rename columns for clarity
colnames(compare_phs) <- c("Date", "Model", "Observed")
compare_phs_filtered <- compare_phs %>%
  filter(Date >= as.Date("2016-01-01") & Date <= as.Date("2016-12-31"))

ggplot(compare_phs_filtered, aes(x = Date)) +
  geom_line(aes(y = Model, colour = "Model"), linewidth = 1) +
  geom_line(aes(y = Observed, colour = "Observed"), size = 2) +
  scale_colour_manual(values = c("Model" = "blue",
                                 "Observed" = "red")) +
  labs(y = "Surface Soluble Phosphorus (mmol/m3)",
       colour = "",
       title = "Surface Soluble Phosphorus: Model vs Observed") +
  theme_classic() +
  theme(legend.position = "top")+
  scale_y_continuous(limits = c(0,1.5))

# get rmse
rmse_phs <- sqrt(mean((compare_phs_filtered$Model - compare_phs_filtered$Observed)^2, 
                  na.rm = TRUE))
# print rmse
rmse_phs

#mae
mae(compare_phs_filtered$Model,compare_phs_filtered$Observed)

## FOR SURFACE TEMP ----
ncfile <- "output/output_baseline.nc"

model_temp <- get_var(ncfile, var_name = "temp", reference = "surface")

# Keep needed columns
model_temp <- model_temp[, c("DateTime", "temp_0")]

# Convert to Date
model_temp$Date <- as.Date(model_temp$DateTime)

# Remove DateTime
model_temp$DateTime <- NULL

# clean obs
obs_temp <- read.csv("calibration/emp.temp.csv")

# Keep only surface observations
obs_temp <- obs_temp[obs_temp$Depth == 0, ]

# Convert DateTime properly
obs_temp$DateTime <- as.POSIXct(obs_temp$DateTime, tz = "UTC")

# Convert to Date
obs_temp$Date <- as.Date(obs_temp$DateTime)

# Keep only needed columns
obs_temp <- obs_temp[, c("Date", "temp")]

# merge model and observed
compare_temp <- merge(model_temp, obs_temp, by = "Date")

# Rename for clarity
colnames(compare_temp) <- c("Date", "Model", "Observed")

# filter to only be 2016
compare_temp_filtered <- compare_temp %>%
  filter(Date >= as.Date("2016-01-01") & Date <= as.Date("2016-12-31"))

# plot
ggplot(compare_temp_filtered, aes(x = Date)) +
  geom_line(aes(y = Model, colour = "Model"), linewidth = 1) +
  geom_point(aes(y = Observed, colour = "Observed"), size = 2) +
  scale_colour_manual(values = c("Model" = "blue",
                                 "Observed" = "red")) +
  labs(y = "Surface Temperature (°C)",
       colour = "",
       title = "Surface Temperature: Model vs Observed") +
  theme_classic() +
  theme(legend.position = "top")

#rmse
rmse_temp <- sqrt(mean((compare_temp_filtered$Model - compare_temp_filtered$Observed)^2, 
                na.rm = TRUE))
# print rmse
rmse_temp

# mae
install.packages('Metrics')
library(Metrics)
mae_temp <- mae(compare_temp_filtered$Model, compare_temp_filtered$Observed)
mae_temp

## Cyno: Model vs Observed----
library(ggplot2)
library(tidyr)

# Load model data
ncfile <- "output/output.nc"
model_cyno <- get_var(ncfile, var_name = "PHY_cyno", reference = "surface")

# Keep needed columns and rename
model_cyno <- model_cyno[, c("DateTime", "PHY_cyno_0")]
colnames(model_cyno) <- c("DateTime", "Model")

# Convert DateTime to Date
model_cyno$Date <- as.Date(model_cyno$DateTime)
model_cyno$DateTime <- NULL

# Load observed data
obs_cyno <- read.csv("calibration/PHY_cyno(obs).csv")

# Keep only surface observations if Depth column exists
if ("Depth" %in% colnames(obs_cyno)) {
  obs_cyno <- obs_cyno[obs_cyno$Depth == 0, ]
}

# Keep only needed columns and rename
obs_cyno <- obs_cyno[, c("Date", "PHY_cyno")]
colnames(obs_cyno) <- c("Date", "Observed")

# Convert Date to Date class
obs_cyno$Date <- as.Date(obs_cyno$Date)

# Remove rows with NA observations
obs_cyno <- obs_cyno[!is.na(obs_cyno$Observed), ]

# Merge model and observed data (left join to keep all model dates)
compare_cyno <- merge(model_cyno, obs_cyno, by = "Date", all.x = TRUE)


compare_cyno_filtered <- compare_cyno %>%
  filter(Date >= as.Date("2016-01-01") & Date <= as.Date("2016-12-31"))

# find rmse
rmse <- sqrt(mean((compare_cyno_filtered$Model - compare_cyno_filtered$Observed)^2, na.rm = TRUE))
rmse

# mae
mae_cyno <- mae(compare_cyno_filtered$Model, compare_cyno_filtered$Observed)
mae_cyno

# Reshape to long format for ggplot
compare_long <- compare_cyno_filtered %>%
  pivot_longer(cols = c("Model", "Observed"),
               names_to = "Source",
               values_to = "Cyno")

# Plot
ggplot(compare_cyno_filtered, aes(x = Date, y = Cyno, colour = Source)) +
  geom_line(data = subset(compare_long, Source == "Model"), linewidth = 1) +
  geom_point(data = subset(compare_long, Source == "Observed"), size = 2) +
  scale_colour_manual(values = c("Model" = "blue", "Observed" = "red")) +
  labs(y = "Surface Cyanobacteria (mmol/m**3)",
       colour = "",
       title = "Surface Cyanobacteria: Model vs Observed") +
  theme_classic() +
  theme(legend.position = "top")
#rmse
rmse <- sqrt(mean((compare_long$Model - compare_long$Observed)^2, 
                  na.rm = TRUE))
# print rmse
rmse

# cyno: diatoms-----

library(ggplot2)
library(tidyr)

# Load model data
ncfile <- "output/output.nc"
model_diat <- get_var(ncfile, var_name = "PHY_diat", reference = "surface")

# Keep needed columns and rename
model_diat <- model_diat[, c("DateTime", "PHY_diat_0")]
colnames(model_diat) <- c("DateTime", "Model")

# Convert DateTime to Date
model_diat$Date <- as.Date(model_diat$DateTime)
model_diat$DateTime <- NULL

# Load observed data
obs_diat <- read.csv("calibration/PHY_diat(obs).csv")

# Keep only surface observations if Depth column exists
if ("Depth" %in% colnames(obs_diat)) {
  obs_diat <- obs_diat[obs_diat$Depth == 0, ]
}

# Keep only needed columns and rename
obs_diat <- obs_diat[, c("Date", "PHY_diat")]
colnames(obs_diat) <- c("Date", "Observed")

# Convert Date to Date class
obs_diat$Date <- as.Date(obs_diat$Date)

# Remove rows with NA observations
obs_diat <- obs_diat[!is.na(obs_diat$Observed), ]

# Merge model and observed data (left join to keep all model dates)
compare_diat <- merge(model_diat, obs_diat, by = "Date", all.x = TRUE)

# make only 2016
compare_diat_filtered <- compare_diat %>%
  filter(Date >= as.Date("2016-01-01") & Date <= as.Date("2016-12-31"))

# get rid of nas
compare_diat_clean <- compare_diat_filtered %>%
  filter(!is.na(Model) & !is.na(Observed))
# find rmse
rmse <- sqrt(mean((compare_diat_filtered$Model - compare_diat_filtered$Observed)^2, na.rm = TRUE))
rmse

mae(compare_diat_clean$Model, compare_diat_clean$Observed )


# Reshape to long format for ggplot
compare_long <- compare_diat_filtered %>%
  pivot_longer(cols = c("Model", "Observed"),
               names_to = "Source",
               values_to = "Diat")

# Plot
ggplot(compare_long, aes(x = Date, y = Diat, colour = Source)) +
  geom_line(data = subset(compare_long, Source == "Model"), linewidth = 1) +
  geom_point(data = subset(compare_long, Source == "Observed"), size = 2) +
  scale_colour_manual(values = c("Model" = "blue", "Observed" = "red")) +
  labs(y = "Surface Diatoms (mmol/m**3)",
       colour = "",
       title = "Surface Diatoms: Model vs Observed") +
  theme_classic() +
  theme(legend.position = "top")

#rmse
rmse <- sqrt(mean((compare_diat$Model - compare_diat$Observed)^2, 
                  na.rm = TRUE))
# print rmse
rmse
mae(compare_diat$Model, compare_diat$Observed)

## greens----


library(ggplot2)
library(tidyr)

# Load model data
ncfile <- "output/output.nc"
model_green <- get_var(ncfile, var_name = "PHY_green", reference = "surface")

# Keep needed columns and rename
model_green <- model_green[, c("DateTime", "PHY_green_0")]
colnames(model_green) <- c("DateTime", "Model")

# Convert DateTime to Date
model_green$Date <- as.Date(model_green$DateTime)
model_green$DateTime <- NULL

# Load observed data
obs_green <- read.csv("calibration/PHY_green(obs).csv")

# Keep only surface observations if Depth column exists
if ("Depth" %in% colnames(obs_green)) {
  obs_green <- obs_green[obs_green$Depth == 0, ]
}

# Keep only needed columns and rename
obs_green <- obs_green[, c("Date", "PHY_green")]
colnames(obs_green) <- c("Date", "Observed")

# Convert Date to Date class
obs_green$Date <- as.Date(obs_green$Date)

# Remove rows with NA observations
obs_green <- obs_green[!is.na(obs_green$Observed), ]

# Merge model and observed data (left join to keep all model dates)
compare_green <- merge(model_green, obs_green, by = "Date", all.x = TRUE)

# make only 2016
compare_green_filtered <- compare_green %>%
  filter(Date >= as.Date("2016-01-01") & Date <= as.Date("2016-12-31"))

# get rid of nas
compare_green_clean <- compare_green_filtered %>%
  filter(!is.na(Model) & !is.na(Observed))

# find rmse
rmse <- sqrt(mean((compare_green_clean$Model - compare_green_clean$Observed)^2, na.rm = TRUE))
rmse

#find mae
mae_green <- mae(compare_green_clean$Model, compare_green_clean$Observed )
mae_green

# Reshape to long format for ggplot
compare_long <- compare_diat_filtered %>%
  pivot_longer(cols = c("Model", "Observed"),
               names_to = "Source",
               values_to = "Diat")
# Reshape to long format for ggplot
compare_long <- compare_green %>%
  pivot_longer(cols = c("Model", "Observed"),
               names_to = "Source",
               values_to = "green")

# Plot
ggplot(compare_long, aes(x = Date, y = green, colour = Source)) +
  geom_line(data = subset(compare_long, Source == "Model"), linewidth = 1) +
  geom_point(data = subset(compare_long, Source == "Observed"), size = 2) +
  scale_colour_manual(values = c("Model" = "blue", "Observed" = "red")) +
  labs(y = "Surface Greens (mmol/m**3)",
       colour = "",
       title = "Surface Greens: Model vs Observed") +
  theme_classic() +
  theme(legend.position = "top")
#rmse
rmse <- sqrt(mean((compare_green$Model - compare_green$Observed)^2, 
                  na.rm = TRUE))
# print rmse
rmse
