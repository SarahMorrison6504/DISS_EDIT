# plotting empirical data vs modelled and calculating rmse for:
# nitrate, soluble phosphorus, surface temp


rm(list = ls())
setwd('C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT')

## NITRATE----

# get modelled surface values

ncfile <- 'aed/output/output.nc'
model_nit <- get_var(ncfile, var_name = 'NIT_nit', reference = 'surface')
model_nit <- model_nit[, c('DateTime', 'NIT_nit_0')] # keep only necessary columns
model_nit$Date <- as.Date(model_nit$DateTime) # drop time
model_nit$DateTime <- NULL

# clean obs data
obs_nit <- read.csv("calibration/nit_obs.csv")

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
ncfile <- "aed/output/output.nc"   

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

# merge
compare_phs <- merge(model_phs, obs_phs, by = "Date")

# Rename columns for clarity
colnames(compare_phs) <- c("Date", "Model", "Observed")

ggplot(compare_phs, aes(x = Date)) +
  geom_line(aes(y = Model, colour = "Model"), linewidth = 1) +
  geom_point(aes(y = Observed, colour = "Observed"), size = 2) +
  scale_colour_manual(values = c("Model" = "blue",
                                 "Observed" = "red")) +
  labs(y = "Surface PHS_frp (mmol/m3)",
       colour = "",
       title = "Surface Soluble Phosphorus: Model vs Observed") +
  theme_minimal() +
  theme(legend.position = "top")

# get rmse
rmse <- sqrt(mean((compare_phs$Model - compare_phs$Observed)^2, 
                  na.rm = TRUE))
# print rmse
rmse

## FOR SURFACE TEMP ----
ncfile <- "aed/output/output.nc"

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

# plot
ggplot(compare_temp, aes(x = Date)) +
  geom_line(aes(y = Model, colour = "Model"), linewidth = 1) +
  geom_point(aes(y = Observed, colour = "Observed"), size = 2) +
  scale_colour_manual(values = c("Model" = "blue",
                                 "Observed" = "red")) +
  labs(y = "Surface Temperature (°C)",
       colour = "",
       title = "Surface Temperature: Model vs Observed") +
  theme_minimal() +
  theme(legend.position = "top")

#rmse
rmse <- sqrt(mean((compare_temp$Model - compare_temp$Observed)^2, 
                na.rm = TRUE))
# print rmse
rmse



