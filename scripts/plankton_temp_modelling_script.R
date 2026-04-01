## trying total means as opposed to at certain depths

setwd("C:/Users/Sarah/OneDrive/Dissertation/DISS(EDIT)/DISS_EDIT/aed/scenarios/scenario_outputs")

library(ncdf4)
library(ggplot2)
library(dplyr)


## looping for all files
extract_mean_temp <- function(file, scenario_name){
  nc <- nc_open(file)
  
  temp <- ncvar_get(nc, 'temp')
  time <- ncvar_get(nc, 'time')
  
  nc_close(nc)
  
  temp_mean <- colMeans(temp, na.rm = TRUE)
  
  dates <- as.POSIXct(time*3600,
                      origin = '2016-01-01 00:00:00',
                      tz= 'UTC')
  data.frame(
    date = dates,
    temp_mean = temp_mean,
    scenario = scenario_name
  )
}

files <-c('baseline_output/output_baseline.nc','scenario_10_output/output_10.nc', 'scenario_40_output/output_40.nc', 'scenario_90_output/output_90.nc')


results <- lapply(files, function(f) {
  scenario <- tools::file_path_sans_ext(basename(f))
  extract_mean_temp(f, scenario)
})
all_data <- bind_rows(results)
scenario_labels <- c(
  "output_baseline" = "A",
  "output_10"       = "B",
  "output_40"       = "C",
  "output_90"       = "D"
)

# Replace names in all_data
all_data$scenario <- scenario_labels[all_data$scenario]

head(all_data)
unique(all_data$scenario)

# finding seasonal peaks
# remove the initial period
all_data_filtered <- all_data %>%
  filter(date >= as.Date('2016-02-15'))

all_data_seasonal <- all_data_filtered %>%
  mutate(
    month = month(date),
    season = case_when(
      month %in% c(12,1,2) ~ 'Winter',
      month %in% c(3,4,5) ~ 'Spring',
      month %in% c(6,7,8) ~ 'Summer',
      month %in% c(9,10,11) ~ 'Autumn'
    ))
seasonal_peaks <- all_data_seasonal %>%
  group_by(scenario, season) %>%
  slice_max(order_by = temp_mean, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(scenario, season, date, temp_mean)

seasonal_peaks

#seasonal differences compared to baseline
seasonal_diff <- seasonal_peaks %>%
  group_by(season) %>%
  mutate(
    baseline = temp_mean[scenario == 'A'],
    diff_from_A = temp_mean - baseline
  ) %>%
  ungroup()

seasonal_diff


ggplot(all_data, aes(date, temp_mean, colour = scenario)) +
  geom_line(size = 1) +
  labs(
    x = "Date",
    y = "Mean Water Column Temperature (°C)",
    colour = "Scenario"
  ) +
  theme_classic()+
  theme(
    axis.title.x = element_text(face = 'bold', size = 10),
    axis.title.y = element_text(face = 'bold', size = 10))


#trying modelling
# linear mixed model with date as a fixed effect and season as a fixed effec
all_data$scenario <- factor(all_data$scenario,
                            levels = c('A', 'B', 'C', 'D'))

# plot histogram to look at distribtuion
hist(all_data$temp_mean)
# left skewed

# log transform
library(dplyr)
temp_log <- all_data %>%
  mutate(logTemp = log(temp_mean))

# add season as a variable
temp_log$season <- case_when(
  month(temp_log$date) %in% c(12,1,2) ~ 'winter',
  month(temp_log$date) %in% c(3,4,5) ~ 'spring',
  month(temp_log$date) %in% c(6,7,8) ~ 'summer',
  month(temp_log$date) %in% c(9,10,11) ~ 'autumn'
)

temp_log$season <- factor(temp_log$season)
model_log_season <- lmer(logTemp ~ scenario + season + (1|date), data = temp_log)
model_log_season_interaction <- lmer(logTemp ~ scenario*season + (1|date), data = temp_log)
model_log <- lmer(logTemp ~ scenario + (1|date), data = temp_log)

# compare aic
AIC(model_log)
AIC(model_log_season)
AIC(model_log_season_interaction)
summary(model_log)
summary(model_log_season)
summary(model_log_season_interaction)




# plot
scenario_palette <- c('')
ggplot(all_data, aes(date, temp_mean, colour = scenario)) +
  geom_line(size = 1) +
  labs(
    x = "Date",
    y = "Mean Water Column Temperature (°C)",
    colour = "Scenario"
  ) +
  theme_classic()+
  theme(
    axis.title.x = element_text(face = 'bold', size = 10),
    axis.title.y = element_text(face = 'bold', size = 10))



library(dplyr)

summary_stats <- all_data %>%
  group_by(scenario) %>%
  summarise(
    mean_temp = mean(temp_mean),
    sd_temp   = sd(temp_mean),
    se_temp = sd_temp/sqrt(365),
    min_temp  = min(temp_mean),
    max_temp  = max(temp_mean),
    median_temp = median(temp_mean)
  )

summary_stats

# for cyno(repeat code above)----
## looping for all files
extract_mean_cyno <- function(file, scenario_name){
  nc <- nc_open(file)
  
  cyno <- ncvar_get(nc, 'PHY_cyno')
  time <- ncvar_get(nc, 'time')
  
  nc_close(nc)
  
  cyno_mean <- colMeans(cyno, na.rm = TRUE)
  
  dates <- as.POSIXct(time*3600,
                      origin = '2016-01-01 00:00:00',
                      tz= 'UTC')
  data.frame(
    date = dates,
    cyno_mean = cyno_mean,
    scenario = scenario_name
  )
}

files <-c('baseline_output/output_baseline.nc','scenario_10_output/output_10.nc', 'scenario_40_output/output_40.nc', 'scenario_90_output/output_90.nc')


results <- lapply(files, function(f) {
  scenario <- tools::file_path_sans_ext(basename(f))
  extract_mean_cyno(f, scenario)
})
all_data <- bind_rows(results)


scenario_labels <- c(
  "output_baseline" = "A",
  "output_10"       = "B",
  "output_40"       = "C",
  "output_90"       = "D"
)

# Replace names in all_data
all_data$scenario <- scenario_labels[all_data$scenario]

head(all_data)

# linear mixed model with date as a fixed effect
all_data$scenario <- factor(all_data$scenario,
                     levels = c('A', 'B', 'C', 'D'))

# plot histogram to look at distribtuion
hist(all_data$cyno_mean)
# left skewed

# log transform
library(dplyr)
cyno_log <- all_data %>%
  mutate(logCyno = log(cyno_mean))

model_log <- lmer(logCyno ~ scenario + (1|date), data = cyno_log)
model_log_ineraction <- lmer(logCyno~scenario*date, data = cyno_log)
summary(model_log)

# exponenet to get reductions
#a-b
exp(-0.007)
# = 0.993 = -0.007%

#a-c
exp(-0.018)
# 0.982, = 0.018%

#a-d
exp(-0.0356)
# 0.965, = -0.035%
unique(all_data$scenario)

# plot
(cyno <- ggplot(all_data, aes(date, cyno_mean, colour = scenario)) +
  geom_line(size = 1) +
    ylim(0.2,0.5)+
  labs(
    x = "Date",
    y = "Cyanobacteria",
    colour = "Scenario"
  ) +
  theme_classic()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = 'bold', size = 10, vjust = 0.5, hjust = 0.5, angle = 0 ),
    axis.text.x = element_blank(),
    legend.position = 'none'))



library(dplyr)

summary_stats <- all_data %>%
  group_by(scenario) %>%
  summarise(
    mean_temp = mean(temp_mean),
    sd_temp   = sd(temp_mean),
    se_temp = sd_temp/sqrt(365),
    min_temp  = min(temp_mean),
    max_temp  = max(temp_mean),
    median_temp = median(temp_mean)
  )

summary_stats

### diat----
extract_mean_diat <- function(file, scenario_name){
  nc <- nc_open(file)
  
  diat <- ncvar_get(nc, 'PHY_diat')
  time <- ncvar_get(nc, 'time')
  
  nc_close(nc)
  
  diat_mean <- colMeans(diat, na.rm = TRUE)
  
  dates <- as.POSIXct(time*3600,
                      origin = '2016-01-01 00:00:00',
                      tz= 'UTC')
  data.frame(
    date = dates,
    diat_mean = diat_mean,
    scenario = scenario_name
  )
}

files <-c('baseline_output/output_baseline.nc','scenario_10_output/output_10.nc', 'scenario_40_output/output_40.nc', 'scenario_90_output/output_90.nc')


results <- lapply(files, function(f) {
  scenario <- tools::file_path_sans_ext(basename(f))
  extract_mean_diat(f, scenario)
})
all_data <- bind_rows(results)
scenario_labels <- c(
  "output_baseline" = "A",
  "output_10"       = "B",
  "output_40"       = "C",
  "output_90"       = "D"
)

# Replace names in all_data
all_data$scenario <- scenario_labels[all_data$scenario]

head(all_data)
unique(all_data$scenario)

# linear mixed model with date as a fixed effect
all_data$scenario <- factor(all_data$scenario,
                            levels = c('A', 'B', 'C', 'D'))

# plot histogram to look at distribtuion
hist(all_data$diat_mean)
diat_log <- all_data %>%
  mutate(logDiat = log(diat_mean))

model_log <- lmer(logDiat ~ scenario + (1|date), data = diat_log)

summary(model_log)

# exponent
exp(-0.004)
1-0.996
# 0.004

exp(-0.07)
1-0.9323
# 0.0677

exp(0.083)
1.08-1
# 8% increaes
# plot
scenario_palette <- c('')
(diat <- ggplot(all_data, aes(date, diat_mean, colour = scenario)) +
  geom_line(size = 1) +
  labs(
    x = "Date",
    y = "Diatom",
    colour = "Scenario"
  ) +
  theme_classic()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(face = 'bold', size = 10, vjust = 0.5, hjust = 0.5, angle = 0)))

summary_stats <- all_data %>%
  group_by(scenario) %>%
  summarise(
    mean_diat = mean(diat_mean),
    sd_diat   = sd(diat_mean),
    se_diat = sd_diat/sqrt(365),
    min_diat  = min(diat_mean),
    max_diat  = max(diat_mean),
    median_diat = median(diat_mean)
  )

summary_stats
## bga----
extract_mean_bga <- function(file, scenario_name){
  nc <- nc_open(file)
  
  bga <- ncvar_get(nc, 'PHY_bga')
  time <- ncvar_get(nc, 'time')
  
  nc_close(nc)
  
  bga_mean <- colMeans(bga, na.rm = TRUE)
  
  dates <- as.POSIXct(time*3600,
                      origin = '2016-01-01 00:00:00',
                      tz= 'UTC')
  data.frame(
    date = dates,
    bga_mean = bga_mean,
    scenario = scenario_name
  )
}

files <-c('baseline_output/output_baseline.nc','scenario_10_output/output_10.nc', 'scenario_40_output/output_40.nc', 'scenario_90_output/output_90.nc')


results <- lapply(files, function(f) {
  scenario <- tools::file_path_sans_ext(basename(f))
  extract_mean_bga(f, scenario)
})

all_data <- bind_rows(results)
scenario_labels <- c(
  "output_baseline" = "A",
  "output_10"       = "B",
  "output_40"       = "C",
  "output_90"       = "D"
)

# Replace names in all_data
all_data$scenario <- scenario_labels[all_data$scenario]

head(all_data)
unique(all_data$scenario)

# plot
(bga <- ggplot(all_data, aes(date, bga_mean, colour = scenario)) +
  geom_line(size = 1) +
  ylim(0.3,0.5)+
  labs(
    x = "Date",
    y = "Blue-Green Algae",
    colour = "Scenario"
  ) +
  theme_classic()+
  theme(
    axis.title.y = element_text(face = 'bold', size = 10, vjust = 0.5, hjust = 0.5, angle = 0),
    axis.title.x = element_blank(),
    legend.position = 'none'))

summary_stats <- all_data %>%
  group_by(scenario) %>%
  summarise(
    mean_bga = mean(bga_mean),
    sd_bga   = sd(bga_mean),
    se_bga = sd_bga/sqrt(365),
    min_bga  = min(bga_mean),
    max_bga  = max(bga_mean),
    median_bga = median(bga_mean)
  )

summary_stats
### green----
extract_mean_green <- function(file, scenario_name){
  nc <- nc_open(file)
  
  green <- ncvar_get(nc, 'PHY_green')
  time <- ncvar_get(nc, 'time')
  
  nc_close(nc)
  
  green_mean <- colMeans(green, na.rm = TRUE)
  
  dates <- as.POSIXct(time*3600,
                      origin = '2016-01-01 00:00:00',
                      tz= 'UTC')
  data.frame(
    date = dates,
    green_mean = green_mean,
    scenario = scenario_name
  )
}

files <-c('baseline_output/output_baseline.nc','scenario_10_output/output_10.nc', 'scenario_40_output/output_40.nc', 'scenario_90_output/output_90.nc')


results <- lapply(files, function(f) {
  scenario <- tools::file_path_sans_ext(basename(f))
  extract_mean_green(f, scenario)
})
all_data <- bind_rows(results)
scenario_labels <- c(
  "output_baseline" = "A",
  "output_10"       = "B",
  "output_40"       = "C",
  "output_90"       = "D"
)

# Replace names in all_data
all_data$scenario <- scenario_labels[all_data$scenario]

head(all_data)
unique(all_data$scenario)

# linear mixed model
all_data$scenario <- factor(all_data$scenario,
                            levels = c('A', 'B', 'C', 'D'))

# plot histogram to look at distribtuion
hist(all_data$green_mean)
# left skewed

# log transform
library(dplyr)
green_log <- all_data %>%
  mutate(logGreen = log(green_mean))

model_log <- lmer(logGreen ~ scenario + (1|date), data = green_log)

summary(model_log)

# exponenting
exp(-0.025)
1-0.975
# =0.025%

exp(-0.35)
1-0.7
# -30%

exp(-1.99)
1-0.14
# 86%
# plot
(green <- ggplot(all_data, aes(date, green_mean, colour = scenario)) +
  geom_line(size = 1) +
  labs(
    x = "Date",
    y = "Green Algae",
    colour = "Scenario"
  ) +
  theme_classic()+
  theme(
    axis.title.x = element_text(face = 'bold', size = 10),
    axis.title.y = element_text(face = 'bold', size = 10, hjust = 0.5, vjust = 0.5, angle = 0),
    legend.position = 'none'))

summary_stats <- all_data %>%
  group_by(scenario) %>%
  summarise(
    mean_green = mean(green_mean),
    sd_green   = sd(green_mean),
    se_green = sd_green/sqrt(365),
    min_green  = min(green_mean),
    max_green  = max(green_mean),
    median_green = median(green_mean)
  )

summary_stats

library(patchwork)

cyno <- cyno + theme(axis.title.x = element_blank())
diat  <- diat  + theme(axis.title.x = element_blank())

# Keep x-axis title only on bottom row
bga   <- bga   + theme(axis.title.x = element_text(face = "bold", size = 10, hjust = 0.5))
green   <- green   + theme(axis.title.x = element_text(face = "bold", size = 10, hjust = 0.5))

panel <- (cyno)/(diat)/(green)
(panel <- panel + plot_annotation(
  title = 'Mean Water Column Phytplankton Concentrations by Functional Group (mmol/m³)',
  subtitle = '',
  theme(plot.title = element_text(size = 12))
))
panel

 # (mmol/m³)
