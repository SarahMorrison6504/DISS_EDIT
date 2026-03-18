## plotting the corelation between % cover and seasonal averages
## for temp for example

library(ncdf4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ncdf4)

no <- get_var("baseline_output/output_baseline.nc", var_name = "temp", reference = "surface")
colnames(no)

# temp_2.10748098224222, temp_4.74183221004499

ten <- get_var("scenario_10_output/output_10.nc", var_name = "temp", reference = "surface")
colnames(ten)
# temp_2.10748778701908, temp_4.74184752079292

twenty <- get_var("scenario_20_output/output_20.nc", var_name = "temp", reference = "surface")
colnames(twenty)
# temp_2.10749459179594, temp_4.74186283154086

forty <- get_var("scenario_40_output/output_40.nc", var_name = "temp", reference = "surface")
colnames(forty)
# temp_2.10750820134965, temp_4.74189345303672

ninety <- get_var("scenario_90_output/output_90.nc", var_name = "temp", reference = "surface")
colnames(ninety)
# temp_2.10754222523386, temp_4.74197000677618

# trying again
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# List of your datasets
datasets <- list(
  no = no,
  ten = ten,
  twenty = twenty,
  forty = forty,
  ninety = ninety
)

# Function to rename temp columns based on their numeric value
rename_temps <- function(df) {
  colnames(df) <- sapply(colnames(df), function(x) {
    if (x == "DateTime") return("DateTime")
    if (x == "temp_0") return("surface")
    if (startsWith(x, "temp_2.1")) return("middle")
    if (startsWith(x, "temp_4.7")) return("bottom")
    return(x) # leave any other columns unchanged
  })
  return(df)

}


# Apply renaming to all datasets
datasets <- lapply(datasets, rename_temps)

# Add coverage levels
coverage_values <- c(no = 0, ten = 10, twenty = 20, forty = 40, ninety = 90)
datasets <- Map(function(df, cov) {
  df$coverage <- cov
  return(df)
}, datasets, coverage_values)

# Combine all datasets
all_data <- bind_rows(datasets)

# Make sure DateTime is POSIXct
all_data$DateTime <- as.POSIXct(all_data$DateTime, format = "%Y-%m-%d %H:%M:%S")
print(colnames(all_data))
# Add seasons
all_data <- all_data %>%
  mutate(
    season = case_when(
      month(DateTime) %in% c(12,1,2) ~ "Winter",
      month(DateTime) %in% c(3,4,5) ~ "Spring",
      month(DateTime) %in% c(6,7,8) ~ "Summer",
      month(DateTime) %in% c(9,10,11) ~ "Autumn"
    )
  )
colnames(all_data)

# Seasonal averages
seasonal_avg <- all_data %>%
  group_by(coverage, season) %>%
  summarise(
    surface = mean(surface, na.rm = TRUE),
    middle  = mean(middle, na.rm = TRUE),
    bottom  = mean(bottom, na.rm = TRUE),
    .groups = "drop"
  )

colnames(seasonal_avg)
# Convert to long format
seasonal_long <- seasonal_avg %>%
  pivot_longer(cols = c(surface, middle, bottom),
               names_to = "depth",
               values_to = "temperature")



# Plot
ggplot(seasonal_long, aes(x = coverage, y = temperature, colour = depth)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~season) +
  ylim(0,15)+
  scale_colour_manual(
    values = c(
      'surface' = '#339933',
      'middle' = '#ff9933',
      'bottom' = '#cc0000'),
    labels = c('Surface', 'Middle', 'Bottom'))+
  labs(
    x = "FPV Coverage (%)",
    y = "Seasonal Average Temperature (°C)",
    colour = "Depth",
    title = "Seasonal Temperature Response to FPV Coverage"
  ) +
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text( size = 12))
    
seasonal_stats <- all_data %>%
  pivot_longer(cols = c(surface, middle, bottom),
               names_to = 'depth',
               values_to = 'temperature') %>%
  group_by(coverage, season, depth) %>%
  summarise(
    mean_temp = mean(temperature, na.rm = TRUE),
    se_temp = sd(temperature, na.rm = TRUE)/sqrt(n()),
    .groups = 'drop')


# Plot
(ggplot(seasonal_stats, aes(x = coverage, y = mean_temp, colour = depth)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_temp - se_temp, ymax = mean_temp + se_temp),
                width = 2, size = 0.8) +
  facet_wrap(~season) +
  ylim(0,15)+
  scale_colour_manual(
    values = c(
      'surface' = '#339933',
      'middle' = '#ff9933',
      'bottom' = '#cc0000'),
    labels = c('Surface', 'Middle', 'Bottom'))+
  labs(
    x = "FPV Coverage (%)",
    y = "Seasonal Average Temperature (°C)",
    colour = "Depth",
    title = "Seasonal Temperature Response to FPV Coverage"
  ) +
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text( size = 12)))

## pearsons r corelation for FPV coverage and temperature for each
# depth and season
cor_results <- seasonal_long %>%
  group_by(season, depth) %>%
  summarise(
    pearson_r = cor(coverage, temperature, method = 'pearson'),
    .groups = 'drop')

print(cor_results)


# not seasonal but shows depths

cor_results <- seasonal_long %>%
  group_by(depth) %>%
  summarise(
    r = cor(coverage, temperature, method = "pearson"),
    .groups = "drop"
  )

print(cor_results)

# no depths,just corelation between FPV coverage and temp
cor_results <- seasonal_long %>%
  summarise(
    r = cor(coverage, temperature, method = "pearson"),
    .groups = "drop"
  )

print(cor_results)
