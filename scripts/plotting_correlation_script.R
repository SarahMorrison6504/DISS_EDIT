## plotting the correlation between % cover and seasonal averages

library(ncdf4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glmtools)
library(lubridate)
library(ncdf4)

setwd()
# for temp----
no <- get_var("baseline_output/output_baseline.nc", var_name = "temp", reference = "surface")
colnames(no)

# temp_2.10748098224222, temp_4.74183221004499

ten <- get_var("scenario_10_output/output_10.nc", var_name = "temp", reference = "surface")
colnames(ten)
# temp_2.10748778701908, temp_4.74184752079292

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

# List of datasets
datasets <- list(
  no = no,
  ten = ten,
  forty = forty,
  ninety = ninety
)

# Function to rename temp columns based on their numeric value
rename_temps <- function(df) {
  colnames(df) <- sapply(colnames(df), function(x) {
    if (x == "DateTime") return("DateTime")
    if (x == "temp_0") return("Surface")
    if (startsWith(x, "temp_2.1")) return("Middle")
    if (startsWith(x, "temp_4.7")) return("Bottom")
    return(x) # leave any other columns unchanged
  })
  return(df)

}



# Apply renaming to all datasets
datasets <- lapply(datasets, rename_temps)

# Add coverage levels
coverage_values <- c(no = 0, ten = 10, forty = 40, ninety = 90)
datasets <- Map(function(df, cov) {
  df$coverage <- cov
  return(df)
}, datasets, coverage_values)

# Combine all datasets
all_data <- bind_rows(datasets)

# Make sure DateTime is POSIXct
all_data$DateTime <- as.POSIXct(all_data$DateTime, format = "%Y-%m-%d %H:%M:%S")
print(colnames(all_data))
# Seasonal averages
# only select the columns at the depths we are looking at
all_data_clean <- all_data%>%
  select(coverage, Surface, Middle, Bottom)

# get annual mean values at each depth
all_data_avg <- all_data_clean %>%
  group_by(coverage) %>%
  summarise(
    Surface = mean(Surface, na.rm = TRUE),
    Middle  = mean(Middle, na.rm = TRUE),
    Bottom  = mean(Bottom, na.rm = TRUE),
    .groups = "drop"
  )
# make long format
all_data_long <- all_data_avg %>%
  pivot_longer(
    cols = c(Surface, Middle, Bottom),
    names_to = "depth",
    values_to = "Temperature"
  )


library(ggplot2)
# Plot
(temp_cover <- ggplot(all_data_long, aes(x = coverage, y = Temperature, colour = depth)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    ylim(0,15)+
    scale_colour_manual(
      values = c(
        'Surface' = 'skyblue',
        'Middle' = '#619cff',
        'Bottom' = 'darkblue'))+
    scale_x_continuous(breaks = seq(0,90, by = 10))+
    scale_y_continuous(breaks = seq(0,15, by = 2))+
    labs(
      x = "FPV Coverage (%)",
      y = "Mean Annual Water Temperature (°C)",
      colour = "Depth",
    ) +
    theme_classic()+
    theme(strip.background = element_blank(),
          strip.text = element_text( size = 12),
          axis.title = element_text(face = 'bold')))


## pearsons r corelation for FPV coverage and temperature for each
# depth 
cor_results_temp <- all_data_long %>%
  group_by(depth)%>%
  summarise(
    pearson_r = cor(coverage, Temperature, method = 'pearson'),
    .groups = 'drop'
  )

cor_results_temp



# for DO----
no <- get_var("baseline_output/output_baseline.nc", var_name = "OXY_oxy", reference = "surface")
colnames(no)

# temp_2.10748098224222, temp_4.74183221004499

ten <- get_var("scenario_10_output/output_10.nc", var_name = "OXY_oxy", reference = "surface")
colnames(ten)
# temp_2.10748778701908, temp_4.74184752079292

forty <- get_var("scenario_40_output/output_40.nc", var_name = "OXY_oxy", reference = "surface")
colnames(forty)
# temp_2.10750820134965, temp_4.74189345303672

ninety <- get_var("scenario_90_output/output_90.nc", var_name = "OXY_oxy", reference = "surface")
colnames(ninety)
# temp_2.10754222523386, temp_4.74197000677618

# List of your datasets
datasets <- list(
  no = no,
  ten = ten,
  forty = forty,
  ninety = ninety
)
# Function to rename temp columns based on their numeric value
rename_temps <- function(df) {
  colnames(df) <- sapply(colnames(df), function(x) {
    if (x == "DateTime") return("DateTime")
    if (x == "OXY_oxy_0") return("Surface")
    if (startsWith(x, "OXY_oxy_2.1")) return("Middle")
    if (startsWith(x, "OXY_oxy_4.7")) return("Bottom")
    return(x) # leave any other columns unchanged
  })
  return(df)
  
}


# Apply renaming to all datasets
datasets <- lapply(datasets, rename_temps)

# Add coverage levels
coverage_values <- c(no = 0, ten = 10, forty = 40, ninety = 90)
datasets <- Map(function(df, cov) {
  df$coverage <- cov
  return(df)
}, datasets, coverage_values)

# Combine all datasets
all_data <- bind_rows(datasets)

# Make sure DateTime is POSIXct
all_data$DateTime <- as.POSIXct(all_data$DateTime, format = "%Y-%m-%d %H:%M:%S")
print(colnames(all_data))

# only select the columns at the depths we are looking at
all_data_clean <- all_data%>%
  select(coverage, Surface, Middle, Bottom)

# get annual mean values at each depth
all_data_avg <- all_data_clean %>%
  group_by(coverage) %>%
  summarise(
    Surface = mean(Surface, na.rm = TRUE),
    Middle  = mean(Middle, na.rm = TRUE),
    Bottom  = mean(Bottom, na.rm = TRUE),
    .groups = "drop"
  )
# make long format
all_data_long <- all_data_avg %>%
  pivot_longer(
    cols = c(Surface, Middle, Bottom),
    names_to = "depth",
    values_to = "DO"
  )

(do_cover <- ggplot(all_data_long, aes(x = coverage, y = DO, colour = depth)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    ylim(350,415)+
    scale_colour_manual(
      values = c(
        'Surface' = 'skyblue',
        'Middle' = '#619cff',
        'Bottom' = 'darkblue'))+
    scale_x_continuous(breaks = seq(0,90, by = 10))+
    scale_y_continuous(breaks = seq(350,415, by = 10))+
    labs(
      x = "FPV Coverage (%)",
      y = "Mean Annual Dissolved Oxygen Concentration (mmol/m³)",
      colour = "Depth",
    ) +
    theme_classic()+
    theme(strip.background = element_blank(),
          strip.text = element_text( size = 12),
          axis.title = element_text(face = 'bold')))




all_data_long
# save as high quality png
ggsave(filename = 'DO_vs_Coverage.png',
       path = 'correlation_plots',
       width = 8, height = 6,
       dpi = 600)

# correlation stat
cor_results_do <- all_data_long %>%
  group_by(depth)%>%
  summarise(
    pearson_r = cor(coverage, DO, method = 'pearson'),
    .groups = 'drop'
  )

cor_results_do





# nit----
no <- get_var("baseline_output/output_baseline.nc", var_name = "NIT_nit", reference = "surface")
colnames(no)

# temp_2.10748098224222, temp_4.74183221004499

ten <- get_var("scenario_10_output/output_10.nc", var_name = "NIT_nit", reference = "surface")
colnames(ten)
# temp_2.10748778701908, temp_4.74184752079292

forty <- get_var("scenario_40_output/output_40.nc", var_name = "NIT_nit", reference = "surface")
colnames(forty)
# temp_2.10750820134965, temp_4.74189345303672

ninety <- get_var("scenario_90_output/output_90.nc", var_name = "NIT_nit", reference = "surface")
colnames(ninety)
# temp_2.10754222523386, temp_4.74197000677618

# List of your datasets
datasets <- list(
  no = no,
  ten = ten,
  forty = forty,
  ninety = ninety
)
# Function to rename temp columns based on their numeric value
rename_nit <- function(df) {
  colnames(df) <- sapply(colnames(df), function(x) {
    if (x == "DateTime") return("DateTime")
    if (x == "NIT_nit_0") return("Surface")
    if (startsWith(x, "NIT_nit_2.1")) return("Middle")
    if (startsWith(x, "NIT_nit_4.7")) return("Bottom")
    return(x) # leave any other columns unchanged
  })
  return(df)
  
}


# Apply renaming to all datasets
datasets <- lapply(datasets, rename_nit)

# Add coverage levels
coverage_values <- c(no = 0, ten = 10, forty = 40, ninety = 90)
datasets <- Map(function(df, cov) {
  df$coverage <- cov
  return(df)
}, datasets, coverage_values)

# Combine all datasets
all_data <- bind_rows(datasets)

# Make sure DateTime is POSIXct
all_data$DateTime <- as.POSIXct(all_data$DateTime, format = "%Y-%m-%d %H:%M:%S")
print(colnames(all_data))

# only select the columns at the depths we are looking at
all_data_clean <- all_data%>%
  select(coverage, Surface, Middle, Bottom)

# get annual mean values at each depth
all_data_avg <- all_data_clean %>%
  group_by(coverage) %>%
  summarise(
    Surface = mean(surface, na.rm = TRUE),
    Middle  = mean(middle, na.rm = TRUE),
    Bottom  = mean(bottom, na.rm = TRUE),
    .groups = "drop"
  )
# make long format
all_data_long <- all_data_avg %>%
  pivot_longer(
    cols = c(Surface, Middle, Bottom),
    names_to = "depth",
    values_to = "NO3"
  )


# Plot
(nit_cover <- ggplot(all_data_long, aes(x = coverage, y = NO3, colour = depth)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    ylim(0,200)+
    scale_colour_manual(
      values = c(
        'Surface' = 'skyblue',
        'Middle' = '#619cff',
        'Bottom' = 'darkblue'))+
    scale_x_continuous(breaks = seq(0,90, by = 10))+
    scale_y_continuous(breaks = seq(100,200, by = 10))+
    labs(
      x = "FPV Coverage (%)",
      y = "Mean Annual Nitrate Concentration (mmol/m³)",
      colour = "Depth",
    ) +
    theme_classic()+
    theme(strip.background = element_blank(),
          strip.text = element_text( size = 12),
          axis.title = element_text(face = 'bold')))

nit_cover
# save as high quality png
ggsave(filename = 'nit_vs_Coverage.png',
       path = 'correlation_plots',
       width = 8, height = 6,
       dpi = 600)

# correlation stat
cor_results_nit <- all_data_long %>%
  group_by(depth)%>%
  summarise(
    pearson_r = cor(coverage, NO3, method = 'pearson'),
    .groups = 'drop'
  )


# for phs----
no <- get_var("baseline_output/output_baseline.nc", var_name = "PHS_frp", reference = "surface")
colnames(no)

# temp_2.10748098224222, temp_4.74183221004499

ten <- get_var("scenario_10_output/output_10.nc", var_name = "PHS_frp", reference = "surface")
colnames(ten)
# temp_2.10748778701908, temp_4.74184752079292

forty <- get_var("scenario_40_output/output_40.nc", var_name = "PHS_frp", reference = "surface")
colnames(forty)
# temp_2.10750820134965, temp_4.74189345303672

ninety <- get_var("scenario_90_output/output_90.nc", var_name = "PHS_frp", reference = "surface")
colnames(ninety)
# temp_2.10754222523386, temp_4.74197000677618

# List of your datasets
datasets <- list(
  no = no,
  ten = ten,
  forty = forty,
  ninety = ninety
)
# Function to rename temp columns based on their numeric value
rename_phs <- function(df) {
  colnames(df) <- sapply(colnames(df), function(x) {
    if (x == "DateTime") return("DateTime")
    if (x == "PHS_frp_0") return("surface")
    if (startsWith(x, "PHS_frp_2.1")) return("middle")
    if (startsWith(x, "PHS_frp_4.7")) return("bottom")
    return(x) # leave any other columns unchanged
  })
  return(df)
  
}


# Apply renaming to all datasets
datasets <- lapply(datasets, rename_phs)

# Add coverage levels
coverage_values <- c(no = 0, ten = 10, forty = 40, ninety = 90)
datasets <- Map(function(df, cov) {
  df$coverage <- cov
  return(df)
}, datasets, coverage_values)

# Combine all datasets
all_data <- bind_rows(datasets)

# Make sure DateTime is POSIXct
all_data$DateTime <- as.POSIXct(all_data$DateTime, format = "%Y-%m-%d %H:%M:%S")
print(colnames(all_data))

# only select the columns at the depths we are looking at
all_data_clean <- all_data%>%
  select(coverage, surface, middle, bottom)

# get annual mean values at each depth
all_data_avg <- all_data_clean %>%
  group_by(coverage) %>%
  summarise(
    Surface = mean(surface, na.rm = TRUE),
    Middle  = mean(middle, na.rm = TRUE),
    Bottom  = mean(bottom, na.rm = TRUE),
    .groups = "drop"
  )
# make long format
all_data_long <- all_data_avg %>%
  pivot_longer(
    cols = c(Surface, Middle, Bottom),
    names_to = "depth",
    values_to = "PHS"
  )


# Plot
(phs_cover <- ggplot(all_data_long, aes(x = coverage, y = PHS, colour = depth)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    ylim(0,0.5)+
    scale_colour_manual(
      values = c(
        'Surface' = 'skyblue',
        'Middle' = '#619cff',
        'Bottom' = 'darkblue'))+
    scale_x_continuous(breaks = seq(0,90, by = 10))+
    scale_y_continuous(breaks = seq(0,0.5, by = 0.05))+
    labs(
      x = "FPV Coverage (%)",
      y = "Mean Annual Soluble Reactive Phosphorus Concentration (mmol/m³)",
      colour = "Depth",
    ) +
    theme_classic()+
    theme(strip.background = element_blank(),
          strip.text = element_text( size = 12),
          axis.title = element_text(face = 'bold')))
all_data_long

# save as high quality png
ggsave(filename = 'phs_vs_Coverage.png',
       plot = phs_cover,
       path = 'correlation_plots',
       width = 8, height = 6,
       dpi = 600)

# correlation stat
cor_results_phs <- all_data_long %>%
  group_by(depth)%>%
  summarise(
    pearson_r = cor(coverage, PHS, method = 'pearson'),
    .groups = 'drop'
  )

cor_results_phs


# for cyno----
no <- get_var("baseline_output/output_baseline.nc", var_name = "PHY_cyno", reference = "surface")
colnames(no)

# temp_2.10748098224222, temp_4.74183221004499

ten <- get_var("scenario_10_output/output_10.nc", var_name = "PHY_cyno", reference = "surface")
colnames(ten)
# temp_2.10748778701908, temp_4.74184752079292

forty <- get_var("scenario_40_output/output_40.nc", var_name = "PHY_cyno", reference = "surface")
colnames(forty)
# temp_2.10750820134965, temp_4.74189345303672

ninety <- get_var("scenario_90_output/output_90.nc", var_name = "PHY_cyno", reference = "surface")
colnames(ninety)
# temp_2.10754222523386, temp_4.74197000677618

# List of your datasets
datasets <- list(
  no = no,
  ten = ten,
  forty = forty,
  ninety = ninety
)
# Function to rename temp columns based on their numeric value
rename_cyno <- function(df) {
  colnames(df) <- sapply(colnames(df), function(x) {
    if (x == "DateTime") return("DateTime")
    if (x == "PHY_cyno_0") return("surface")
    if (startsWith(x, "PHY_cyno_2.1")) return("middle")
    if (startsWith(x, "PHY_cyno_4.7")) return("bottom")
    return(x) # leave any other columns unchanged
  })
  return(df)
  
}


# Apply renaming to all datasets
datasets <- lapply(datasets, rename_cyno)

# Add coverage levels
coverage_values <- c(no = 0, ten = 10, forty = 40, ninety = 90)
datasets <- Map(function(df, cov) {
  df$coverage <- cov
  return(df)
}, datasets, coverage_values)

# Combine all datasets
all_data <- bind_rows(datasets)

# Make sure DateTime is POSIXct
all_data$DateTime <- as.POSIXct(all_data$DateTime, format = "%Y-%m-%d %H:%M:%S")
print(colnames(all_data))

# only select the columns at the depths we are looking at
all_data_clean <- all_data%>%
  select(coverage, surface, middle, bottom)

# get annual mean values at each depth
all_data_avg <- all_data_clean %>%
  group_by(coverage) %>%
  summarise(
    Surface = mean(surface, na.rm = TRUE),
    Middle  = mean(middle, na.rm = TRUE),
    Bottom  = mean(bottom, na.rm = TRUE),
    .groups = "drop"
  )
# make long format
all_data_long <- all_data_avg %>%
  pivot_longer(
    cols = c(Surface, Middle, Bottom),
    names_to = "depth",
    values_to = "CYNO"
  )


# Plot
(cyno_cover <- ggplot(all_data_long, aes(x = coverage, y = CYNO, colour = depth)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    ylim(0.23,0.32)+
    scale_colour_manual(
      values = c(
        'Surface' = 'skyblue',
        'Middle' = '#619cff',
        'Bottom' = 'darkblue'))+
    scale_x_continuous(breaks = seq(0,90, by = 10))+
    scale_y_continuous(breaks = seq(0.23,0.32, by = 0.01))+
    labs(
      x = "FPV Coverage (%)",
      y = "Cyanobacteria 
      (mmol/m³)",
      title = "Mean Annual Phytoplankton Concentration by Functional Group (mmol/m³)",
      colour = "Depth",
    ) +
    theme_classic()+
    theme(strip.background = element_blank(),
          strip.text = element_text( size = 12),
          axis.title.y = element_text(angle = 0, vjust = 0.5, hjust=0.5),
          legend.position = 'none',
          axis.title = element_text(face = 'bold'),
          axis.text.x = element_blank(),
          axis.title.x = element_blank()))


# save as high quality png
ggsave(filename = 'CYNO_vs_Coverage.png',
       path = 'correlation_plots',
       width = 8, height = 6,
       dpi = 600)

# correlation stat
cor_results_cyno <- all_data_long %>%
  group_by(depth)%>%
  summarise(
    pearson_r = cor(coverage, CYNO, method = 'pearson'),
    .groups = 'drop'
  )
cor_results_cyno

# for diat----
no <- get_var("baseline_output/output_baseline.nc", var_name = "PHY_diat", reference = "surface")
colnames(no)

# temp_2.10748098224222, temp_4.74183221004499

ten <- get_var("scenario_10_output/output_10.nc", var_name = "PHY_diat", reference = "surface")
colnames(ten)
# temp_2.10748778701908, temp_4.74184752079292


forty <- get_var("scenario_40_output/output_40.nc", var_name = "PHY_diat", reference = "surface")
colnames(forty)
# temp_2.10750820134965, temp_4.74189345303672

ninety <- get_var("scenario_90_output/output_90.nc", var_name = "PHY_diat", reference = "surface")
colnames(ninety)
# temp_2.10754222523386, temp_4.74197000677618

# List of your datasets
datasets <- list(
  no = no,
  ten = ten,
  forty = forty,
  ninety = ninety
)
# Function to rename temp columns based on their numeric value
rename_diat <- function(df) {
  colnames(df) <- sapply(colnames(df), function(x) {
    if (x == "DateTime") return("DateTime")
    if (x == "PHY_diat_0") return("surface")
    if (startsWith(x, "PHY_diat_2.1")) return("middle")
    if (startsWith(x, "PHY_diat_4.7")) return("bottom")
    return(x) # leave any other columns unchanged
  })
  return(df)
  
}


# Apply renaming to all datasets
datasets <- lapply(datasets, rename_diat)

# Add coverage levels
coverage_values <- c(no = 0, ten = 10, forty = 40, ninety = 90)
datasets <- Map(function(df, cov) {
  df$coverage <- cov
  return(df)
}, datasets, coverage_values)

# Combine all datasets
all_data <- bind_rows(datasets)

# Make sure DateTime is POSIXct
all_data$DateTime <- as.POSIXct(all_data$DateTime, format = "%Y-%m-%d %H:%M:%S")
print(colnames(all_data))

# only select the columns at the depths we are looking at
all_data_clean <- all_data%>%
  select(coverage, surface, middle, bottom)

# get annual mean values at each depth
all_data_avg <- all_data_clean %>%
  group_by(coverage) %>%
  summarise(
    Surface = mean(surface, na.rm = TRUE),
    Middle  = mean(middle, na.rm = TRUE),
    Bottom  = mean(bottom, na.rm = TRUE),
    .groups = "drop"
  )
# make long format
all_data_long <- all_data_avg %>%
  pivot_longer(
    cols = c(Surface, Middle, Bottom),
    names_to = "depth",
    values_to = "DIAT"
  )


# Plot
(diat_cover <- ggplot(all_data_long, aes(x = coverage, y = DIAT, colour = depth)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    ylim(0,10)+
    scale_colour_manual(
      values = c(
        'Surface' = 'skyblue',
        'Middle' = '#619cff',
        'Bottom' = 'darkblue'))+
    scale_x_continuous(breaks = seq(0,90, by = 10))+
    scale_y_continuous(breaks = seq(0,10, by = 1))+
    labs(
      x = "FPV Coverage (%)",
      y = "Diatom
      (mmol/m³)",
      colour = "Depth",
    ) +
    theme_classic()+
    theme(strip.background = element_blank(),
          strip.text = element_text( size = 12),
          axis.title.y = element_text(angle = 0, vjust = 0.5, hjust=0.5),
          axis.title = element_text(face = 'bold'),
          axis.text.x = element_blank(),
          axis.title.x = element_blank()))


# save as high quality png
ggsave(filename = 'diat_vs_Coverage.png',
       path = 'correlation_plots',
       width = 8, height = 6,
       dpi = 600)

# correlation stat
cor_results_diat <- all_data_long %>%
  group_by(depth)%>%
  summarise(
    pearson_r = cor(coverage, DIAT, method = 'pearson'),
    .groups = 'drop'
  )
cor_results_diat
# for greens----
no <- get_var("baseline_output/output_baseline.nc", var_name = "PHY_green", reference = "surface")
colnames(no)

# temp_2.10748098224222, temp_4.74183221004499

ten <- get_var("scenario_10_output/output_10.nc", var_name = "PHY_green", reference = "surface")
colnames(ten)
# temp_2.10748778701908, temp_4.74184752079292

forty <- get_var("scenario_40_output/output_40.nc", var_name = "PHY_green", reference = "surface")
colnames(forty)
# temp_2.10750820134965, temp_4.74189345303672

ninety <- get_var("scenario_90_output/output_90.nc", var_name = "PHY_green", reference = "surface")
colnames(ninety)
# temp_2.10754222523386, temp_4.74197000677618

# List of your datasets
datasets <- list(
  no = no,
  ten = ten,
  forty = forty,
  ninety = ninety
)
# Function to rename temp columns based on their numeric value
rename_green <- function(df) {
  colnames(df) <- sapply(colnames(df), function(x) {
    if (x == "DateTime") return("DateTime")
    if (x == "PHY_green_0") return("surface")
    if (startsWith(x, "PHY_green_2.1")) return("middle")
    if (startsWith(x, "PHY_green_4.7")) return("bottom")
    return(x) # leave any other columns unchanged
  })
  return(df)
  
}


# Apply renaming to all datasets
datasets <- lapply(datasets, rename_green)

# Add coverage levels
coverage_values <- c(no = 0, ten = 10, forty = 40, ninety = 90)
datasets <- Map(function(df, cov) {
  df$coverage <- cov
  return(df)
}, datasets, coverage_values)

# Combine all datasets
all_data <- bind_rows(datasets)

# Make sure DateTime is POSIXct
all_data$DateTime <- as.POSIXct(all_data$DateTime, format = "%Y-%m-%d %H:%M:%S")
print(colnames(all_data))

# only select the columns at the depths we are looking at
all_data_clean <- all_data%>%
  select(coverage, surface, middle, bottom)

# get annual mean values at each depth
all_data_avg <- all_data_clean %>%
  group_by(coverage) %>%
  summarise(
    surface = mean(surface, na.rm = TRUE),
    middle  = mean(middle, na.rm = TRUE),
    bottom  = mean(bottom, na.rm = TRUE),
    .groups = "drop"
  )
# make long format
all_data_long <- all_data_avg %>%
  pivot_longer(
    cols = c(surface, middle, bottom),
    names_to = "depth",
    values_to = "GREEN"
  )


# Plot
(green_cover <- ggplot(all_data_long, aes(x = coverage, y = GREEN, colour = depth)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    ylim(0,50)+
    scale_colour_manual(
      values = c(
        'surface' = 'skyblue',
        'middle' = '#619cff',
        'bottom' = 'darkblue'))+
    scale_x_continuous(breaks = seq(0,90, by = 10))+
    scale_y_continuous(breaks = seq(0, 50, by = 5))+
    labs(
      x = "FPV Coverage (%)",
      y = "Green Algae
      (mmol/m³)",
      colour = "Depth",
    ) +
    theme_classic()+
    theme(strip.background = element_blank(),
          legend.position = 'none',
          strip.text = element_text( size = 12),
          axis.title.y = element_text(angle = 0, vjust = 0.5, hjust=0.5),
          axis.title = element_text(face = 'bold')))



# save as high quality png
ggsave(filename = 'green_vs_Coverage.png',
       path = 'correlation_plots',
       width = 8, height = 6,
       dpi = 600)

# correlation stat
cor_results_green <- all_data_long %>%
  group_by(depth)%>%
  summarise(
    pearson_r = cor(coverage, GREEN, method = 'pearson'),
    .groups = 'drop'
  )
cor_results_green

## panel plot for cyno----
library(patchwork)

panel <- (cyno_cover/diat_cover/green_cover) + plot_layout(guides = 'collect')
panel
ggsave(filename = 'plankon_cor_panel.png',
       path= 'correlation_plots',
       width = 10, height = 12,
       dpi = 600) 


## trying something else

library(ncdf4)
getwd()

nc <- nc_open("baseline_output/output_baseline.nc")

temp  <- ncvar_get(nc, "temp")   # temperature array
depth <- ncvar_get(nc, "z")      # depth
time  <- ncvar_get(nc, "time")

nc_close(nc)

depth_matrix <- matrix(depth, nrow=length(depth), ncol=length(time))

depth_vec <- as.vector(depth_matrix)
temp_vec  <- as.vector(temp)

temp_vec <- as.vector(temp)

cor(depth, temp_vec, use = "complete.obs")
plot(depth, temp_vec,
     pch = 16,
     col = rgb(0,0,1,0.2),
     xlab = "Depth (m)",
     ylab = "Temperature (°C)",
     main = "Temperature vs Depth")
