# n:p ratio----
# plotting raw nitrate and srp values to determine N:P ratios for water
# quality and identifying nitrogen limiting or phosphorus limiting conditions

# load libraries
library(glmtools)
library(dplyr)
library(ncdf4)
library(lme4)
library(MuMIn)
library(ggplot2)
library(DHARMa)

# create a function to gather N:P ratios at surface waters for all scenarios

np_ratio <- function(file, label){
  no3 <- get_var(file,
                 var_name = 'NIT_nit',
                 reference = 'surface')
  
  phs <- get_var(file,
                 var_name = 'PHS_frp',
                 reference= 'surface')
  np <- data.frame(
    DateTime = no3$DateTime,
    NO3 = no3$NIT_nit_0,
    PHS = phs$PHS_frp_0
  )
  
  np <- np %>%
    mutate(
      NP_ratio = NO3/PHS,
      FPV = label
    )
  
  return(np)
}

np_0 <- np_ratio('baseline_output/output_baseline.nc', '0%')
np_10 <- np_ratio('scenario_10_output/output_10.nc', '10%')
np_20 <- np_ratio('scenario_20_output/output_20.nc', '20%')
np_40 <- np_ratio('scenario_40_output/output_40.nc', '40%')
np_90 <- np_ratio('scenario_90_output/output_90.nc', '90%')

np_all <- bind_rows(np_0, np_10, np_40, np_90)

# plot
dev.off()
(n_p_plot <- ggplot(np_all, aes(DateTime, NP_ratio, colour = FPV))+
    geom_line(linewidth = 1) +
    labs(
      y = 'N:P ratio (mmol/m³)',
      x = 'Date', 
      colour = 'FPV Coverage')+
    theme_classic()+
    theme(
      axis.title = element_text(size = 12, face ='bold')
    ))
# Save high-resolution PNG

ggsave(
  filename = "np_plot.png",
  plot = n_p_plot,
  width = 8,
  height = 6,
  dpi = 600
)

# linear mixed modelling to compare changes between scenarios
# establish levels
np_all$FPV <- factor(np_all$FPV,
                     levels = c('0%', '10%', '40%', '90%'))


# check distribution
hist(np_all$NP_ratio)
# left skewed

# try gamma distribution model with date as randdom effect 
model_gamme <- glmer(NP_ratio~ FPV + (1|DateTime), data = np_all, family = Gamma(link = 'log'))
summary(model_gamme)

# check the diagnostic plots
plot(model_gamme)
gamme_residuals <- simulateResiduals((model_gamme))
plot(gamme_residuals)

## does not fit the best
# try log transforming and using linear mixed model
# log transform
np_log <- np_all %>%
  mutate(logNP = log(NP_ratio))

# model
model_log_nodate <- lm(logNP ~ FPV, data = np_log)
model_log <- lmer(logNP ~ FPV + (1|DateTime), data = np_log)
model_log_season <- lmer(logNP ~ FPV*season + (1|DateTime), data = np_log)
summary(model_log_season)
# check residuals
plot(model_log_season)
log_residuals <- simulateResiduals((model_log_season))
plot(log_residuals)

# save for appendics

# check model performace
AIC(model_log_nodate)
AIC(model_log)
AIC(model_log_season)

# AIC for model_log_season = lowest!

# exponent values to get percentage changes between groups in general
#10%
exp(0.01975)
# = 1,02 = 2% increase

# 40%
exp(-0.30422)
# = 0.7376986 = 26% decrease

# 90%
exp(-1.39473)
# 0.2479 = 75% decrease

# summer
exp(1.06798)
