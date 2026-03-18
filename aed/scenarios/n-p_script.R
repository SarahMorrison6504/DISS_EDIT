## N:P ratio----

library(glmtools)
library(dplyr)
library(ggplot2)
library(ncdf4)
library(lme4)
library(MuMIn)

rm(list = ls())

np_ratio <- function(file, label){
  no3 <- get_var(file,
                 var_name = 'NIT_nit',
                 reference = 'surface')
  nh4 <- get_var(file,
                 var_name = 'NIT_amm',
                 reference = 'surface')
  
  phs <- get_var(file,
                 var_name = 'PHS_frp',
                 reference= 'surface')
  np <- data.frame(
    DateTime = no3$DateTime,
    NO3 = no3$NIT_nit_0,
    NH4 = nh4$NIT_amm_0,
    PHS = phs$PHS_frp_0
  )
  
  np <- np %>%
    mutate(
      TotalN = NO3 + NH4,
      NP_ratio = TotalN/PHS,
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

#plot
(ggplot(np_all, aes(DateTime, NP_ratio, colour = FPV))+
    geom_line(linewidth = 1) +
    labs(
      y = 'NO3:SRP ratio (mmol/m³)',
      x = 'Date', 
      colour = 'FPV Coverage')+
    theme_classic()+
    theme(
      axis.title = element_text(size = 12, face ='bold')
    ))


np_all$FPV <- factor(np_all$FPV,
                     levels = c('0%', '10%', '40%', '90%'))

# plot histogram to look at distribtuion
hist(np_all$NP_ratio)
# left skewed

# log transform
np_log <- np_all %>%
  mutate(logNP = log(NP_ratio))

model_log <- lmer(logNP ~ FPV + (1|DateTime), data = np_log)
summary(model_log)
par(mfrow=c(1,2))

# back transform
# baseline
exp(7.53)
# 1860
#10%
exp(-0.028)
# =0.97, t < 2 so not significant

#40%
exp(-0.329)
# =0.710643, ~28% reduction

#90% 
exp(-0.965)
# = 0.38, ~ 68% reduction compared to baseline

plot(model_log)
library(performance)
bartlett.test(NP_ratio~FPV, data = np_all)

qqnorm(resid(model_raw))
qqline(resid(model_raw))


ggplot(np_all, aes(x = FPV, y = temp)) +
  geom_point(size = 2, alpha = 0.7, colour = 'steelblue') +
  geom_smooth(method = 'lm', se = TRUE, colour = 'red', linetype = 'dashed') +
  labs(
    x = 'FPV Coverage',
    y = 'Temperature (°C)',
    title = 'Effect of FPV Coverage on Surface Temperature'
  ) +
  theme_classic()
