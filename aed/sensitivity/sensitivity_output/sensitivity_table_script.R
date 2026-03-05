## making sensitivtiy analyses results table
install.packages('gt')
library(tidyverse)
library(pixiedust)
library(kableExtra)
library(gt)
library(dplyr)
library(readxl)

sensitive <- sensitivity_table
head(sensitive)

sensitivity_table <- sensitive %>%
  group_by(Depth) %>%
  mutate(across(Temp:Greens, ~round(.x, 3)), 
         Depth = ifelse(row_number()== 1, Depth, "")) %>%
  ungroup()

sensitivity <-  sensitivity_table %>%
  gt() %>%
  tab_style(
    style = cell_text(weight = 'bold'),
    locations = cells_column_labels(everything())
    )
sensitivity
library(webshot2)
gtsave(sensitivity, 'Sensitivity_table.png',
       vwidth = 3000,
       vheight = 2000)
