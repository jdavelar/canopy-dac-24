#### CLEANING CANOPY SPRING UPDATE 2021 DATASET ####
library(here)
library(tidyverse)
library(rio)
dat <- import(here("data", "canopy_spring_update_2021.csv"))
# drop cols 1 & 2, keep rows 3-106
clean <- dat %>% 
  select(-c(V1, V2)) %>% 
  slice(3:106)

#map dirty vars and original Qs to add labels
label <- dat %>% 
  slice(2) %>% 
  pivot_longer(cols = everything())
export(label, "data/temp_label.csv")
