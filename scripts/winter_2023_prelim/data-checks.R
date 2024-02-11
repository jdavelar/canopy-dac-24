## Tag data checks ##
library(here)
library(rio)
library(tidyverse)
long <- import(here("data/longitudinal", "tags-long.csv"))
wide <- import(here("data/longitudinal", "tags-wide.csv"))
long_n <- long %>% 
  summarize(n = sum(usage), .by = c("var", "year"))
n_2023 <- long_n %>% filter(year == 2023)
n_2022 <- long_n %>% filter(year == 2022)
n_2021 <- long_n %>% filter(year == 2021)
n_2019 <- long_n %>% filter(year == 2019)
#2023
load("data/complete_canopy_2023.RData")
full %>% 
  select(starts_with("practices")) %>% 
  pivot_longer(cols = everything(),
               names_to = "var",
               values_to = "usage") %>% 
  summarize(n = sum(usage), .by = "var") %>% 
  left_join(n_2023, by = "var") %>% 
  mutate(match = ifelse(n.x == n.y, 1, 0)) %>% 
  filter(match == 0)
rm(confidential, dictionary, full, impact, schools, tags)

#2022
load("data/complete_canopy_2022.RData")
practices_data %>% 
  select(starts_with("practices")) %>% 
  pivot_longer(cols = everything(),
               names_to = "var",
               values_to = "usage") %>% 
  summarize(n = sum(usage), .by = "var") %>% 
  left_join(n_2022, by = "var") %>% 
  mutate(match = ifelse(n.x == n.y, 1, 0)) %>% 
  filter(match == 0)
# service learning is not matching - getting a 0 in the merged dataset, where it should be 91
# double check that the tag name was set right when merging
rm(clean_data_dictionary, clean_data_full, demographic_data, learning_model_data, practices_data, school_data, survey_data)

#2021
a21 <- import(here("data/schools_2021a.csv")) %>% 
  select(school_id, c(16:106)) %>% 
  mutate(across(c(2:92), ~ifelse(.=="", 0, 1)))
b21 <- import(here("data/schools_2021b.csv")) %>% 
  select(school_id, c(17:107)) %>% 
  mutate(across(c(2:92), ~ifelse(.=="", 0, 1)))
c21 <- import(here("data/schools_2021c.csv")) %>%  # this was a follow-up - checking against a21
  select(school_id, c(4:19)) %>% 
  mutate(across(c(2:17), ~ifelse(.=="", 0, 1)))
c_schools <- c21 %>% 
  pull(school_id)
a_check <- a21 %>% 
  filter(school_id %in% c_schools) %>% 
  pivot_longer(cols = !school_id,
               names_to = "tag",
               values_to = "usage") %>% 
  filter(!is.na(usage))
c_check <- c21 %>% 
  pivot_longer(cols = !school_id,
               names_to = "tag",
               values_to = "usage2") %>% 
  filter(!is.na(usage2)) %>% 
  right_join(a_check, by = c("school_id", "tag")) %>% 
  mutate(check = ifelse(usage == usage2, 1, 0)) %>% 
  filter(check == 0)
# there's an issue with 2021 data
# in the winter follow-up it looks like we asked schools that responded in the first wave to report on 16 COVID-specific practices
# in the follow-up, 61 schools reported changes across 9 different tags:
# 1. family learning
# 2. hybrid
# 3. families choose modality
# 4. asynchronous online
# 5. rotating schedule
# 6. fully remote
# 7. remote accommodations
# 8. synchronous online
# 9, fully in person
# in all of these cases, the school is reporting using a practice in the winter that they were not using in the fall
# should we overwrite practices so that in these instances we consider the school to have reported the practice in 2021?

#continuing to check Ns
# need labels
labels <- import(here("data/longitudinal", "tag-labels.csv")) %>% 
  rename("var" = variable)
update_label <- labels %>% 
  filter(str_starts(var, "practices_"))
bind_rows(a21, b21) %>% 
  pivot_longer(cols = !school_id,
               names_to = "var",
               values_to = "usage") %>% 
  summarize(n = sum(usage), .by = "var") %>% 
  left_join(labels, by = "var") %>% 
  select(label, n) %>% 
  left_join(update_label, by = "label") %>% 
  select(var, n) %>% 
  left_join(n_2021, by = "var") %>% 
  mutate(match = ifelse(n.x == n.y, 1, 0)) %>% 
  filter(match == 0)
# numbers match, which means the saved dataset does not incorporate wave 3 changes from winter
rm(a21, b21, a_check, c_check, c21, c_schools)


#2019
# modify labels to accurately pick up portfolios + exhibitions
labels <- labels %>% 
  mutate(var = case_when(
    label == "portfolios and public exhibitions of student work" ~ "portfolios AND exhibitions",
    TRUE ~ as.character(var)
  )) %>% 
  unique()
dat_19 <- import(here("data/schools_2019.csv"))
dat_19 %>% 
  select(school_id, c(15:102)) %>% 
  mutate(across(c(2:89), ~ifelse(is.na(.), 0, 1))) %>% 
  mutate(`portfolios AND exhibitions` = case_when(
    portfolios == 1 | exhibitions == 1 ~ 1,
    is.na(portfolios) & exhibitions == 1 ~ 1,
    is.na(exhibitions) & portfolios == 1 ~ 1,
    TRUE ~ 0
  )) %>% 
  select(-portfolios, -exhibitions) %>% 
  pivot_longer(cols = !school_id,
               names_to = "var",
               values_to = "usage") %>% 
  summarize(n = sum(usage), .by = "var") %>% 
  left_join(labels, by = "var") %>% 
  select(label, n) %>% 
  left_join(update_label, by = "label") %>% 
  select(var, n) %>% 
  left_join(n_2019, by = "var") %>% 
  mutate(match = ifelse(n.x == n.y, 1, 0)) %>% 
  filter(match == 0)
# some weirdness with merging portfolios and exhibitions
# likely because 2 tags became 1 tag "portfolios and exhibitions"
# rerunning I get 202 in the original dataset, and only 119 in the long dataset
# check merging

####### TO DOS #########
# 1. check service learning in tag merge for 2022