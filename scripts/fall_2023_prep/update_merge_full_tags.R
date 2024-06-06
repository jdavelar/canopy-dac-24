#### Adding core and time variables to longitudinal tags ####
#### 6.6.24 #################################
pacman::p_load(tidyverse, here, rio, janitor)
tag_dat <- import(here("data", "tags_2018-2023.csv")) %>% 
  clean_names() %>% 
  select(all = variable_name,
         var_24 = equivalent_variable_name_2024, 
         var_23 = equivalent_variable_name_2023, 
         var_22 = equivalent_variable_name_2022, 
         var_21 = equivalent_variable_name_2021, 
         var_19 = equivalent_variable_name_2019) %>% 
  filter(all != "")
# create merge keys for each year
# check that number of tags matches correctly
merge24 <- tag_dat %>% 
  select(all, var_24) %>% 
  filter(!is.na(var_24)) #73 tags
merge23 <- tag_dat %>% 
  select(all, var_23) %>% 
  filter(!is.na(var_23)) #73 tags (other co-leaders not in AT)
merge22 <- tag_dat %>% 
  select(all, var_22) %>% 
  filter(!is.na(var_22)) #72 tags
merge21 <- tag_dat %>% 
  select(all, var_21) %>% 
  filter(!is.na(var_21)) #91 tags
merge19 <- tag_dat %>% 
  select(all, var_19) %>% 
  filter(!is.na(var_19)) #87 tags (portfolios & exhibitions are merged into 1)

###################
#### CORE TAGS ####
###################
# 2024 rename
load("data/2024 data/complete_canopy_2024.RData")
core_24 <- tags %>% 
  select(school_id, starts_with("core")) %>% 
  rename_with(~ gsub("^core_", "practices_", .), starts_with("core_")) %>% 
  pivot_longer(cols = starts_with("practices"),
               names_to = "var_24",
               values_to = "usage") %>% 
  left_join(merge24, by = "var_24") %>% 
  select(school_id, var = all, usage) %>% 
  mutate(year = 2024) # in long format
rm(dictionary, full, tag_dat, tags, variables)
#2023 rename
load("data/complete_canopy_2023.RData")
core_23 <- full %>% 
  select(school_id, starts_with("core")) %>% 
  rename_with(~ gsub("^core_", "practices_", .), starts_with("core_")) %>%
  pivot_longer(cols = starts_with("practices"),
               names_to = "var_23",
               values_to = "usage") %>% 
  left_join(merge23, by = "var_23") %>% 
  select(school_id, var = all, usage) %>% 
  mutate(year = 2023) # in long format
rm(confidential, dictionary, full, impact, schools, tags)
#2022 rename
load("data/complete_canopy_2022.RData")
core_22 <- clean_data_full %>% 
  select(school_id, starts_with("core")) %>% 
  rename_with(~ gsub("^core_", "practices_", .), starts_with("core_")) %>%
  pivot_longer(cols = starts_with("practices"),
               names_to = "var_22",
               values_to = "usage") %>% 
  left_join(merge22, by = "var_22") %>% 
  select(school_id, var = all, usage) %>% 
  mutate(year = 2022) # in long format
rm(clean_data_dictionary, clean_data_full, demographic_data, learning_model_data, practices_data, school_data, survey_data)
#2021 rename
#2021 rule - if a school reported a practice at any point in 2021, it's marked as a 1
core_21c <- import(here("data", "schools_2021c.csv")) %>% 
  select(-school_name, -promising_practices) %>% 
  mutate(across(2:17, ~ifelse(is.na(.), 0, .))) %>% 
  pivot_longer(cols = !school_id,
               names_to = "var_21",
               values_to = "usage_c")
#link follow-up reporting to first wave
core_21a <- import(here("data", "schools_2021a.csv")) %>% 
  select(school_id, c(16:106)) %>% 
  mutate(across(2:92, ~ case_when(
    . == 1 ~ 0,
    . == "" ~ 0,
    TRUE ~ 1
  ))) %>% 
  pivot_longer(cols = !school_id,
               names_to = "var_21",
               values_to = "usage_a") %>% 
  left_join(core_21c, by = c("school_id", "var_21")) %>% 
  mutate(usage = case_when(
    is.na(usage_a) & !is.na(usage_c) ~ usage_c,
    is.na(usage_c) & !is.na(usage_a) ~ usage_a,
    usage_a == 1 | usage_c == 1 ~ 1,
    usage_a == 0 & usage_c == 1 ~ 1,
    usage_a == 1 & usage_c == 0 ~ 1,
    TRUE ~ 0
  )) %>% 
  select(school_id, var_21, usage)
core_21b <- import(here("data", "schools_2021b.csv")) %>% 
  select(school_id, c(17:107)) %>% 
  mutate(across(2:92, ~ case_when(
    . == 1 ~ 0,
    . == "" ~ 0,
    TRUE ~ 1
  ))) %>% 
  pivot_longer(cols = !school_id,
               names_to = "var_21",
               values_to = "usage")
core_21 <- core_21b %>% 
  bind_rows(core_21a) %>% 
  left_join(merge21, by = "var_21") %>% 
  select(school_id, var = all, usage) %>% 
  mutate(year = 2021)
rm(core_21a, core_21b, core_21c)
#no 2019 core data
# merge all
core_long <- core_21 %>% 
  bind_rows(core_22) %>% 
  bind_rows(core_23) %>% 
  bind_rows(core_24) %>% 
  rename("core" = usage)
rm(core_21, core_22, core_23, core_24)

###################
#### TIME TAGS ####
###################
# 2024 rename
load("data/2024 data/complete_canopy_2024.RData")
time_24 <- tags %>% 
  select(school_id, starts_with("time")) %>% 
  rename_with(~ gsub("^time_", "practices_", .), starts_with("time_")) %>% 
  pivot_longer(cols = starts_with("practices"),
               names_to = "var_24",
               values_to = "usage") %>% 
  left_join(merge24, by = "var_24") %>% 
  select(school_id, var = all, usage) %>% 
  mutate(year = 2024,
         usage = replace(usage, is.na(usage), 0)) # in long format
rm(dictionary, full, tags, variables)
#2023 rename
load("data/complete_canopy_2023.RData")
time_23 <- full %>% 
  select(school_id, starts_with("time")) %>% 
  rename_with(~ gsub("^time_", "practices_", .), starts_with("time_")) %>%
  pivot_longer(cols = starts_with("practices"),
               names_to = "var_23",
               values_to = "usage") %>% 
  left_join(merge23, by = "var_23") %>% 
  select(school_id, var = all, usage) %>% 
  mutate(year = 2023,
         usage = case_when(
           usage == 5 ~ "Not sure",
           usage == 4 ~ "5+ years",
           usage == 3 ~ "3-4 years",
           usage == 2 ~ "1-2 years",
           usage == 1 ~ "Less than a year"),
         usage = replace(usage, is.na(usage), 0))# in long format
rm(confidential, dictionary, full, impact, schools, tags)
#2022 rename
load("data/complete_canopy_2022.RData")
time_22 <- clean_data_full %>% 
  select(school_id, starts_with("time")) %>% 
  rename_with(~ gsub("^time_", "practices_", .), starts_with("time_")) %>%
  pivot_longer(cols = starts_with("practices"),
               names_to = "var_22",
               values_to = "usage") %>% 
  left_join(merge22, by = "var_22") %>% 
  select(school_id, var = all, usage) %>% 
  mutate(year = 2022,
         usage = replace(usage, is.na(usage), 0)) # in long format
rm(clean_data_dictionary, clean_data_full, demographic_data, learning_model_data, practices_data, school_data, survey_data)
#2021 rename
#2021c did not include reporting for time
#link follow-up reporting to first wave
time_21a <- import(here("data", "schools_2021a.csv")) %>% 
  select(school_id, c(16:106)) %>% 
  mutate(across(2:92, ~ case_when(
    . == 1 ~ "0",
    . == "" ~ "0",
    is.na(.) ~ "0",
    TRUE ~ as.character(.)
  ))) %>% 
  pivot_longer(cols = !school_id,
               names_to = "var_21",
               values_to = "usage")
time_21b <- import(here("data", "schools_2021b.csv")) %>% 
  select(school_id, c(17:107)) %>% 
  mutate(across(2:92, ~ case_when(
    . == 1 ~ "0",
    . == "" ~ "0",
    is.na(.) ~ "0",
    TRUE ~ as.character(.)
  ))) %>%  
  pivot_longer(cols = !school_id,
               names_to = "var_21",
               values_to = "usage")
time_21 <- time_21b %>% 
  bind_rows(time_21a) %>% 
  left_join(merge21, by = "var_21") %>% 
  select(school_id, var = all, usage) %>% 
  mutate(year = 2021,)
rm(time_21a, time_21b)
#no 2019 core/time data
# merge all
time_long <- time_21 %>% 
  bind_rows(time_22) %>% 
  bind_rows(time_23) %>% 
  bind_rows(time_24) %>% 
  rename("time" = usage)
rm(time_21, time_22, time_23, time_24)

######################
#### REGULAR TAGS ####
######################
#### Data updated with final 2024 column ####
#### Data re-merge following checks #########
#### 2.7.23 #################################
#2024 rename
load("data/2024 data/complete_canopy_2024.RData")
tags_24 <- tags %>% 
  pivot_longer(cols = starts_with("practices"),
               names_to = "var_24",
               values_to = "usage") %>% 
  left_join(merge24, by = "var_24") %>% 
  select(school_id, var = all, usage) %>% 
  mutate(year = 2024) # in long format
rm(dictionary, full, tags, variables)
#2023 rename
load("data/complete_canopy_2023.RData")
tags_23 <- full %>% 
  select(school_id, starts_with("practices")) %>% 
  pivot_longer(cols = starts_with("practices"),
               names_to = "var_23",
               values_to = "usage") %>% 
  left_join(merge23, by = "var_23") %>% 
  select(school_id, var = all, usage) %>% 
  mutate(year = 2023) # in long format
rm(confidential, dictionary, full, impact, schools, tags)
#2022 rename
load("data/complete_canopy_2022.RData")
tags_22 <- clean_data_full %>% 
  select(school_id, starts_with("practices")) %>% 
  pivot_longer(cols = starts_with("practices"),
               names_to = "var_22",
               values_to = "usage") %>% 
  left_join(merge22, by = "var_22") %>% 
  select(school_id, var = all, usage) %>% 
  mutate(year = 2022) # in long format
rm(clean_data_dictionary, clean_data_full, demographic_data, learning_model_data, practices_data, school_data, survey_data)
#2021 rename
#2021 rule - if a school reported a practice at any point in 2021, it's marked as a 1
tags_21c <- import(here("data", "schools_2021c.csv")) %>% 
  select(-school_name, -promising_practices) %>% 
  mutate(across(2:17, ~ifelse(is.na(.), 0, .))) %>% 
  pivot_longer(cols = !school_id,
               names_to = "var_21",
               values_to = "usage_c")
#link follow-up reporting to first wave
tags_21a <- import(here("data", "schools_2021a.csv")) %>% 
  select(school_id, c(16:106)) %>% 
  mutate(across(2:92, ~ifelse(is.na(.) | . == "", 0, 1))) %>% 
  pivot_longer(cols = !school_id,
               names_to = "var_21",
               values_to = "usage_a") %>% 
  left_join(tags_21c, by = c("school_id", "var_21")) %>% 
  mutate(covid_change = case_when(
    !is.na(usage_c) & usage_a != usage_c ~ 1,
    !is.na(usage_c) & usage_a == usage_c ~ 0,
    TRUE ~ NA
  ))
# save COVID schools
covid <- tags_21a %>% 
  filter(!is.na(covid_change))
write.csv(covid, "data/covid_shifters_2021.csv", row.names = FALSE)
tags_21a <- tags_21a %>% 
  mutate(usage = case_when(
    is.na(usage_a) & !is.na(usage_c) ~ usage_c,
    is.na(usage_c) & !is.na(usage_a) ~ usage_a,
    usage_a == 1 | usage_c == 1 ~ 1,
    usage_a == 0 & usage_c == 1 ~ 1,
    usage_a == 1 & usage_c == 0 ~ 1,
    TRUE ~ 0
  )) %>% 
  select(school_id, var_21, usage)
tags_21b <- import(here("data", "schools_2021b.csv")) %>% 
  select(school_id, c(17:107)) %>% 
  mutate(across(2:92, ~ifelse(is.na(.) | . == "", 0, 1))) %>% 
  pivot_longer(cols = !school_id,
               names_to = "var_21",
               values_to = "usage")
tags_21 <- tags_21b %>% 
  bind_rows(tags_21a) %>% 
  left_join(merge21, by = "var_21") %>% 
  select(school_id, var = all, usage) %>% 
  mutate(year = 2021)
rm(covid, tags_21a, tags_21b, tags_21c)
#2019
tags_19 <- import(here("data", "schools_2019.csv")) %>% 
  select(school_id, c(15:102)) %>% 
  mutate(across(2:89, ~ifelse(is.na(.), 0, .))) %>% 
  mutate(`portfolios AND exhibitions` = case_when(
    portfolios == 1 | exhibitions == 1 ~ 1,
    is.na(portfolios) & exhibitions == 1 ~ 1,
    is.na(exhibitions) & portfolios == 1 ~ 1,
    TRUE ~ 0
  )) %>% 
  select(-portfolios, -exhibitions) %>% 
  pivot_longer(cols = !school_id,
               names_to = "var_19",
               values_to = "usage") %>% 
  left_join(merge19, by = "var_19") %>% 
  select(school_id, var = all, usage) %>% 
  mutate(year = 2019)
# merge all
tags_long <- tags_19 %>% 
  bind_rows(tags_21) %>% 
  bind_rows(tags_22) %>% 
  bind_rows(tags_23) %>% 
  bind_rows(tags_24)
rm(tags_19, tags_21, tags_22, tags_23, tags_24, merge19, merge21, merge22, merge23, merge24)

############################
#### MERGE ALL TAG DATA ####
############################
# create long version
tag_dat <- tags_long %>% 
  left_join(core_long, by = c("school_id", "var", "year")) %>% 
  left_join(time_long, by = c("school_id", "var", "year")) %>% 
  select(school_id, var, year, usage, core, time)

# create wide version
tags_wide <- tag_dat %>% 
  pivot_wider(names_from = "var",
              values_from = c(usage, core, time)) %>% 
  rename_with(~ gsub("^usage_practices_", "practices_", .), starts_with("usage_")) %>%
  rename_with(~ gsub("^core_practices_", "core_", .), starts_with("core_")) %>% 
  rename_with(~ gsub("^time_practices_", "time_", .), starts_with("time_"))

# save both versions
write.csv(tag_dat, "data/longitudinal/full-tags-long.csv", row.names = FALSE)
write.csv(tags_wide, "data/longitudinal/full-tags-wide.csv", row.names = FALSE)

