#### Data updated with final 2024 column ####
#### Data re-merge following checks #########
#### 2.7.23 #################################
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
#2024 rename
load("data/2024 data/complete_canopy_2024.RData")
tags_24 <- tags %>% 
  pivot_longer(cols = starts_with("practices"),
               names_to = "var_24",
               values_to = "usage") %>% 
  left_join(merge24, by = "var_24") %>% 
  select(school_id, var = all, usage) %>% 
  mutate(year = 2024) # in long format
rm(dictionary, full, tag_dat, tags, variables)
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
write.csv(tags_long, "data/longitudinal/tags-long.csv", row.names = FALSE)

#create wide version
tags_wide <- tags_long %>% 
  pivot_wider(names_from = "var",
              values_from = "usage")

# check to make sure NA values are operating correctly 
# (i.e., that NA values are corresponding to years a tag was not present)
check <- tags_wide %>%
  pivot_longer(cols = 3:132, names_to = "tag", values_to = "value") %>%
  mutate(tag_sum = ifelse(is.na(value), 0, 1)) %>%
  group_by(year, tag) %>%
  summarise(school_count = sum(tag_sum), missing_count = sum(is.na(value))) %>%
  ungroup()
write.csv(tags_wide, "data/longitudinal/tags-wide.csv", row.names = FALSE)
