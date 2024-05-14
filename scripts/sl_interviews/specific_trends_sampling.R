###################################
#### TRENDS INTERVIEW SAMPLING ####
###################################
library(here)
library(rio)
library(tidyverse)
dat <- import(here("data/2024 data", "schools_2024.csv"))
long_dat <- import(here("data/longitudinal", "tags-long.csv"))
schl_labs <- import(here("data", "school_names.csv"))

#### Prep descriptors dataset for joining ####
participation <- long_dat %>% 
  select(school_id, year) %>% 
  unique() %>% 
  arrange(school_id, year) %>% 
  group_by(school_id) %>% 
  mutate(n_years = row_number()) %>% 
  filter(n_years == max(n_years)) %>% 
  rename(last_participated = year)
descriptors <- import(here("data/longitudinal", "longitudinal_data.csv")) %>% 
  select(school_id, year, school_city, school_state, school_type, school_locale, school_enrollment, grades_elementary, grades_middle, grades_high) %>% 
  arrange(school_id, year) %>%
  group_by(school_id) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank == max(rank)) %>% 
  ungroup() %>% 
  mutate(school_level = case_when(
    grades_elementary == 1 & grades_middle == 0 & grades_high == 0 ~ "Elementary",
    grades_elementary == 1 & grades_middle == 1 & grades_high == 0 ~ "K-8",
    grades_elementary == 1 & grades_middle == 1 & grades_high == 1 ~ "K-12",
    grades_elementary == 0 & grades_middle == 1 & grades_high == 0 ~ "Middle",
    grades_elementary == 0 & grades_middle == 1 & grades_high == 1 ~ "Jr/Sr High",
    grades_elementary == 0 & grades_middle == 0 & grades_high == 1 ~ "High"
  )) %>% 
  select(-c(year, rank, grades_elementary, grades_middle, grades_high)) %>% 
  left_join(participation, by = "school_id")
  

#### Schools doing interesting stuff with AI ####
# pull list of schools that answered AI questions with text
# export to hand-select interesting use-cases (stored in GDrive)
ai <- dat %>% 
  select(school_id, school_name, starts_with("ai")) %>% 
  filter(!is.na(ai_innovation) | !is.na(ai_policy_text)) %>% 
  left_join(descriptors, by = "school_id")
write.csv(ai, "data/interview_sampling/ai_schools.csv", row.names = FALSE)
rm(ai)

#### Schools with major blended learning changes ####
# create count for dropped/added blended learning tags across years
# begin by prioritizing largest adds/drops
blended_tags <- c("practices_blended_learning", 
                  "practices_a_la_carte", 
                  "practices_flipped_classroom", 
                  "practices_flex", 
                  "practices_enriched_virtual", 
                  "practices_station_rotation")
#generate counts
blended = long_dat |>
  filter(var %in% blended_tags) %>% 
  arrange(school_id, var, year) |>
  summarize(
    added = any(diff(usage) == 1),
    dropped = any(diff(usage) == -1),
    waffled = added & dropped,
    modified = added | dropped,
    .by = c(var, school_id)
  ) |>
  filter(added | dropped) |>
  summarize(
    across(c(added, dropped, waffled, modified), sum),
    .by = school_id
  ) |>
  left_join(schl_labs, by = "school_id") %>% 
  select(school_id, school_name, added, dropped, waffled, modified)
#wafflers 
blended_waffles <- blended %>% 
  arrange(-waffled) %>% 
  slice_head(n = 15) %>% 
  mutate(type = "waffles")
waff_id <- blended_waffles %>% 
  pull(school_id)
#largest adds - top 15
blended_adds <- blended %>% 
  filter(!school_id %in% waff_id) %>% 
  arrange(-added) %>% 
  slice_head(n = 15) %>% 
  mutate(type = "adds")
#largest drops - top 15
blended_drops <- blended %>% 
  filter(!school_id %in% waff_id) %>% 
  arrange(-dropped) %>% 
  slice_head(n = 15) %>% 
  mutate(type = "drops")
#join
blended_merge <- bind_rows(blended_adds, blended_drops, blended_waffles) %>% 
  left_join(descriptors, by = "school_id")
#export
write.csv(blended_merge, "data/interview_sampling/blended_schools.csv", row.names = FALSE)
rm(blended, blended_adds, blended_drops, blended_waffles, blended_tags, waff_id)

#### Ed Justice schools ####
# create count for dropped/added blended learning tags across years
# begin by prioritizing largest adds/drops
cluster <- import(here("data/longitudinal", "tag_clusters_longitudinal.csv")) %>% 
  janitor::clean_names()
labels <- import(here("data", "tag_labels.csv"))
justice_tags <- cluster %>% 
  filter(proposed_cluster_for_preliminary_24_analysis == "Ed justice") %>% 
  select(label = tag) %>% 
  left_join(labels, by = "label") %>% 
  pull(var)
#generate counts
justice = long_dat |>
  filter(var %in% justice_tags) %>% 
  arrange(school_id, var, year) |>
  summarize(
    added = any(diff(usage) == 1),
    dropped = any(diff(usage) == -1),
    waffled = added & dropped,
    modified = added | dropped,
    .by = c(var, school_id)
  ) |>
  filter(added | dropped) |>
  summarize(
    across(c(added, dropped, waffled, modified), sum),
    .by = school_id
  ) |>
  left_join(schl_labs, by = "school_id") %>% 
  select(school_id, school_name, added, dropped, waffled, modified)
#wafflers 
justice_waffles <- justice %>% 
  arrange(-waffled) %>% 
  slice_head(n = 15) %>% 
  mutate(type = "waffles")
waff_id <- justice_waffles %>% 
  pull(school_id)
#largest adds - top 15
justice_adds <- justice %>% 
  filter(!school_id %in% waff_id) %>% 
  arrange(-added) %>% 
  slice_head(n = 15) %>% 
  mutate(type = "adds")
#largest drops - top 15
justice_drops <- justice %>% 
  filter(!school_id %in% waff_id) %>% 
  arrange(-dropped) %>% 
  slice_head(n = 15) %>% 
  mutate(type = "drops")
#join
justice_merge <- bind_rows(justice_adds, justice_drops, justice_waffles) %>% 
  left_join(descriptors, by = "school_id")
#export
write.csv(justice_merge, "data/interview_sampling/justice_schools.csv", row.names = FALSE)
rm(justice, justice_adds, justice_drops, justice_waffles, justice_tags, waff_id)

# Who is in both blended learning and ed justice lists?
blended <- blended_merge %>% 
  select(school_id, type) %>% 
  mutate(cluster = "blended")
justice <- justice_merge %>% 
  select(school_id, type) %>% 
  mutate(cluster = "justice")
merged <- bind_rows(blended, justice) %>% 
  mutate(rank = 1) %>% 
  group_by(school_id) %>% 
  summarize(n = sum(rank)) %>% 
  filter(n > 1)
