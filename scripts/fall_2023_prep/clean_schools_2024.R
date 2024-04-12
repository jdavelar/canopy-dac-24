pacman::p_load(tidyverse, here, rio)
dat <- import(here("data/2024 data", "schools-2024-raw.csv"))
long <- import(here("data/longitudinal", "longitudinal_data.csv"))

# cols to remove at end:
# -catalyst_key_other_text (no text responses)
# -pct_ell_na
# -pct_swd_na
# -pct_frpl_na
# -starts_with("tool_share_other_text") - should not have had text box here...?
# -support_key_other_text - should not have had text box here...?

##########################
#### LEADER VARIABLES ####
##########################

# exported following leader check CSV
# leader_chk <- clean %>%
#   select(school_id, contact_role, leader)
# write.csv(leader_chk, "data/leadership_role_check.csv", row.names = FALSE)

# CW made manual changes - imported below
lead <- import(here("data/2024 data", "leadership_role_check_updated.csv")) %>% 
  select(school_id, leader_updated)

#########################
#### CLEAN 2024 DATA ####
#########################

#rename dup col to drop
names(dat) <- make.unique(names(dat))
clean <- dat %>% 
  #drop dup col
  select(-ends_with(".1")) %>% 
  #NA for all missing values
  mutate(across(where(is.character), ~na_if(.x, ""))) %>% 
  #merge updated leader info
  left_join(lead, by = "school_id") %>% 
  #remove extraneous descriptors
  mutate(school_desc_other_text = ifelse(grepl("(?i)\\b(n/a|none)\\b", school_desc_other_text), 
                                         NA, 
                                         school_desc_other_text)) %>% 
  #fix dem pct
  mutate(across(contains("pct"), ~round(./100, 2))) %>% 
  #locale vars
  mutate(locale_multiple = ifelse(rowSums(select(., locale_rural, locale_suburban, locale_urban)) > 1, 1, 0),
         school_locale = case_when(
           locale_multiple == 1 ~ "Multiple",
           locale_multiple == 0 & locale_urban == 1 ~ "Urban",
           locale_multiple == 0 & locale_suburban == 1 ~ "Suburban",
           locale_multiple == 0 & locale_rural == 1 ~ "Rural"
         ),
  #fix contact race/gender + create leader_race/gender IF role = leader
  respondent_race = str_remove_all(contact_race, ","),
  respondent_race = case_when(
    respondent_race == "Prefer not to say" ~ NA,
    respondent_race == "Hispanic or Latinx" ~ "Latinx",
    respondent_race == "American Indian or Alaska Native" ~ "American Indian & Alaska Native",
    respondent_race == "Black or African American" ~ "Black",
    respondent_race == "Asian" ~ "Asian",
    TRUE ~ "Multiple races/ethnicities"
  ),
  respondent_gender = str_remove_all(contact_gender, ","),
  #imported leader_updated to define leader - ignore the following line
  #leader = ifelse(grepl("(?i)\\b(principal|director|head|chief|CEO|leader)\\b", contact_role), 1, 0),
  leader_race = case_when(
    leader_updated == 1 ~ respondent_race,
    TRUE ~ NA
  ),
  leader_gender = case_when(
    leader_updated == 1 ~ respondent_gender,
    TRUE ~ NA
  ),
  #school level vars
  grades_elementary = case_when(
    grades_1 == 1 |
      grades_2 == 1 |
      grades_3 == 1 |
      grades_4 == 1 |
      grades_5 == 1 ~ 1,
    TRUE ~ 0
  ),
  grades_middle = case_when(
    grades_7 == 1 | 
      (grades_8 == 1 & grades_9 == 0) |
      (grades_6 == 1 & grades_5 == 0) ~ 1,
    TRUE ~ 0
  ),
  grades_high = case_when(
    grades_10 == 1 |
      grades_11 == 1 |
      grades_12 == 1 ~ 1,
    TRUE ~ 0
  )) %>% 
  #remove extra cols
  select(-c(catalyst_key_other_text, support_key_other_text), -starts_with("tool_share_other_text"), -ends_with("na")) %>% 
  #rename incorrectly specified practices_ follow-ups
  rename(
    focus_economic_disadvantage = `practices_design_marginalized_eco-dis`,
    focus_emergent_bilingual = practices_design_marginalized_ell,
    focus_foster = practices_design_marginalized_foster,
    focus_homeless = practices_design_marginalized_homeless,
    focus_interrupted = practices_design_marginalized_interrupted,
    focus_juvenile_justice = practices_design_marginalized_juvenile,
    focus_multilingual = practices_design_marginalized_multilingual,
    focus_newcomer = practices_design_marginalized_newcomer,
    focus_bipoc = practices_design_marginalized_soc,
    focus_swd = practices_design_marginalized_swd,
    focus_other = practices_design_marginalized_other,
    focus_other_text = practices_design_marginalized_other_text,
    inclusive_emergent_bilingual = practices_inclusion_ell,
    inclusive_juvenile_justice = practices_inclusion_juvenile,
    inclusive_newcomer = practices_inclusion_newcomer,
    inclusive_other = practices_inclusion_other,
    inclusive_other_text = practices_inclusion_other_text,
    inclusive_swd = practices_inclusion_swd
  ) %>% 
  #remove na/none/independent options in district
  mutate(school_district = ifelse(grepl("(?i)\\b(none|n/a|independent|private)\\b", school_district), NA, school_district))

##############################
#### MERGE NOMINATOR INFO ####
##############################
noms_24 <- import(here("data/2024 data", "eligible_nominations_2024.csv")) %>% 
  janitor::clean_names() %>% 
  select(school_id = canopy_school_id, nom_24 = publicly_disclosable_nominator_org) %>% 
  group_by(school_id) %>%
  summarise(nom_24 = if(n_distinct(nom_24) == 1) first(nom_24) else toString(nom_24))
#working backwards to fill holes
noms_23 <- long %>%
  filter(year == 2023) %>% 
  select(school_id, nom_23 = nominator)
noms_22 <- long %>% 
  filter(year == 2022) %>% 
  select(school_id, nom_22 = nominator)
noms_21 <- long %>% 
  filter(year == 2021) %>% 
  select(school_id, nom_21 = nominator)
clean <- clean %>% 
  left_join(noms_24, by = "school_id") %>% 
  left_join(noms_23, by = "school_id") %>% 
  left_join(noms_22, by = "school_id") %>% 
  left_join(noms_21, by = "school_id") %>% 
  mutate(nominator = case_when(
    !is.na(nom_24) ~ nom_24,
    is.na(nom_24) & !is.na(nom_23) ~ nom_23,
    is.na(nom_24) & is.na(nom_23) & !is.na(nom_22) ~ nom_22,
    is.na(nom_24) & is.na(nom_23) & is.na(nom_22) & !is.na(nom_21) ~ nom_21
  )) %>% 
  select(-c(nom_24, nom_23, nom_22, nom_21))
rm(noms_24, noms_23, noms_22, noms_21, lead, long, dat)
#still some missing - track down
miss_nom <- clean %>% 
  filter(is.na(nominator))
#manual fill
clean <- clean %>% 
  mutate(nominator = case_when(
    school_id == 66 ~ "Anonymous",
    school_id == 184 ~ "Arkansas Team Digital",
    school_id == 205 ~ "Big Picture Learning, EdSurge, Anonymous",
    school_id == 233 ~ "Clayton Christensen Institute",
    school_id == 361 ~ "Big Picture Learning, Anonymous",
    school_id == 686 ~ "NewSchools Venture Fund",
    school_id == 704 ~ "NewSchools Venture Fund",
    school_id == 775 ~ "Transcend",
    TRUE ~ as.character(nominator)
  ))
sum(is.na(clean$nominator)) #fixed
rm(miss_nom)

######################################
#### CREATE PUBLIC FACING DATASET ####
######################################

#pull vars for public facing dataset
dictionary <- import(here("data/2024 data", "dictionary_2024.csv")) %>% 
  janitor::clean_names()
vars <- dictionary %>% 
  select(variable_name, confidential) %>% 
  filter(confidential == "Public-facing") %>% 
  pull(variable_name)
#create public facing dataset
public <- clean %>% 
  select(all_of(vars))
write.csv(public, "data/2024 data/public_data_2024.csv", row.names = FALSE)
rm(vars)

#################################
#### CREATE INTERNAL DATASET ####
#################################

#pull vars for tag data and variable data
tag_vars <- dictionary %>% 
  select(variable_name, data_subset) %>% 
  filter(data_subset == "Tags") %>% 
  pull(variable_name)
school_vars <- dictionary %>% 
  select(variable_name, data_subset) %>% 
  filter(data_subset == "Variables") %>% 
  pull(variable_name)
full_vars <- dictionary %>% 
  pull(variable_name)
#create internal datasets
tags <- clean %>% 
  select(school_id, all_of(tag_vars))
variables <- clean %>% 
  select(all_of(school_vars))
full <- clean %>% 
  select(all_of(full_vars))
rm(tag_vars, school_vars, full_vars)


######################################
#### DATA CHECKS - RESPONSE TYPES ####
######################################

# for loop to generate frequency table as CSV
# frequency_tables_list <- list()
# for(variable in names(full)) {
#   freq_table <- table(full[[variable]])
#   freq_df <- as.data.frame(freq_table)
#   names(freq_df) <- c("Value", "Frequency")
#   freq_df$Variable <- variable
#   frequency_tables_list[[length(frequency_tables_list) + 1]] <- freq_df
# }
# 
# combined_freq_df <- do.call(rbind, frequency_tables_list)
# #save as CSV
# write.csv(combined_freq_df, "data/2024 data/frequency_tables.csv", row.names = FALSE)

####################
#### FINAL SAVE ####
####################
#save R dataset
save(dictionary, tags, variables, full,
     file = "data/2024 data/complete_canopy_2024.RData")
write.csv(full, "data/2024 data/schools_2024.csv", row.names = FALSE)
