#### Data updated with final 2024 column ####
#### Data re-merge following checks #########
#### 2.25.24 #################################

# Overview steps:
# 1. create merge keys for each year
# 2. read in data, 1 year at a time
# 3. address notes column to get vars in correct format for merging
# 4. use merge key to select and rename cols

pacman::p_load(tidyverse, here, rio, janitor)
var <- import(here("data", "var_labels.csv")) %>% 
  select(-notes)
# create merge keys for each year
# check that number of vars matches correctly
merge24 <- var %>% 
  select(all, var_2024) %>% 
  filter(var_2024 != "") #99 vars
cols24 <- merge24 %>% pull(var_2024)
merge24 <- merge24 %>% pull(all)
merge23 <- var %>% 
  select(all, var_2023) %>% 
  filter(var_2023 != "") #106 vars
cols23 <- merge23 %>% pull(var_2023)
merge23 <- merge23 %>% pull(all)
merge22 <- var %>% 
  select(all, var_2022) %>% 
  filter(var_2022 != "") #98 vars
cols22 <- merge22 %>% pull(var_2022)
merge22 <- merge22 %>% pull(all)
merge21 <- var %>% 
  select(all, var_2021) %>% 
  filter(var_2021 != "")  #75 vars
cols21 <- merge21 %>% pull(var_2021)
merge21 <- merge21 %>% pull(all)
merge19 <- var %>% 
  select(all, var_2019) %>% 
  filter(var_2019 != "")  #54 vars
cols19 <- merge19 %>% pull(var_2019)
merge19 <- merge19 %>% pull(all)

####################################
########## 2019 DATASET ############
####################################
dat19 <- import(here("data", "schools_2019.csv")) %>% 
  #recode yes/no
  mutate(across(where(~any(. %in% c("Yes", "No"))), ~case_when(
    . == "No" ~ 0,
    . == "Yes" ~ 1,
    TRUE ~ NA
  ))) %>% 
  #standardize level vars
  mutate(grades_pk = ifelse(pk_grade == 1, 1, 0),
         grades_elementary = case_when(
           `1_grade` == 1 |
           `2_grade` == 1 |
           `3_grade` == 1 |
           `4_grade` == 1 |
           `5_grade` == 1 ~ 1,
           TRUE ~ 0
         ),
         grades_middle = case_when(
           `7_grade` == 1 | 
           (`8_grade` == 1 & `9_grade` == 0) |
           (`6_grade` == 1 & `5_grade` == 0) ~ 1,
           TRUE ~ 0
         ),
         grades_high = case_when(
           `10_grade` == 1 |
           `11_grade` == 1 |
           `12_grade` == 1 ~ 1,
           TRUE ~ 0
         )) %>% 
  #standardize percent cols
  mutate(across(contains("percent"), ~{
    numeric_values <- as.numeric(str_remove_all(., "%"))
    rounded_values <- round(numeric_values / 100, 2)
    return(rounded_values)
  })) %>% 
  #standardize locale vars
  mutate(locale = na_if(locale, ""),
         #manual recodes for missing data - extracted from portal
         locale = case_when(
           school_id == 3 ~ "Urban",
           school_id == 8 ~ "Urban",
           school_id == 11 ~ "Suburban",
           school_id == 58 ~ "Urban",
           school_id == 80 ~ "Urban",
           school_id == 81 ~ "Suburban",
           school_id == 101 ~ "Urban",
           school_id == 122 ~ "Suburban",
           school_id == 141 ~ "Rural",
           school_id == 150 ~ "Multiple",
           school_id == 165 ~ "Urban",
           school_id == 172 ~ "Urban",
           TRUE ~ locale
         ),
             #missing in portal: 1, 35, 49, 62, 67, 72, 100, 106, 111, 135, 136, 143, 164, 190, 194, 195, 205, 230
         locale_rural = case_when(
           locale == "Rural" ~ 1,
           is.na(locale) ~ NA,
           TRUE ~ 0
         ),
         locale_suburban = case_when(
           locale == "Suburban" ~ 1,
           is.na(locale) ~ NA,
           TRUE ~ 0
         ),
         locale_urban = case_when(
           locale == "Urban" ~ 1,
           is.na(locale) ~ NA,
           TRUE ~ 0
         ),
         #create BIPOC equiv
         pct_bipoc = 1 - white_percent,
         #address 1/NA format
         poverty_supports = ifelse(poverty_supports == 1, 1, 0),
         ell_supports = ifelse(ell_supports == 1, 1, 0),
         #standardize title I
         title_i_status = case_when(
           title_i_status == "Title I schoolwide school" ~ 1,
           title_i_status == "Not reported" ~ NA,
           title_i_status == "Missing" ~ NA,
           TRUE ~ 0
         ),
         #standardize type
         type = case_when(
           charter == 1 ~ "Public charter school",
           school_type == "Private" ~ "Independent (private) school",
           TRUE ~ "Public district school"
         ),
         #year var
         year = 2019) %>% 
  #select relevant cols
  select(all_of(cols19)) %>% 
  #rename cols
  rename_with(~merge19)
rm(merge19, cols19)

####################################
########## 2021 DATASET ############
####################################
# for 2021, only merge A & B - follow-up was only for tags
dat21a <- import(here("data", "schools_2021a.csv")) %>% 
  #remove tags
  select(-setdiff(16:106, which(names(.) == "hybrid"))) %>% 
  #change ELA & math school average to allow merge below
  mutate(across(contains("Average"), ~as.numeric(str_remove_all(., "%"))))
dat21b <- import(here("data", "schools_2021b.csv")) %>% 
  #remove tags
  select(-setdiff(17:107, which(names(.) == "hybrid"))) %>% 
  #change average grad rate to allow merge below
  mutate(average_grad_rate = as.numeric(str_remove_all(average_grad_rate, "%")))
#B has 1 more col than A... why?
setdiff(names(dat21b), names(dat21a))
#promising_practices not asked in first round - weird.
#doesn't affect long. merging, so ignore

dat21 <- bind_rows(dat21a, dat21b)
#set vector to ignore conditions_other_text - throwing error later on
conditions <- dat21 %>% 
  select(starts_with("conditions"), -conditions_other_text) %>% 
  colnames()
#continue cleaning data
dat21 <- dat21 %>% 
  #recode yes/no
  mutate(across(where(~any(. %in% c("Yes", "No"))), ~case_when(
    . == "No" ~ 0,
    . == "Yes" ~ 1,
    TRUE ~ NA
  ))) %>% 
  #standardize percent cols
  mutate(across(contains("percent"), ~{
    numeric_values <- as.numeric(str_remove_all(., "%"))
    rounded_values <- round(numeric_values / 100, 2)
    return(rounded_values)
  })) %>% 
  #standardize level vars
  mutate(grades_pk = ifelse(pk_grade == 1, 1, 0),
         grades_elementary = case_when(
           `1_grade` == 1 |
             `2_grade` == 1 |
             `3_grade` == 1 |
             `4_grade` == 1 |
             `5_grade` == 1 ~ 1,
           TRUE ~ 0
         ),
         grades_middle = case_when(
           `7_grade` == 1 | 
             (`8_grade` == 1 & `9_grade` == 0) |
             (`6_grade` == 1 & `5_grade` == 0) ~ 1,
           TRUE ~ 0
         ),
         grades_high = case_when(
           `10_grade` == 1 |
             `11_grade` == 1 |
             `12_grade` == 1 ~ 1,
           TRUE ~ 0
         ),
         #standardize locale
         locale = na_if(locale, ""),
             #manual recode missing data
         locale = case_when(
           school_id == 52 ~ "Suburban",
           school_id == 63 ~ "Urban",
           school_id == 150 ~ "Multiple",
           school_id == 189 ~ "Suburban",
           school_id == 247 ~ "Suburban",
           school_id == 253 ~ "Urban",
           school_id == 266 ~ "Urban",
           school_id == 276 ~ "Urban",
           school_id == 284 ~ "Urban",
           school_id == 293 ~ "Multiple",
           school_id == 303 ~ "Urban",
           school_id == 325 ~ "Multiple",
           school_id == 348 ~ "Urban",
           school_id == 360 ~ "Urban",
           school_id == 368 ~ "Multiple",
           school_id == 400 ~ "Urban",
           school_id == 394 ~ "Rural",
           school_id == 475 ~ "Multiple",
           school_id == 251 ~ "Urban",
           school_id == 443 ~ "Urban",
           school_id == 382 ~ "Urban",
           school_id == 472 ~ "Multiple",
           school_id == 462 ~ "Rural",
           school_id == 466 ~ "Multiple",
           TRUE ~ locale
         ),
             #not in portal: 39, 49, 67, 112, 185, 190, 199, 229, 230, 287, 329, 351, 88, 389, 402, 395, 194, 478, 1, 391, 396, 408, 228, 62
         locale_rural = ifelse(locale == "Rural", 1, 0),
         locale_urban = ifelse(locale == "Urban", 1, 0),
         locale_suburban = ifelse(locale == "Suburban", 1, 0),
         #standardize magnet
         magnet = case_when(
           magnet == "1-Yes" ~ 1,
           magnet == "" ~ NA,
           magnet == "–" ~ NA,
           TRUE ~ 0
         ),
         #create pct bipoc
         pct_bipoc = 1 -  white_percent,
         #standardize title I
         title_i_schoolwide = case_when(
           title_i_schoolwide == "1-Yes" ~ 1,
           title_i_schoolwide == "" ~ NA,
           title_i_schoolwide == "-" ~ NA,
           TRUE ~ 0
         ),
         #standardize type
         type = case_when(
           #some schools were missing data - fill from portal if possible
           school_id == 52 ~ "Public district school",
           school_id == 63 ~ "Public charter school",
           school_id == 67 ~ "Public charter school",
           school_id == 150 ~ "Independent (private) school",
           school_id == 189 ~ "Public district school",
           school_id == 247 ~ "Public district school",
           school_id == 253 ~ "Independent (private) school",
           school_id == 266 ~ "Independent (private) school",
           school_id == 276 ~ "Public district school",
           school_id == 284 ~ "Independent (private) school",
           school_id == 293 ~ "Public district school",
           school_id == 303 ~ "Public charter school",
           school_id == 325 ~ "Independent (private) school",
           school_id == 348 ~ "Public district school",
           school_id == 360 ~ "Public district school",
           school_id == 368 ~ "Independent (private) school",
           school_id == 400 ~ "Public charter school",
           school_id == 394 ~ "Public district school",
           school_id == 475 ~ "Public district school",
           school_id == 251 ~ "Independent (private) school",
           school_id == 443 ~ "Public charter school",
           school_id == 382 ~ "Public district school",
           school_id == 472 ~ "Public district school",
           school_id == 462 ~ "Public district school",
           school_id == 466 ~ "Independent (private) school",
           school_id == 228 ~ "Public district school",
           type == "Charter" ~ "Public charter school",
           type == "District" ~ "Public district school",
           type == "Independent" ~ "Independent (private) school"),
          #missing 39, 49, 112, 190, 199, 229, 287, 329, 351, 88, 389, 402, 395, 194, 478, 1, 391, 396, 408, 62
         #standardize averages
         ELA_School_Average = round(ELA_School_Average/100, 2),
         Math_School_Average = round(Math_School_Average/100, 2),
         #standardize hybrid
         hybrid = ifelse(hybrid == "", 0, 1),
         #create year col
         year = 2021,
         #standardize conditions question responses
         #assuming rank order runs 1 = most important
         rank_covid = ifelse(conditions_covid19_closures < 4 & conditions_covid19_closures > 0, 1, 0),
         rank_cutting_edge = ifelse(conditions_desire_cutting_edge < 4 & conditions_desire_cutting_edge > 0, 1, 0),
         rank_demographics = ifelse(conditions_change_in_demographics < 4 & conditions_change_in_demographics > 0, 1, 0),
         rank_external = ifelse(conditions_external_catalyst_events < 4 & conditions_external_catalyst_events > 0, 1, 0),
         rank_inequities = ifelse(conditions_systemic_inequities < 4 & conditions_systemic_inequities > 0, 1, 0),
         rank_internal = ifelse(conditions_negative_factors_inside_school < 4 & conditions_negative_factors_inside_school > 0, 1, 0),
         rank_other = ifelse(conditions_other_rank < 4 & conditions_other_rank > 0, 1, 0),
         rank_stakeholders = ifelse(conditions_stakeholder_demand < 4 & conditions_stakeholder_demand > 0, 1, 0),
         rank_student_agency = ifelse(conditions_lack_of_student_agency < 4 & conditions_lack_of_student_agency > 0, 1, 0),
         rank_teacher_agency = ifelse(conditions_lack_of_teacher_agency < 4 & conditions_lack_of_teacher_agency > 0, 1, 0)) %>% 
  mutate(across(.cols = all_of(conditions), .fns = ~ifelse(!is.na(.), 1, 0))) %>% 
  mutate(across(.cols = starts_with("rank"), .fns = ~ifelse(is.na(.), 0, .))) %>% 
  #select relevant cols
  select(all_of(cols21)) %>% 
  #rename cols
  rename_with(~merge21)
#merge nominator data
nom_21 <- import(here("data", "nominations_2021.csv")) %>% 
  select(school_id, nominator) %>% 
  mutate(year = 2021)
# dat21 <- dat21 %>% 
#   left_join(nom, by = "school_id")
nom_19 <- dat19 %>% 
  select(school_id, nominator) %>% 
  mutate(year = 2019)
merge_nom <- bind_rows(nom_21, nom_19) %>% 
  group_by(school_id) %>%
  summarise(nominator = if(n_distinct(nominator) == 1) first(nominator) else toString(nominator))
dat21 <- dat21 %>% 
  left_join(merge_nom, by = "school_id")
rm(merge21, cols21, dat21a, dat21b, nom_19, nom_21, merge_nom, conditions)


####################################
########## 2022 DATASET ############
####################################
load("data/complete_canopy_2022.RData")
dat22 <- clean_data_full %>% 
  rename(hybrid = practices_hybrid) %>% 
  select(-starts_with("practices"), -starts_with("core"), -starts_with("time")) %>% 
  #standardize percent cols
  mutate(across(ends_with("percent"), ~round(., 2))) %>% 
  #standardize race/gender
  mutate(confidential_leader_gender = case_when(
    confidential_leader_gender == "0" ~ NA,
    confidential_leader_gender == "No Response" ~ NA,
    TRUE ~ as.character(confidential_leader_gender)
  ),
  confidential_leader_race = case_when(
    confidential_leader_race == "AIAN" ~ "American Indian & Alaska Native",
    confidential_leader_race == "Hispanic" ~ "Latinx",
    confidential_leader_race == "Biracial" ~ "Multiple races/ethnicities",
    confidential_leader_race == "No Response" ~ NA,
    TRUE ~ as.character(confidential_leader_race)
  ),
  confidential_leadership_team_diversity = case_when(
    confidential_leadership_team_diversity == "0" ~ NA,
    confidential_leadership_team_diversity == "Not sure" ~ NA,
    TRUE ~ as.character(confidential_leadership_team_diversity)
  ),
  confidential_teaching_staff_diversity = case_when(
    confidential_teaching_staff_diversity == "0" ~ NA,
    confidential_teaching_staff_diversity == "Not sure" ~ NA,
    TRUE ~ as.character(confidential_teaching_staff_diversity)
  ),
  pct_bipoc = 1 - white_percent,
  #merge enrollment
  enrollment = case_when(
    !is.na(self_reported_total_enrollment) ~ self_reported_total_enrollment,
    is.na(self_reported_total_enrollment) & !is.na(nces_total_enrollment) ~ nces_total_enrollment
  ),
  #standardize other text vars
  focus_other_student_group_text = case_when(
    focus_other_student_group_text == "" ~ NA,
    focus_other_student_group_text == "0" ~ NA,
    focus_other_student_group_text == "-99" ~ NA,
    TRUE ~ as.character(focus_other_student_group_text)
  ),
  school_descriptor_other_text = case_when(
    school_descriptor_other_text == "0" ~ NA,
    school_descriptor_other_text == "-99" ~ NA,
    TRUE ~ as.character(school_descriptor_other_text)
  ),
  suggested_tags_s = case_when(
    suggested_tags_s == "0" ~ NA,
    suggested_tags_s == "-99" ~ NA,
    TRUE ~ as.character(suggested_tags_s)
  ),
  #standardize locale var
  locale = as.character(locale),
     #manual recodes for missing locale were missing
  locale = case_when(
    school_id == 45 ~ "Rural",
    school_id == 150 ~ "Urban",
    school_id == 368 ~ "Suburban",
    school_id == 466 ~ "Multiple",
    school_id == 293 ~ "Multiple",
    school_id == 475 ~ "Multiple",
    school_id == 528 ~ "Urban",
    school_id == 532 ~ "Rural",
    school_id == 535 ~ "Rural",
    school_id == 571 ~ "Urban",
    school_id == 573 ~ "Urban",
    school_id == 586 ~ "Urban",
    school_id == 610 ~ "Suburban",
    school_id == 612 ~ "Urban",
    TRUE ~ locale
  ),
  locale_rural = ifelse(locale == "Rural", 1, 0),
  locale_urban = ifelse(locale == "Urban", 1, 0),
  locale_suburban = ifelse(locale == "Suburban", 1, 0),
  #standardize ELA/math var
  self_reported_ela = round(self_reported_ela/100, 2),
  self_reported_math = round(self_reported_math/100, 2),
  #standardize school type
  type = case_when(
    school_descriptor_charter == 1 ~ "Public charter school",
    school_descriptor_district == 1 ~ "Public district school",
    school_descriptor_independent == 1 ~ "Independent (private) school",
    #replicating manual recodes from 2022 for missing 
    school_id == 24 ~ "Public charter school",
    school_id == 41 ~ "Public district school",
    school_id == 204 ~ "Public district school",
    school_id == 417 ~ "Public district school",
    school_id == 571 ~ "Independent (private) school",
    school_id == 573 ~ "Independent (private) school",
    school_id == 612 ~ "Independent (private) school",
    school_id == 615 ~ "Independent (private) school",
    school_id == 572 ~ "Independent (private) school",
    school_id == 604 ~ "Independent (private) school"
  ),
  #create year var
  year = 2022) %>% 
  #convert all grades to numeric
  mutate(across(starts_with("grades"), ~as.numeric(.))) %>% 
  #standardize leaps
  #this is a dumb workaround but it works
  rename_with(.fn = ~str_remove(., "leaps_"), .cols = starts_with("leaps_rank")) %>% 
  mutate(across(starts_with("leap"), ~ifelse(. == "0", 0, 1))) %>% 
  rename_with(.fn = ~paste0("leaps_", .), .cols = starts_with("rank")) %>% 
  #select relevant cols
  select(all_of(cols22)) %>% 
  #rename cols
  rename_with(~merge22)
rm(clean_data_dictionary, demographic_data, learning_model_data, practices_data, survey_data, school_data, clean_data_full, cols22, merge22)



####################################
########## 2023 DATASET ############
####################################
load("data/complete_canopy_2023.RData")
dat23 <- full %>% 
  select(-starts_with("practices"), -starts_with("core"), -starts_with("time")) %>% 
  #standardize gender/race
  mutate(leader1_gender = case_when(
    leader1_gender == "1" ~ "Man",
    leader1_gender == "2" ~ "Woman",
    leader1_gender == "3" ~ "Nonbinary",
    leader1_gender == "1,2" ~ "Multiple",
    leader1_gender == "1,2,3,4" ~ "Multiple",
    leader1_gender == "0" ~ "Prefer not to say"
  ),
  leader1_race = case_when(
    leader1_race == "1" ~ "American Indian & Alaska Native",
    leader1_race == "2" ~ "Asian",
    leader1_race == "3" ~ "Black",
    leader1_race == "4" ~ "Latinx",
    leader1_race == "5" ~ "Native Hawaiian & Pacific Islander",
    leader1_race == "6" ~ "White",
    leader1_race == "7" & leader1_race_self_identify_text == "Several principals" ~ NA,
    leader1_race == "7" & leader1_race_self_identify_text == "Other" ~ NA,
    leader1_race == "7" & leader1_race_self_identify_text == "Iranian/Middle Eastern" ~ "Middle Eastern",
    leader1_race == "7" & leader1_race_self_identify_text == "African American, Jewish, Italian, French," ~ "Multiple races/ethnicities",
    is.na(leader1_race) ~ NA,
    TRUE ~ "Multiple races/ethnicities"
  ),
  leadership_diversity = case_when(
    leadership_diversity == 1 ~ "0 - 24% people of color",
    leadership_diversity == 2 ~ "25 - 49% people of color",
    leadership_diversity == 3 ~ "50 - 74% people of color",
    leadership_diversity == 4 ~ "75 - 100% people of color",
    leadership_diversity == 0 ~ "Prefer not to say"
  ),
  teaching_diversity = case_when(
    teaching_diversity == 1 ~ "0 - 24% people of color",
    teaching_diversity == 2 ~ "25 - 49% people of color",
    teaching_diversity == 3 ~ "50 - 74% people of color",
    teaching_diversity == 4 ~ "75 - 100% people of color",
    teaching_diversity == 0 ~ "Prefer not to say"
  ),
  pct_bipoc = round(pct_bipoc/100, 2),
  #standardize school descriptor
  school_descriptor = case_when(
    school_descriptor == 1 ~ "Public district school",
    school_descriptor == 2 ~ "Public charter school",
    school_descriptor == 3 ~ "Independent (private) school"
  ),
  school_descriptor_other = case_when(
    school_descriptor_other == 1 & school_descriptor_other_text == "N/A" ~ 0,
    school_descriptor_other == 1 & school_descriptor_other_text == "None of the above" ~ 0,
    TRUE ~ school_descriptor_other
  ),
  school_desciptor_other_text = case_when(
    school_descriptor_other_text == "N/A" ~ NA,
    school_descriptor_other_text == "None of the above" ~ NA
  ),
  #standardize ELA & math vars
  self_reported_ela = round(self_reported_ela/100, 2),
  self_reported_math = round(self_reported_math/100, 2),
  #standardize percent vars
  self_reported_ell = round(self_reported_ell/100, 2),
  self_reported_frpl = round(self_reported_frpl/100, 2),
  self_reported_swd = round(self_reported_swd/100, 2),
  #add year
  year = 2023) %>% 
  mutate(across(starts_with("self_reported_race"), ~round(./100, 2))) %>% 
  #select relevant cols
  select(all_of(cols23)) %>% 
  #rename cols
  rename_with(~merge23)
rm(confidential, dictionary, full, impact, schools, tags, cols23, merge23)


####################################
########## 2024 DATASET ############
####################################
load("data/2024 data/complete_canopy_2024.RData")
dat24 <- full %>% 
  #drop tag data
  select(-starts_with("practices"), -starts_with("core"), -starts_with("time"), -starts_with("pilot")) %>% 
  #add year
  mutate(year = 2024) %>% 
  #select relevant cols
  select(all_of(cols24)) %>% 
  #rename cols
  rename_with(~merge24)
rm(dictionary, full, tags, var, variables, cols24, merge24)


#####################################################
########## COMPLETE LONGITUDINAL DATASET ############
#####################################################
order <- import(here("data", "var_order.csv")) %>% 
  pull(order)
long_dat <- bind_rows(dat19, dat21, dat22, dat23, dat24) %>% 
  #organize cols
  select(all_of(order)) %>%
  #backwards/forwards fill time-invariant vars
  group_by(school_id) %>%
  arrange(year) %>%
  fill(school_type,
       school_locale,
       locale_rural,
       locale_urban,
       locale_suburban,
       school_address,
       school_city,
       school_state,
       school_district,
       website,
       school_desc_homeschool,
       school_desc_hybrid,
       school_desc_micro,
       school_desc_sws,
       school_desc_virtual,
       school_desc_magnet,
       school_desc_title_i,
       grades_pk,
       grades_elementary,
       grades_middle,
       grades_high,
       focus_bipoc,
       focus_economic_disadvantage,
       focus_emergent_bilingual,
       focus_foster,
       focus_homeless,
       focus_interrupted,
       focus_juvenile_justice,
       focus_multilingual,
       focus_newcomer,
       focus_swd,
       focus_other,
       inclusive_emergent_bilingual,
       inclusive_juvenile_justice,
       inclusive_newcomer,
       inclusive_swd,
       inclusive_other,
       .direction = "downup") %>%
  ungroup()
#save dataset
write.csv(long_dat, "data/longitudinal/longitudinal_data.csv", row.names = FALSE)
#still need to close-comb fill and address any remaining NA we're able

