library(here)
library(rio)
library(tidyverse)
noms_19a <- import(here("data/nominations", "nominations_2019.csv"))
noms_19b <- import(here("data/nominations", "nominations_2019_confirmed.csv"))
noms_21 <- import(here("data/nominations", "nominations_2021.csv"))
noms_22 <- import(here("data/nominations", "nominations_2022.csv"))
noms_23 <- import(here("data/nominations", "nominations_2023.csv"))
noms_24 <- import(here("data/nominations", "nominations_2024-raw.csv"))
#NOTE: The CSV file for 2019b is manually modified - the original confirmed dataset includes a single observation/row for
#each school, meaning nominators and nominator reasons were collapsed. This was an issue because in instances where the
#nominator reason was not provided and multiple nominators were recorded, there is no way to cleanly split the data because
#we'd end up with differing keys/no way to identify NA values since it was manually stored, and not a "true" CSV value.
#Thus, I manually split these rows to get dup observations for long format - this will allow similar collapsing in long df.
#Importantly, I did not confirm reasons matched with nominator - because we're collapsing at the end for the portal this should
#not matter, but has implications for any potential future analysis of reasons paired alongside org/org demographics.

##########################
#### 2024 NOMINATIONS ####
##########################

#create function for fixing state names
#first, create state abbreviation function
state_abbreviated <- function(name) {
  name_upper <- toupper(name)
  
  if (toupper(name) == "DISTRICT OF COLUMBIA") {
    return("District of Columbia")
  } else if (name_upper == "DC") {
    return("District of Columbia")
  } else if (name_upper %in% state.abb) {
    return(tools::toTitleCase(state.name[which(state.abb == name_upper)]))
  } else if (toupper(name) %in% toupper(state.name)) { 
    return(tools::toTitleCase(name))
  } else {
    return(NA) 
  }
}
#select relevant cols matching public data
noms_24 <- noms_24 %>% 
  janitor::clean_names() %>% 
  #drop extra cols
  #note we're retaining public org name & not original name in long
  select(-c(nomination_id, official_school_name, nominator_org)) %>% 
  #start with renames
  rename(
    nominator = publicly_disclosable_nominator_org,
    school_id = canopy_school_id,
    school_name_n = school_name,
    school_city_n = school_city,
    school_state_n = school_state,
    school_district_n = district_or_cmo,
    nominator_reason_n = reason_for_nominating
  ) %>% 
  #fix state names
  mutate(school_state_n = sapply(school_state_n, state_abbreviated))

#save
write.csv(noms_24, "data/nominations/nominations_2024.csv", row.names = FALSE)

#prep for longitudinal
noms_24 <- noms_24 %>% 
  mutate(school_district_n = str_remove_all(school_district_n, "n/a|N/A|-"),
         school_district_n = na_if(school_district_n, ""),
         year = 2024)

##########################
#### 2023 NOMINATIONS ####
##########################
noms_23 <- noms_23 %>% 
  mutate(school_district_n = str_remove_all(school_district_n, "N/A|\\(single site charter\\)"),
         school_district_n = na_if(school_district_n, ""),
         school_district_n = na_if(school_district_n, " "),
         school_district_n = na_if(school_district_n, "Unsure"),
         school_id = case_when(
           school_id == "3, 687" ~ 3,
           TRUE ~ as.numeric(school_id)
         ),
         year = 2023) %>% 
  rename(school_name_n = school_name)
#note - there's an issue here with a nomination for school_id "3,687" that appears to be a single school with 2 IDs
#in the process of checking out, in the meantime, will convert all IDs to character to circumvent issue

##########################
#### 2022 NOMINATIONS ####
##########################
noms_22 <- noms_22 %>% 
  mutate(school_district_n = str_remove_all(school_district_n, "(?i)n/a"),
         school_district_n = na_if(school_district_n, ""),
         year = 2022) %>% 
  rename(nominator_reason_n = nominator_reason)

##########################
#### 2021 NOMINATIONS ####
##########################
noms_21 <- noms_21 %>% 
  mutate(school_state_n = str_to_title(school_state_n),
         school_state_n = sapply(school_state_n, state_abbreviated),
         school_district_n = ifelse(grepl("(?i)\\b(n/a|independent|private)\\b", school_district_n), NA, school_district_n),
         school_district_n = na_if(school_district_n, ""),
         year = 2021) %>% 
  select(nominator, school_id, school_name_n, school_city_n, school_state_n, school_district_n, nominator_reason_n = nominator_reason, year)

##########################
#### 2019 NOMINATIONS ####
##########################
#begin prepping dataset 1 (Nominated Schools) for long
noms_19a <- noms_19a %>% 
  mutate(school_district = ifelse(grepl("(?i)\\b(n/a|none)\\b", school_district), NA, school_district),
         school_district = na_if(school_district, ""),
         year = 2019) %>% 
  select(nominator, school_id, school_name_n = school_name, school_city_n = school_city, school_state_n = school_state, school_district_n = school_district, nominator_reason_n = nominator_reason, year)
#prep dataset 2 with CSV (Confirmed Schools) for long
noms_19b <- noms_19b %>% 
  mutate(school_district = ifelse(grepl("(?i)\\b(n/a|none|single site)\\b", school_district), NA, school_district),
         school_district = na_if(school_district, ""),
         year = 2019) %>% 
  select(nominator, school_id, school_name_n = school_name, school_city_n = school_city, school_state_n = school_state, school_district_n = school_district, nominator_reason_n = nominator_reason, year)
#merge
noms_19 <- bind_rows(noms_19a, noms_19b)
###################################
#### MERGE NOMINATION DATASETS ####
###################################
merge <- bind_rows(noms_19, noms_21, noms_22, noms_23, noms_24) %>% 
  mutate(nominator_reason_n = ifelse(grepl("(?i)\\b(N/A|none)\\b", nominator_reason_n), NA, nominator_reason_n),
         nominator_reason_n = na_if(nominator_reason_n, ""),
         school_city_n = na_if(school_city_n, ""),
         school_state_n = na_if(school_state_n, ""))
#check for missing nomination reasons
missing <- merge %>% 
  filter(is.na(nominator_reason_n)) #19 missing
#save long format
write.csv(merge, "data/longitudinal/longitudinal_nominations.csv", row.names = FALSE)

#export list for Amber
portal <- merge %>% 
  select(nominator, school_id, year, nominator_reason_n) %>% 
  #group reasons by year
  arrange(school_id, desc(year)) %>% 
  group_by(school_id) %>% 
  filter(year == max(year)) %>%
  ungroup()
  # summarise(
  #   nominator = toString(unique(nominator)),
  #   nominator_reason = toString(unique(nominator_reason_n)),
  #   .groups = "drop"
  # ) %>% 
  # #NA rewritten as "NA" - fix
  # mutate(nominator_reason = na_if(nominator_reason, "NA")) %>% 
  # #filter(!is.na(nominator_reason)) %>%
  # arrange(school_id, desc(year)) %>%
  # group_by(school_id) %>%
  # slice(1) %>%
  # ungroup() %>% 
  # mutate(school_id = as.numeric(school_id))
  #change back to long format

#pull missing IDs to ensure they won't be missing
missing_id <- missing %>% 
  pull(school_id)
#check portal
portal %>% filter(school_id %in% missing_id) #43 obs will remain missing in full collapse
#check long df
merge %>% filter(school_id %in% missing_id) #come from 49 original missing values in long df
#how many schools will this affect?
portal_schools <- import(here("data/nominations", "portal_schools_2024.csv")) %>% 
  janitor::clean_names() %>% 
  pull(canopy_id)
missing %>%  filter(school_id %in% portal_schools) #7 will have missing reasons in the portal
#filter to schools in portal
check <- portal %>% 
  filter(school_id %in% portal_schools) #this gets me to 314 - but there's 319 records in the portal. I'm not sure where those 5 went
#still more complete than previously, we'll bite the 12 with missing reasons and call it a day
#which ones are missing?
portal_missing <- check %>% 
  pull(school_id)
setdiff(portal_schools, portal_missing) #School ID: 497 517 519 530 628

#add in org names
org_names <- import(here("data/nominations", "org-name-check.csv")) %>% 
  janitor::clean_names() %>% 
  #drop missing canopy ID
  filter(canopy_school_id != "") %>% 
  #fix issue with the 1 school with multiple IDs
  mutate(canopy_school_id = ifelse(canopy_school_id == "3, 687", 3, as.numeric(canopy_school_id)),
         #create year column
         year = case_when(
           grepl("2019", nomination_id) ~ 2019,
           grepl("2020", nomination_id) ~ 2021,
           grepl("2021", nomination_id) ~ 2022,
           grepl("2022", nomination_id) ~ 2023,
           grepl("2023", nomination_id) ~ 2024
         ),
         test_match = publicly_disclosable_nominator_org) %>% 
  #group names by year
  arrange(canopy_school_id, desc(year)) %>% 
  group_by(canopy_school_id) %>% 
  filter(year == max(year)) %>%
  ungroup() %>% 
  #filter to portal schools
  filter(canopy_school_id %in% portal_schools) %>% 
  select(year, school_id = canopy_school_id, org_name = nominator_org, public_org_name = publicly_disclosable_nominator_org, test_match)
#merge names
check <- check %>% 
  mutate(test_match = nominator) %>% 
  left_join(org_names, by = c("school_id", "year", "test_match"))

#save
write.csv(portal, "data/nominations/most-recent-nomination-reasons.csv", row.names = FALSE)
write.csv(check, "data/nominations/portal-most-recent-noms.csv", row.names = FALSE) #NOTE: filled in missing manually to resolve a few issues

#final check
portal_check <- import(here("data/nominations", "portal-most-recent-noms-manual-edit.csv")) %>% 
  unique()

#merge website info in for Sarah
website <- import(here("data/longitudinal", "longitudinal_data.csv")) %>% 
  select(school_id, school_name, website, year) %>% 
  right_join(merge, by = c("school_id", "year")) %>% 
  select(nominator, year, school_id, official_school_name = school_name, website, nominator_reason = nominator_reason_n)
  
write.csv(website, "data/nominations/all-nominations-websites.csv", row.names = FALSE)
