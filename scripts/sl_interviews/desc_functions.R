library(here)
library(rio)
library(tidyverse)
long_dat <- import(here("data/longitudinal", "tags-long.csv"))

### justice tags ####
pull_justice_tags <- function(id){
  cluster <- import(here("data/longitudinal", "tag_clusters_longitudinal.csv")) %>% 
  janitor::clean_names()

  labels <- import(here("data", "tag_labels.csv"))

  justice_tags <- cluster %>% 
    filter(proposed_cluster_for_preliminary_24_analysis == "Ed justice") %>% 
    select(label = tag) %>% 
    left_join(labels, by = "label") %>% 
    pull(var)
  data <- long_dat %>% 
    filter(school_id == id) %>% 
    filter(var %in% justice_tags) %>% 
    pivot_wider(names_from = var,
              values_from = usage)
  return(data)
}

test <- pull_justice_tags(132)

#### blended learning tags ####
pull_blended_tags <- function(id){
  blended_tags <- c("practices_blended_learning", 
                    "practices_a_la_carte", 
                    "practices_flipped_classroom", 
                    "practices_flex", 
                    "practices_enriched_virtual", 
                    "practices_station_rotation")
  data <- long_dat %>% 
    filter(school_id == id) %>% 
    filter(var %in% blended_tags) %>% 
    pivot_wider(names_from = var,
                values_from = usage)
  return(data)
}

test <- pull_blended_tags(209)
