library(googlesheets4)
library(readr)
library(here)
library(dplyr)

old_clusters = read_sheet(
  ss = "1E5-VWIqyq1XRhDvU6cLon9TwvribO0tpV0tEIlW_4I0",
  sheet = "2024 Tags by Cluster"
)

tag_labs = read_csv(here("data/tag_labels.csv"))

old_clusters = old_clusters |>
  left_join(tag_labs, by = c("Tag" = "label")) |>
  select(Tag, var, cluster = `Proposed Cluster for Preliminary '24 Analysis`) |>
  mutate(cluster = ifelse(cluster == "None?", "None", cluster)) ## standardizing

if(anyNA(old_clusters$var)) stop("need to fill in missing tag names!")

old_clusters |>
  select(var, cluster) |>
  write_csv(file = here("data/clusters_through_2024.csv"))
