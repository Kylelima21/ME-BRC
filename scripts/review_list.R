rlist <- read.csv("data/maine-brc_review_list.csv") %>% 
  tibble() %>% 
  rename("common.name" = "species")

tax <- read.csv("data/ebird_taxonomy_v2022.csv") %>% 
  tibble() %>% 
  rename_with(tolower) %>% 
  select("common.name" = "primary_com_name", "scientific.name" = "sci_name")


new <- left_join(rlist, tax, by = "common.name")


write.csv(new, "outputs/final_review_species_list.csv", row.names = F)
