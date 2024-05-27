
## Anonymous
## Script date: 15 Dec 2023

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}

# Import text ------------------------------------------------------------------

names = read.csv('nameratings.csv')

# Select names by race ---------------------------------------------------------

set.seed(1048596)

sampled_names = names %>% group_by(race, gender) %>% 
  slice_sample(n = 15) %>% select(c("name", "race", "gender")) %>%
  ungroup()

# Write the sampled names as a .csv file
write.csv(sampled_names, "sampled_names.csv", row.names = FALSE)
