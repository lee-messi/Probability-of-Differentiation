
## Anonymous
# Probability of Differentiation

## Script date: 4 May 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("effsize")){install.packages("effsize", dependencies = TRUE); require("effsize")}
if(!require("meta")){install.packages("meta", dependencies = TRUE); require("meta")}

# Load data --------------------------------------------------------------------

load("pod.RData")

# Organize Dataframe (Race/Ethnicity) ------------------------------------------

activity_list = c("Car", "Festival", "Food", "Hobby", "Job", "Major", "Music", "State")
activities <- rep(activity_list, each = 4000)

race_list <- c("African Americans", "Asian Americans", "Hispanic Americans", "White Americans")
races <- rep(rep(race_list, each = 1000), 8)

pods <- c(black_car, asian_car, hispanic_car, white_car, 
          black_festival, asian_festival, hispanic_festival, white_festival,
          black_food, asian_food, hispanic_food, white_food,
          black_hobby, asian_hobby, hispanic_hobby, white_hobby,
          black_job, asian_job, hispanic_job, white_job,
          black_major, asian_major, hispanic_major, white_major,
          black_music, asian_music, hispanic_music, white_music,
          black_state, asian_state, hispanic_state, white_state)

pod_race_df = data.frame(race = races, activity = activities, pod = pods) %>%
  mutate(race = factor(race, levels = race_list)) %>% 
  mutate(activity = factor(activity, levels = activity_list))

pod_race_summary <- pod_race_df %>%
  group_by(race, activity) %>%
  summarise(mean = quantile(pod, 0.5),
            lower = quantile(pod, 0.025),
            upper = quantile(pod, 0.975),
            se = (upper - lower) / (2 * 1.96))

# Cohen's d (Race/Ethnicity) ---------------------------------------------------

race.d <- function(df){
  
  final.df = NULL
  
  for(activity_name in activity_list){
  
    black = df %>% filter(activity == activity_name & race == "African Americans") %>% pull(pod)
    asian = df %>% filter(activity == activity_name & race == "Asian Americans") %>% pull(pod)
    hispanic = df %>% filter(activity == activity_name & race == "Hispanic Americans") %>% pull(pod)
    white = df %>% filter(activity == activity_name & race == "White Americans") %>% pull(pod)
    
    white.black.d = cohen.d(white, black)
    white.asian.d = cohen.d(white, asian)
    white.hispanic.d = cohen.d(white, hispanic)
    
    white.black = data.frame(comparison = "White v. African Americans", 
                             activity = activity_name,
                             d = white.black.d$estimate, 
                             lower = white.black.d$conf.int[1], 
                             upper = white.black.d$conf.int[2])
    white.asian = data.frame(comparison = "White v. Asian Americans", 
                             activity = activity_name,
                             d = white.asian.d$estimate, 
                             lower = white.asian.d$conf.int[1], 
                             upper = white.asian.d$conf.int[2])
    white.hispanic = data.frame(comparison = "White v. Hispanic Americans", 
                                activity = activity_name,
                                d = white.hispanic.d$estimate, 
                                lower = white.hispanic.d$conf.int[1], 
                                upper = white.hispanic.d$conf.int[2])
    
    temp.df = rbind(white.black, white.asian, white.hispanic)
    final.df = rbind(final.df, temp.df)
    
  }
  
  row.names(final.df) = NULL
  return(final.df)
  
}

race.d.df = race.d(pod_race_df) %>% 
  mutate(activity = factor(activity, levels = activity_list))

# Meta Analysis (Race/Ethnicity) -----------------------------------------------

# Convert 95% CI to SE
race.d.df$se <- (race.d.df$upper - race.d.df$lower) / (2 * 1.96)

# Initialize an empty list to store meta-analysis results
race.meta <- list()

# Unique comparison groups
unique_comparisons <- unique(race.d.df$comparison)

# Loop through each comparison group and perform meta-analysis
for (comp in unique_comparisons) {
  # Subset data for the current comparison group
  subset_data <- race.d.df %>% filter(comparison == comp)
  
  # Perform meta-analysis
  meta_analysis <- metagen(
    TE = subset_data$d,
    seTE = subset_data$se,
    studlab = subset_data$activity,
    sm = "SMD"
  )
  
  # Store the results
  race.meta[[comp]] <- meta_analysis
}

race.meta

# Organize Dataframe (Gender) --------------------------------------------------

activities <- rep(activity_list, each = 2000)

gender_list <- c("Men", "Women")
genders <- rep(rep(gender_list, each = 1000), 8)

pods <- c(men_car, women_car,
          men_festival, women_festival, 
          men_food, women_food, 
          men_hobby, women_hobby, 
          men_job, women_job, 
          men_major, women_major, 
          men_music, women_music, 
          men_state, women_state)

pod_gender_df = data.frame(gender = genders, activity = activities, pod = pods) %>%
  mutate(gender = factor(gender, levels = gender_list)) %>% 
  mutate(activity = factor(activity, levels = activity_list))

pod_gender_summary <- pod_gender_df %>%
  group_by(gender, activity) %>%
  summarise(mean = quantile(pod, 0.5),
            lower = quantile(pod, 0.025),
            upper = quantile(pod, 0.975),
            se = (upper - lower) / (2 * 1.96))

# Cohen's d (Race/Ethnicity) ---------------------------------------------------

gender.d <- function(df){
  
  final.df = NULL
  
  for(activity_name in activity_list){
    
    men = df %>% filter(activity == activity_name & gender == "Men") %>% pull(pod)
    women = df %>% filter(activity == activity_name & gender == "Women") %>% pull(pod)
    
    men.women.d = cohen.d(men, women)
    
    men.women = data.frame(comparison = "Men v. Women", 
                           activity = activity_name,
                           d = men.women.d$estimate, 
                           lower = men.women.d$conf.int[1], 
                           upper = men.women.d$conf.int[2])
    
    final.df = rbind(final.df, men.women)
    
  }
  
  row.names(final.df) = NULL
  return(final.df)
  
}

gender.d.df = gender.d(pod_gender_df) %>% 
  mutate(activity = factor(activity, levels = activity_list))

# Meta Analysis (Gender) -------------------------------------------------------

# Convert 95% CI to SE
gender.d.df$se <- (gender.d.df$upper - gender.d.df$lower) / (2 * 1.96)

# Initialize an empty list to store meta-analysis results
gender.meta <- list()

# Unique comparison groups
unique_comparisons <- unique(gender.d.df$comparison)

# Loop through each comparison group and perform meta-analysis
for (comp in unique_comparisons) {
  # Subset data for the current comparison group
  subset_data <- gender.d.df %>% filter(comparison == comp)
  
  # Perform meta-analysis
  meta_analysis <- metagen(
    TE = subset_data$d,
    seTE = subset_data$se,
    studlab = subset_data$activity,
    sm = "SMD"
  )
  
  # Store the results
  gender.meta[[comp]] <- meta_analysis
}

gender.meta

