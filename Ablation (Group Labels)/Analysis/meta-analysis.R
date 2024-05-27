
## Anonymous
# Probability of Differentiation

## Script date: 22 May 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("effsize")){install.packages("effsize", dependencies = TRUE); require("effsize")}
if(!require("meta")){install.packages("meta", dependencies = TRUE); require("meta")}

# Load data --------------------------------------------------------------------

load("pod.RData")

# Organize Dataframe (Race/Ethnicity) ------------------------------------------

activity_list = c("Sports", "Exam", "Preparing Food", "Eating", "Drinking", 
                  "Communicating", "Movies", "Commuting", "Online", "Video games",
                  "Reading", "Working", "Shopping", "Grooming", "Waiting", 
                  "Sleep", "Music", "Telephone")
activities <- rep(activity_list, each = 4000)

race_list <- c("African Americans", "Asian Americans", "Hispanic Americans", "White Americans")
races <- rep(rep(race_list, each = 1000), 18)

pods <- c(black_sports, asian_sports, hispanic_sports, white_sports, 
          black_exams, asian_exams, hispanic_exams, white_exams,
          black_preparing_food, asian_preparing_food, hispanic_preparing_food, white_preparing_food,
          black_eating, asian_eating, hispanic_eating, white_eating,
          black_drinking, asian_drinking, hispanic_drinking, white_drinking,
          black_communicating, asian_communicating, hispanic_communicating, white_communicating,
          black_movies, asian_movies, hispanic_movies, white_movies,
          black_commuting, asian_commuting, hispanic_commuting, white_commuting,
          black_online, asian_online, hispanic_online, white_online,
          black_videogames, asian_videogames, hispanic_videogames, white_videogames,
          black_reading, asian_reading, hispanic_reading, white_reading,
          black_working, asian_working, hispanic_working, white_working,
          black_shopping, asian_shopping, hispanic_shopping, white_shopping,
          black_grooming, asian_grooming, hispanic_grooming, white_grooming,
          black_waiting, asian_waiting, hispanic_waiting, white_waiting,
          black_sleep, asian_sleep, hispanic_sleep, white_sleep,
          black_music, asian_music, hispanic_music, white_music,
          black_telephone, asian_telephone, hispanic_telephone, white_telephone)

pod.race.df = data.frame(race = races, activity = activities, pod = pods) %>%
  mutate(race = factor(race, levels = race_list)) %>% 
  mutate(activity = factor(activity, levels = activity_list))

pod.race.summary <- pod.race.df %>%
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
    
    white.black.t = t.test(white, black)
    white.asian.t = t.test(white, asian)
    white.hispanic.t = t.test(white, hispanic)
    
    white.black.d = cohen.d(white, black)
    white.asian.d = cohen.d(white, asian)
    white.hispanic.d = cohen.d(white, hispanic)
    
    white.black = data.frame(comparison = "White v. African Americans", 
                             activity = activity_name,
                             d = white.black.d$estimate, 
                             lower = white.black.d$conf.int[1], 
                             upper = white.black.d$conf.int[2],
                             t = white.black.t$statistic[['t']], 
                             df = white.black.t$parameter[['df']],
                             p = white.black.t$p.value)
    white.asian = data.frame(comparison = "White v. Asian Americans", 
                             activity = activity_name,
                             d = white.asian.d$estimate, 
                             lower = white.asian.d$conf.int[1], 
                             upper = white.asian.d$conf.int[2],
                             t = white.asian.t$statistic[['t']], 
                             df = white.asian.t$parameter[['df']],
                             p = white.asian.t$p.value)
    white.hispanic = data.frame(comparison = "White v. Hispanic Americans", 
                                activity = activity_name,
                                d = white.hispanic.d$estimate, 
                                lower = white.hispanic.d$conf.int[1], 
                                upper = white.hispanic.d$conf.int[2],
                                t = white.hispanic.t$statistic[['t']], 
                                df = white.hispanic.t$parameter[['df']],
                                p = white.hispanic.t$p.value)
    
    temp.df = rbind(white.black, white.asian, white.hispanic)
    final.df = rbind(final.df, temp.df)
    
  }
  
  row.names(final.df) = NULL
  return(final.df)
  
}

race.d.df = race.d(pod.race.df) %>% 
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
genders <- rep(rep(gender_list, each = 1000), 18)

pods <- c(men_sports, women_sports,
          men_exam, women_exam, 
          men_preparing_food, women_preparing_food, 
          men_eating, women_eating, 
          men_drinking, women_drinking, 
          men_communicating, women_communicating, 
          men_movies, women_movies, 
          men_commuting, women_commuting, 
          men_online, women_online,
          men_videogames, women_videogames,
          men_reading, women_reading,
          men_working, women_working,
          men_shopping, women_shopping,
          men_grooming, women_grooming,
          men_waiting, women_waiting,
          men_sleep, women_sleep,
          men_music, women_music,
          men_telephone, women_telephone)

pod.gender.df = data.frame(gender = genders, activity = activities, pod = pods)

pod.gender.summary <- pod.gender.df %>%
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
    men.women.t = t.test(men, women)
    
    men.women = data.frame(comparison = "Men v. Women", 
                           activity = activity_name,
                           d = men.women.d$estimate, 
                           lower = men.women.d$conf.int[1], 
                           upper = men.women.d$conf.int[2], 
                           t = men.women.t$statistic[['t']], 
                           df = men.women.t$parameter[['df']],
                           p = men.women.t$p.value)
    
    final.df = rbind(final.df, men.women)
    
  }
  
  row.names(final.df) = NULL
  return(final.df)
  
}

gender.d.df = gender.d(pod.gender.df) %>% 
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

# Remove all elements except for the ones we need ------------------------------

exclude = c("race.meta", "gender.meta", 
            "race.d.df", "gender.d.df")

rm(list = setdiff(ls(), exclude))

# White v. African Americans ---------------------------------------------------

c(race.meta[[1]]$I2, race.meta[[1]]$pval.Q)
white.black = race.d.df %>% filter(comparison == 'White v. African Americans')
white.black %>% filter(d > 0 & lower > 0) %>% nrow()
white.black %>% filter(d < 0 & upper < 0) %>% nrow()
white.black %>% slice_max(d)
white.black %>% slice_min(d)
c(race.meta[[1]]$TE.random, race.meta[[1]]$lower.random, race.meta[[1]]$upper.random)

# White v. Asian Americans -----------------------------------------------------

c(race.meta[[2]]$I2, race.meta[[2]]$pval.Q)
white.asian = race.d.df %>% filter(comparison == 'White v. Asian Americans')
white.asian %>% filter(d > 0 & lower > 0) %>% nrow()
white.asian %>% filter(d < 0 & upper < 0) %>% nrow()
white.asian %>% slice_max(d)
white.asian %>% slice_min(d)
c(race.meta[[2]]$TE.random, race.meta[[2]]$lower.random, race.meta[[2]]$upper.random)

# White v. Hispanic Americans --------------------------------------------------

c(race.meta[[3]]$I2, race.meta[[3]]$pval.Q)
white.hispanic = race.d.df %>% filter(comparison == 'White v. Hispanic Americans')
white.hispanic %>% filter(d > 0 & lower > 0) %>% nrow()
white.hispanic %>% filter(d < 0 & upper < 0) %>% nrow()
white.hispanic %>% slice_max(d)
white.hispanic %>% slice_min(d)
c(race.meta[[3]]$TE.random, race.meta[[3]]$lower.random, race.meta[[3]]$upper.random)

# Men v. Women -----------------------------------------------------------------

c(gender.meta[[1]]$I2, gender.meta[[1]]$pval.Q)
gender.d.df %>% filter(d > 0 & lower > 0) %>% nrow()
gender.d.df %>% filter(d < 0 & upper < 0) %>% nrow()
gender.d.df %>% slice_max(d)
gender.d.df %>% slice_min(d)
c(gender.meta[[1]]$TE.random, gender.meta[[1]]$lower.random, gender.meta[[1]]$upper.random)

