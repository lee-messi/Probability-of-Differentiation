
## Anonymous
# Probability of Differentiation

## Script date: 23 May 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("effsize")){install.packages("effsize", dependencies = TRUE); require("effsize")}
if(!require("meta")){install.packages("meta", dependencies = TRUE); require("meta")}

# Load data --------------------------------------------------------------------

# load("../Main/Analysis/pod.RData")
# load("../Ablation (Model)/Analysis/pod.RData")
# load("../Ablation (Group Labels)/Analysis/pod.RData")
load("../Ablation (Individual Prompt)/Analysis/pod.RData")

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

individual.race.df = race.d(pod.race.df) %>% 
  mutate(activity = factor(activity, levels = activity_list))

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

# Cohen's d (Gender) -----------------------------------------------------------

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

individual.gender.df = gender.d(pod.gender.df) %>% 
  mutate(activity = factor(activity, levels = activity_list))

# Save as .RData ---------------------------------------------------------------

exclude = c("individual.race.df", "individual.gender.df")
rm(list = setdiff(ls(), exclude))

save.image("individual_prompt.RData")

