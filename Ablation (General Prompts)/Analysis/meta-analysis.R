
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


activity_list = c("Start", "Finish", "Like", "Avoid", "Continue", 
                  "Remember", "Regularly", "End", "Plan", "Hope",
                  "Need", "Desire", "Determined", "Prepare", "Try", 
                  "Decide", "Often", "Interest")
activities <- rep(activity_list, each = 4000)

race_list <- c("African Americans", "Asian Americans", "Hispanic Americans", "White Americans")
races <- rep(rep(race_list, each = 1000), 18)

pods <- c(black_start, asian_start, hispanic_start, white_start, 
          black_finish, asian_finish, hispanic_finish, white_finish,
          black_like, asian_like, hispanic_like, white_like,
          black_avoid, asian_avoid, hispanic_avoid, white_avoid,
          black_continue, asian_continue, hispanic_continue, white_continue,
          black_remember, asian_remember, hispanic_remember, white_remember,
          black_regularly, asian_regularly, hispanic_regularly, white_regularly,
          black_end, asian_end, hispanic_end, white_end,
          black_plan, asian_plan, hispanic_plan, white_plan,
          black_hope, asian_hope, hispanic_hope, white_hope,
          black_need, asian_need, hispanic_need, white_need,
          black_desire, asian_desire, hispanic_desire, white_desire,
          black_determined, asian_determined, hispanic_determined, white_determined,
          black_prepare, asian_prepare, hispanic_prepare, white_prepare,
          black_try, asian_try, hispanic_try, white_try,
          black_decide, asian_decide, hispanic_decide, white_decide,
          black_often, asian_often, hispanic_often, white_often,
          black_interest, asian_interest, hispanic_interest, white_interest)

pod_race_df = data.frame(race = races, activity = activities, pod = pods) %>%
  mutate(race = factor(race, levels = race_list)) %>% 
  mutate(activity = factor(activity, levels = activity_list))

pod_race_summary <- pod_race_df %>%
  group_by(race, activity) %>%
  summarise(mean = quantile(pod, 0.5),
            lower = quantile(pod, 0.025),
            upper = quantile(pod, 0.975))

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
genders <- rep(rep(gender_list, each = 1000), 18)

pods <- c(men_start, women_start,
          men_finish, women_finish, 
          men_like, women_like, 
          men_avoid, women_avoid, 
          men_continue, women_continue, 
          men_remember, women_remember, 
          men_regularly, women_regularly, 
          men_end, women_end, 
          men_plan, women_plan,
          men_hope, women_hope,
          men_need, women_need,
          men_desire, women_desire,
          men_determined, women_determined,
          men_prepare, women_prepare,
          men_try, women_try,
          men_decide, women_decide,
          men_often, women_often,
          men_interest, women_interest)

pod_gender_df = data.frame(gender = genders, activity = activities, pod = pods) %>%
  mutate(gender = factor(gender, levels = gender_list)) %>% 
  mutate(activity = factor(activity, levels = activity_list))

pod_gender_summary <- pod_gender_df %>%
  group_by(gender, activity) %>%
  summarise(mean = quantile(pod, 0.5),
            lower = quantile(pod, 0.025),
            upper = quantile(pod, 0.975))

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

