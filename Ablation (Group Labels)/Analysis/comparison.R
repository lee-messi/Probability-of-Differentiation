
## Anonymous
# Probability of Differentiation

## Script date: 29 Mar 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}

# Define preprocessing function ------------------------------------------------

keywords = c('sorry')

preprocess = function(df, max.length){
  df <- df %>% 
    mutate(text = tolower(text)) %>% 
    mutate(text = gsub('[[:punct:] ]+', ' ', text)) %>% 
    mutate(text = trimws(text)) %>%
    rowwise() %>%
    mutate(length = str_count(text, '\\w+')) %>%
    filter(length < max.length) %>%
    filter(!any(sapply(keywords, function(k) str_detect(text, regex(k, ignore_case = TRUE))))) %>%
    ungroup() %>% 
    mutate(text = factor(text, levels = unique(text)),
           gender = case_when(
             gender == 'man' ~ 'Men',
             gender == 'woman' ~ 'Women',
             TRUE ~ as.character(gender)  # Handles cases where gender is neither 'M' nor 'F'
           ))
}

# Import text ------------------------------------------------------------------

sports = read.csv('../Data/sports.csv') %>% preprocess(4)
exam = read.csv('../Data/exam.csv') %>% preprocess(3)
preparing_food = read.csv('../Data/preparing_food.csv') %>% preprocess(4)
eating = read.csv('../Data/eating.csv') %>% preprocess(4)
drinking = read.csv('../Data/drinking.csv') %>% preprocess(5)
communicating = read.csv('../Data/communicating.csv') %>% preprocess(3)
movies = read.csv('../Data/movies.csv') %>% preprocess(10)
commuting = read.csv('../Data/commuting.csv') %>% preprocess(2)
online = read.csv('../Data/online.csv') %>% preprocess(4)
videogames = read.csv('../Data/videogames.csv') %>% preprocess(9)
reading = read.csv('../Data/reading.csv') %>% preprocess(9)
working = read.csv('../Data/working.csv') %>% preprocess(3)
shopping = read.csv('../Data/shopping.csv') %>% preprocess(3)
grooming = read.csv('../Data/grooming.csv') %>% preprocess(3)
waiting = read.csv('../Data/waiting.csv') %>% preprocess(5)
sleep = read.csv('../Data/sleeping.csv') %>% preprocess(5) # Tracy
music = read.csv('../Data/music.csv') %>% preprocess(3) # Fidel
telephone = read.csv('../Data/telephone.csv') %>% preprocess(2)

# Define function to estimate probability of differentiation (race) ------------

pod <- function(data, variable, group, R = 1000, conf.level = 0.05){
  
  if(variable == "race"){filtered_data <- data %>% filter(race == group)}
  else if(variable == "gender"){filtered_data <- data %>% filter(gender == group)}
  else(stop("Variable does not exist."))
  
  set.seed(1048596)
  pd_list = numeric(R)
  
  for (i in c(1:R)){
    
    sampled_data <- filtered_data %>% sample_n(nrow(filtered_data), replace = TRUE)
    
    # Calculate probability of differentiation
    text_table <- table(sampled_data$text)
    text_prop <- prop.table(text_table)
    text_prop_2 <- text_prop ** 2
    pd = 1 - sum(text_prop_2)
    
    # Store pd values inside a list
    pd_list[i] = pd
  }
  
  lower.bound = quantile(pd_list, conf.level/2)
  upper.bound = quantile(pd_list, 1 - conf.level/2)
  estimate = quantile(pd_list, 0.50)
  
  cat("Estimate:", estimate, " | CI: [", lower.bound, ", ",  upper.bound, "] \n")
  return(pd_list)
  
}

# Compute PoD for racial/ethnic groups across all domains ----------------------

black_sports = pod(sports, "race", 'African')
asian_sports = pod(sports, "race", 'Asian')
hispanic_sports = pod(sports, "race", 'Hispanic')
white_sports = pod(sports, "race", 'White')

black_exams = pod(exam, "race", 'African')
asian_exams = pod(exam, "race", 'Asian')
hispanic_exams = pod(exam, "race", 'Hispanic')
white_exams = pod(exam, "race", 'White')

black_preparing_food = pod(preparing_food, "race", 'African')
asian_preparing_food = pod(preparing_food, "race", 'Asian')
hispanic_preparing_food = pod(preparing_food, "race", 'Hispanic')
white_preparing_food = pod(preparing_food, "race", 'White')

black_eating = pod(eating, "race", 'African')
asian_eating = pod(eating, "race", 'Asian')
hispanic_eating = pod(eating, "race", 'Hispanic')
white_eating = pod(eating, "race", 'White')

black_drinking = pod(drinking, "race", 'African')
asian_drinking = pod(drinking, "race", 'Asian')
hispanic_drinking = pod(drinking, "race", 'Hispanic')
white_drinking = pod(drinking, "race", 'White')

black_communicating = pod(communicating, "race", 'African')
asian_communicating = pod(communicating, "race", 'Asian')
hispanic_communicating = pod(communicating, "race", 'Hispanic')
white_communicating = pod(communicating, "race", 'White')

black_movies = pod(movies, "race", 'African')
asian_movies = pod(movies, "race", 'Asian')
hispanic_movies = pod(movies, "race", 'Hispanic')
white_movies = pod(movies, "race", 'White')

black_commuting = pod(commuting, "race", 'African')
asian_commuting = pod(commuting, "race", 'Asian')
hispanic_commuting = pod(commuting, "race", 'Hispanic')
white_commuting = pod(commuting, "race", 'White')

black_online = pod(online, "race", 'African')
asian_online = pod(online, "race", 'Asian')
hispanic_online = pod(online, "race", 'Hispanic')
white_online = pod(online, "race", 'White')

black_videogames = pod(videogames, "race", 'African')
asian_videogames = pod(videogames, "race", 'Asian')
hispanic_videogames = pod(videogames, "race", 'Hispanic')
white_videogames = pod(videogames, "race", 'White')

black_reading = pod(reading, "race", 'African')
asian_reading = pod(reading, "race", 'Asian')
hispanic_reading = pod(reading, "race", 'Hispanic')
white_reading = pod(reading, "race", 'White')

black_working = pod(working, "race", 'African')
asian_working = pod(working, "race", 'Asian')
hispanic_working = pod(working, "race", 'Hispanic')
white_working = pod(working, "race", 'White')

black_shopping = pod(shopping, "race", 'African')
asian_shopping = pod(shopping, "race", 'Asian')
hispanic_shopping = pod(shopping, "race", 'Hispanic')
white_shopping = pod(shopping, "race", 'White')

black_grooming = pod(grooming, "race", 'African')
asian_grooming = pod(grooming, "race", 'Asian')
hispanic_grooming = pod(grooming, "race", 'Hispanic')
white_grooming = pod(grooming, "race", 'White')

black_waiting = pod(waiting, "race", 'African')
asian_waiting = pod(waiting, "race", 'Asian')
hispanic_waiting = pod(waiting, "race", 'Hispanic')
white_waiting = pod(waiting, "race", 'White')

black_sleep = pod(sleep, "race", 'African')
asian_sleep = pod(sleep, "race", 'Asian')
hispanic_sleep = pod(sleep, "race", 'Hispanic')
white_sleep = pod(sleep, "race", 'White')

black_music = pod(music, "race", 'African')
asian_music = pod(music, "race", 'Asian')
hispanic_music = pod(music, "race", 'Hispanic')
white_music = pod(music, "race", 'White')

black_telephone = pod(telephone, "race", 'African')
asian_telephone = pod(telephone, "race", 'Asian')
hispanic_telephone = pod(telephone, "race", 'Hispanic')
white_telephone = pod(telephone, "race", 'White')

# Compare gender groups --------------------------------------------------------

men_sports = pod(sports, "gender", 'Men')
women_sports = pod(sports, "gender", 'Women')

men_exam = pod(exam, "gender", 'Men')
women_exam = pod(exam, "gender", 'Women')

men_preparing_food = pod(preparing_food, "gender", 'Men')
women_preparing_food = pod(preparing_food, "gender", 'Women')

men_eating = pod(eating, "gender", 'Men')
women_eating = pod(eating, "gender", 'Women')

men_drinking = pod(drinking, "gender", 'Men')
women_drinking = pod(drinking, "gender", 'Women')

men_communicating = pod(communicating, "gender", 'Men')
women_communicating = pod(communicating, "gender", 'Women')

men_movies = pod(movies, "gender", 'Men')
women_movies = pod(movies, "gender", 'Women')

men_commuting = pod(commuting, "gender", 'Men')
women_commuting = pod(commuting, "gender", 'Women')

men_online = pod(online, "gender", 'Men')
women_online = pod(online, "gender", 'Women')

men_videogames = pod(videogames, "gender", 'Men')
women_videogames = pod(videogames, "gender", 'Women')

men_reading = pod(reading, "gender", 'Men')
women_reading = pod(reading, "gender", 'Women')

men_working = pod(working, "gender", 'Men')
women_working = pod(working, "gender", 'Women')

men_shopping = pod(shopping, "gender", 'Men')
women_shopping = pod(shopping, "gender", 'Women')

men_grooming = pod(grooming, "gender", 'Men')
women_grooming = pod(grooming, "gender", 'Women')

men_waiting = pod(waiting, "gender", 'Men')
women_waiting = pod(waiting, "gender", 'Women')

men_sleep = pod(sleep, "gender", 'Men')
women_sleep = pod(sleep, "gender", 'Women')

men_music = pod(music, "gender", 'Men')
women_music = pod(music, "gender", 'Women')

men_telephone = pod(telephone, "gender", 'Men')
women_telephone = pod(telephone, "gender", 'Women')

# Save as .RData ---------------------------------------------------------------

save.image("pod.RData")
