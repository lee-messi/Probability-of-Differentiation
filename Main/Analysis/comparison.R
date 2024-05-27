
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
             gender == 'M' ~ 'Men',
             gender == 'F' ~ 'Women',
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
    
    name_samples <- sample(unique(filtered_data$name), 
                           size = length(unique(filtered_data$name)), replace = TRUE)
    
    
    # For each name, sample rows with replacement
    sampled_data <- do.call(rbind, lapply(name_samples, function(x) {
      name_data <- filtered_data %>% filter(name == x)
    }))
    
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

black_sports = pod(sports, "race", 'black')
asian_sports = pod(sports, "race", 'asian')
hispanic_sports = pod(sports, "race", 'hispanic')
white_sports = pod(sports, "race", 'white')

black_exams = pod(exam, "race", 'black')
asian_exams = pod(exam, "race", 'asian')
hispanic_exams = pod(exam, "race", 'hispanic')
white_exams = pod(exam, "race", 'white')

black_preparing_food = pod(preparing_food, "race", 'black')
asian_preparing_food = pod(preparing_food, "race", 'asian')
hispanic_preparing_food = pod(preparing_food, "race", 'hispanic')
white_preparing_food = pod(preparing_food, "race", 'white')

black_eating = pod(eating, "race", 'black')
asian_eating = pod(eating, "race", 'asian')
hispanic_eating = pod(eating, "race", 'hispanic')
white_eating = pod(eating, "race", 'white')

black_drinking = pod(drinking, "race", 'black')
asian_drinking = pod(drinking, "race", 'asian')
hispanic_drinking = pod(drinking, "race", 'hispanic')
white_drinking = pod(drinking, "race", 'white')

black_communicating = pod(communicating, "race", 'black')
asian_communicating = pod(communicating, "race", 'asian')
hispanic_communicating = pod(communicating, "race", 'hispanic')
white_communicating = pod(communicating, "race", 'white')

black_movies = pod(movies, "race", 'black')
asian_movies = pod(movies, "race", 'asian')
hispanic_movies = pod(movies, "race", 'hispanic')
white_movies = pod(movies, "race", 'white')

black_commuting = pod(commuting, "race", 'black')
asian_commuting = pod(commuting, "race", 'asian')
hispanic_commuting = pod(commuting, "race", 'hispanic')
white_commuting = pod(commuting, "race", 'white')

black_online = pod(online, "race", 'black')
asian_online = pod(online, "race", 'asian')
hispanic_online = pod(online, "race", 'hispanic')
white_online = pod(online, "race", 'white')

black_videogames = pod(videogames, "race", 'black')
asian_videogames = pod(videogames, "race", 'asian')
hispanic_videogames = pod(videogames, "race", 'hispanic')
white_videogames = pod(videogames, "race", 'white')

black_reading = pod(reading, "race", 'black')
asian_reading = pod(reading, "race", 'asian')
hispanic_reading = pod(reading, "race", 'hispanic')
white_reading = pod(reading, "race", 'white')

black_working = pod(working, "race", 'black')
asian_working = pod(working, "race", 'asian')
hispanic_working = pod(working, "race", 'hispanic')
white_working = pod(working, "race", 'white')

black_shopping = pod(shopping, "race", 'black')
asian_shopping = pod(shopping, "race", 'asian')
hispanic_shopping = pod(shopping, "race", 'hispanic')
white_shopping = pod(shopping, "race", 'white')

black_grooming = pod(grooming, "race", 'black')
asian_grooming = pod(grooming, "race", 'asian')
hispanic_grooming = pod(grooming, "race", 'hispanic')
white_grooming = pod(grooming, "race", 'white')

black_waiting = pod(waiting, "race", 'black')
asian_waiting = pod(waiting, "race", 'asian')
hispanic_waiting = pod(waiting, "race", 'hispanic')
white_waiting = pod(waiting, "race", 'white')

black_sleep = pod(sleep, "race", 'black')
asian_sleep = pod(sleep, "race", 'asian')
hispanic_sleep = pod(sleep, "race", 'hispanic')
white_sleep = pod(sleep, "race", 'white')

black_music = pod(music, "race", 'black')
asian_music = pod(music, "race", 'asian')
hispanic_music = pod(music, "race", 'hispanic')
white_music = pod(music, "race", 'white')

black_telephone = pod(telephone, "race", 'black')
asian_telephone = pod(telephone, "race", 'asian')
hispanic_telephone = pod(telephone, "race", 'hispanic')
white_telephone = pod(telephone, "race", 'white')

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
