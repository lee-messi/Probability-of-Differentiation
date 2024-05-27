
## Anonymous
# Probability of Differentiation

## Script date: 4 May 2024

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

car = read.csv('../Data/car.csv') %>% preprocess(4)
festival = read.csv('../Data/festival.csv') %>% preprocess(3)
food = read.csv('../Data/food.csv') %>% preprocess(4)
hobby = read.csv('../Data/hobby.csv') %>% preprocess(4)
job = read.csv('../Data/job.csv') %>% preprocess(5)
major = read.csv('../Data/major.csv') %>% preprocess(3)
music = read.csv('../Data/music.csv') %>% preprocess(3)
state = read.csv('../Data/state.csv') %>% preprocess(2)

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

black_car = pod(car, "race", 'black')
asian_car = pod(car, "race", 'asian')
hispanic_car = pod(car, "race", 'hispanic')
white_car = pod(car, "race", 'white')

black_festival = pod(festival, "race", 'black')
asian_festival = pod(festival, "race", 'asian')
hispanic_festival = pod(festival, "race", 'hispanic')
white_festival = pod(festival, "race", 'white')

black_food = pod(food, "race", 'black')
asian_food = pod(food, "race", 'asian')
hispanic_food = pod(food, "race", 'hispanic')
white_food = pod(food, "race", 'white')

black_hobby = pod(hobby, "race", 'black')
asian_hobby = pod(hobby, "race", 'asian')
hispanic_hobby = pod(hobby, "race", 'hispanic')
white_hobby = pod(hobby, "race", 'white')

black_job = pod(job, "race", 'black')
asian_job = pod(job, "race", 'asian')
hispanic_job = pod(job, "race", 'hispanic')
white_job = pod(job, "race", 'white')

black_major = pod(major, "race", 'black')
asian_major = pod(major, "race", 'asian')
hispanic_major = pod(major, "race", 'hispanic')
white_major = pod(major, "race", 'white')

black_music = pod(music, "race", 'black')
asian_music = pod(music, "race", 'asian')
hispanic_music = pod(music, "race", 'hispanic')
white_music = pod(music, "race", 'white')

black_state = pod(state, "race", 'black')
asian_state = pod(state, "race", 'asian')
hispanic_state = pod(state, "race", 'hispanic')
white_state = pod(state, "race", 'white')

# Compare gender groups --------------------------------------------------------

men_car = pod(car, "gender", 'Men')
women_car = pod(car, "gender", 'Women')

men_festival = pod(festival, "gender", 'Men')
women_festival = pod(festival, "gender", 'Women')

men_food = pod(food, "gender", 'Men')
women_food = pod(food, "gender", 'Women')

men_hobby = pod(hobby, "gender", 'Men')
women_hobby = pod(hobby, "gender", 'Women')

men_job = pod(job, "gender", 'Men')
women_job = pod(job, "gender", 'Women')

men_major = pod(major, "gender", 'Men')
women_major = pod(major, "gender", 'Women')

men_music = pod(music, "gender", 'Men')
women_music = pod(music, "gender", 'Women')

men_state = pod(state, "gender", 'Men')
women_state = pod(state, "gender", 'Women')

# Save as .RData ---------------------------------------------------------------

save.image("pod.RData")
