
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

start = read.csv('../Data/start.csv') %>% preprocess(4)
finish = read.csv('../Data/finish.csv') %>% preprocess(4)
like = read.csv('../Data/like.csv') %>% preprocess(4)
avoid = read.csv('../Data/avoid.csv') %>% preprocess(4)
continue = read.csv('../Data/continue.csv') %>% preprocess(4)
remember = read.csv('../Data/remember.csv') %>% preprocess(4)
regularly = read.csv('../Data/regularly.csv') %>% preprocess(4)
end = read.csv('../Data/end.csv') %>% preprocess(4)
plan = read.csv('../Data/plan.csv') %>% preprocess(4)
hope = read.csv('../Data/hope.csv') %>% preprocess(4)
need = read.csv('../Data/need.csv') %>% preprocess(4)
desire = read.csv('../Data/desire.csv') %>% preprocess(4)
determined = read.csv('../Data/determined.csv') %>% preprocess(4)
prepare = read.csv('../Data/prepare.csv') %>% preprocess(4)
try = read.csv('../Data/try.csv') %>% preprocess(4)
decide = read.csv('../Data/decide.csv') %>% preprocess(4)
often = read.csv('../Data/often.csv') %>% preprocess(4)
interest = read.csv('../Data/interest.csv') %>% preprocess(4)

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

black_start = pod(start, "race", 'black')
asian_start = pod(start, "race", 'asian')
hispanic_start = pod(start, "race", 'hispanic')
white_start = pod(start, "race", 'white')

black_finish = pod(finish, "race", 'black')
asian_finish = pod(finish, "race", 'asian')
hispanic_finish = pod(finish, "race", 'hispanic')
white_finish = pod(finish, "race", 'white')

black_like = pod(like, "race", 'black')
asian_like = pod(like, "race", 'asian')
hispanic_like = pod(like, "race", 'hispanic')
white_like = pod(like, "race", 'white')

black_avoid = pod(avoid, "race", 'black')
asian_avoid = pod(avoid, "race", 'asian')
hispanic_avoid = pod(avoid, "race", 'hispanic')
white_avoid = pod(avoid, "race", 'white')

black_continue = pod(continue, "race", 'black')
asian_continue = pod(continue, "race", 'asian')
hispanic_continue = pod(continue, "race", 'hispanic')
white_continue = pod(continue, "race", 'white')

black_remember = pod(remember, "race", 'black')
asian_remember = pod(remember, "race", 'asian')
hispanic_remember = pod(remember, "race", 'hispanic')
white_remember = pod(remember, "race", 'white')

black_regularly = pod(regularly, "race", 'black')
asian_regularly = pod(regularly, "race", 'asian')
hispanic_regularly = pod(regularly, "race", 'hispanic')
white_regularly = pod(regularly, "race", 'white')

black_end = pod(end, "race", 'black')
asian_end = pod(end, "race", 'asian')
hispanic_end = pod(end, "race", 'hispanic')
white_end = pod(end, "race", 'white')

black_plan = pod(plan, "race", 'black')
asian_plan = pod(plan, "race", 'asian')
hispanic_plan = pod(plan, "race", 'hispanic')
white_plan = pod(plan, "race", 'white')

black_hope = pod(hope, "race", 'black')
asian_hope = pod(hope, "race", 'asian')
hispanic_hope = pod(hope, "race", 'hispanic')
white_hope = pod(hope, "race", 'white')

black_need = pod(need, "race", 'black')
asian_need = pod(need, "race", 'asian')
hispanic_need = pod(need, "race", 'hispanic')
white_need = pod(need, "race", 'white')

black_desire = pod(desire, "race", 'black')
asian_desire = pod(desire, "race", 'asian')
hispanic_desire = pod(desire, "race", 'hispanic')
white_desire = pod(desire, "race", 'white')

black_determined = pod(determined, "race", 'black')
asian_determined = pod(determined, "race", 'asian')
hispanic_determined = pod(determined, "race", 'hispanic')
white_determined = pod(determined, "race", 'white')

black_prepare = pod(prepare, "race", 'black')
asian_prepare = pod(prepare, "race", 'asian')
hispanic_prepare = pod(prepare, "race", 'hispanic')
white_prepare = pod(prepare, "race", 'white')

black_try = pod(try, "race", 'black')
asian_try = pod(try, "race", 'asian')
hispanic_try = pod(try, "race", 'hispanic')
white_try = pod(try, "race", 'white')

black_decide = pod(decide, "race", 'black')
asian_decide = pod(decide, "race", 'asian')
hispanic_decide = pod(decide, "race", 'hispanic')
white_decide = pod(decide, "race", 'white')

black_often = pod(often, "race", 'black')
asian_often = pod(often, "race", 'asian')
hispanic_often = pod(often, "race", 'hispanic')
white_often = pod(often, "race", 'white')

black_interest = pod(interest, "race", 'black')
asian_interest = pod(interest, "race", 'asian')
hispanic_interest = pod(interest, "race", 'hispanic')
white_interest = pod(interest, "race", 'white')

# Compare gender groups --------------------------------------------------------

men_start = pod(start, "gender", 'Men')
women_start = pod(start, "gender", 'Women')

men_finish = pod(finish, "gender", 'Men')
women_finish = pod(finish, "gender", 'Women')

men_like = pod(like, "gender", 'Men')
women_like = pod(like, "gender", 'Women')

men_avoid = pod(avoid, "gender", 'Men')
women_avoid = pod(avoid, "gender", 'Women')

men_continue = pod(continue, "gender", 'Men')
women_continue = pod(continue, "gender", 'Women')

men_remember = pod(remember, "gender", 'Men')
women_remember = pod(remember, "gender", 'Women')

men_regularly = pod(regularly, "gender", 'Men')
women_regularly = pod(regularly, "gender", 'Women')

men_end = pod(end, "gender", 'Men')
women_end = pod(end, "gender", 'Women')

men_plan = pod(plan, "gender", 'Men')
women_plan = pod(plan, "gender", 'Women')

men_hope = pod(hope, "gender", 'Men')
women_hope = pod(hope, "gender", 'Women')

men_need = pod(need, "gender", 'Men')
women_need = pod(need, "gender", 'Women')

men_desire = pod(desire, "gender", 'Men')
women_desire = pod(desire, "gender", 'Women')

men_determined = pod(determined, "gender", 'Men')
women_determined = pod(determined, "gender", 'Women')

men_prepare = pod(prepare, "gender", 'Men')
women_prepare = pod(prepare, "gender", 'Women')

men_try = pod(try, "gender", 'Men')
women_try = pod(try, "gender", 'Women')

men_decide = pod(decide, "gender", 'Men')
women_decide = pod(decide, "gender", 'Women')

men_often = pod(often, "gender", 'Men')
women_often = pod(often, "gender", 'Women')

men_interest = pod(interest, "gender", 'Men')
women_interest = pod(interest, "gender", 'Women')

# Save as .RData ---------------------------------------------------------------

save.image("pod.RData")
