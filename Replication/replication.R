
## Anonymous
# Probability of Differentiation

## Script date: 23 May 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("effsize")){install.packages("effsize", dependencies = TRUE); require("effsize")}
if(!require("meta")){install.packages("meta", dependencies = TRUE); require("meta")}

# Load data --------------------------------------------------------------------

load("main.RData")
load("model.RData")
load("label.RData")
load("individual_prompt.RData")

# Define functions to transform t into r ---------------------------------------

# Function to convert t-statistic to correlation coefficient
t_to_r <- function(t, df) {
  r <- sqrt(t^2 / (t^2 + df))
  sign(t) * r
}

# Function to calculate the standard error of r
se_r <- function(r, df) {
  sqrt((1 - r^2)^2 / df)
}

transform.df <- function(df){
  df = df %>% mutate(r = t_to_r(t, 1998),
                     se = se_r(r, 1998),
                     ci_lower = r - 1.96 * se,
                     ci_upper = r + 1.96 * se) %>%
    select(c("comparison", "activity", "r", "ci_lower", "ci_upper"))
  return(df)
}

# Modify dataframes ------------------------------------------------------------

main.race.r <- transform.df(main.race.df)
model.race.r <- transform.df(model.race.df)
label.race.r <- transform.df(label.race.df)
individual.race.r <- transform.df(individual.race.df)

main.gender.r <- transform.df(main.gender.df)
model.gender.r <- transform.df(model.gender.df)
label.gender.r <- transform.df(label.gender.df)
individual.gender.r <- transform.df(individual.gender.df)

main.r <- rbind(main.race.r, main.gender.r)
model.r <- rbind(model.race.r, model.gender.r)
label.r <- rbind(label.race.r, label.gender.r)
individual.r <- rbind(individual.race.r, individual.gender.r)

# Function to calculate coverage -----------------------------------------------

calculate_fisher_coverage <- function(main, replication) {
  
  # Because both studies have equal sample size, the expected probability is 83.4%
  # Refer to Appendix A4 of "Estimating the reproducibility of psychological science": https://osf.io/z7aux
  expected = 0.83
  condition <- ifelse(main$r > 0,
                      (main$r > replication$ci_lower) & (main$r < replication$ci_upper),
                      (main$r < replication$ci_upper) & (main$r > replication$ci_lower))
  observed <- sum(condition)
  
  print(binom.test(observed, nrow(main), 0.83, "two.sided", 0.95))
  
}

# Goodness-of-fit test for Model ablation study --------------------------------

calculate_fisher_coverage(main.r, model.r)

# Goodness-of-fit test for Group Label ablation study --------------------------

calculate_fisher_coverage(main.r, label.r)

# Goodness-of-fit test for Individual Prompt ablation study --------------------

calculate_fisher_coverage(main.r, individual.r)

