
## Anonymous
# Probability of Differentiation

## Script date: 3 May 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("ggplot2")){install.packages("ggplot2", dependencies = TRUE); require("ggplot2")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}
if(!require("Cairo")){install.packages("Cairo", dependencies = TRUE); require("Cairo")} # Install XQuartz 

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

pod_race_df = data.frame(race = races, activity = activities, pod = pods) %>%
  mutate(race = factor(race, levels = race_list)) %>% 
  mutate(activity = factor(activity, levels = activity_list))

pod_race_summary <- pod_race_df %>%
  group_by(race, activity) %>%
  summarise(mean = quantile(pod, 0.5),
            lower = quantile(pod, 0.025),
            upper = quantile(pod, 0.975))

# Race Barplot -----------------------------------------------------------------

ggplot(pod_race_summary, aes(x = race, y = mean, color = race)) +
  geom_point(stat = "identity", position = position_dodge(width = 0.75), size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5) +
  theme_bw() + 
  theme(legend.position = "top",
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12), 
        plot.margin = margin(t = 0, l = 5, r = 5)) + 
  facet_wrap(~ activity, ncol = 6) +
  labs(x = "Activities", 
       y = "Probability of Differentiation", 
       color = "Race/Ethnicity") + 
  scale_color_jama()

ggsave("../Plots/pod_race_individual_prompt.pdf", width = 9,  height = 6, 
       dpi = "retina", device = cairo_pdf)

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

pod_gender_df = data.frame(gender = genders, activity = activities, pod = pods) %>%
  mutate(gender = factor(gender, levels = gender_list)) %>% 
  mutate(activity = factor(activity, levels = activity_list))

pod_gender_summary <- pod_gender_df %>%
  group_by(gender, activity) %>%
  summarise(mean = quantile(pod, 0.5),
            lower = quantile(pod, 0.025),
            upper = quantile(pod, 0.975))

# Gender Barplot ---------------------------------------------------------------

ggplot(pod_gender_summary, aes(x = gender, y = mean, color = gender)) +
  geom_point(stat = "identity", position = position_dodge(width = 0.75), size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25) +
  theme_bw() + 
  theme(legend.position = "top",
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12), 
        plot.margin = margin(t = 0, l = 5, r = 5)) + 
  facet_wrap(~ activity, ncol = 6) +
  labs(x = "Activities", 
       y = "Probability of Differentiation", 
       color = "Gender") + 
  scale_color_jama()

ggsave("../Plots/pod_gender_individual_prompt.pdf", width = 9,  height = 6, 
       dpi = "retina", device = cairo_pdf)

