
## Anonymous
# Probability of Differentiation

## Script date: 4 May 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("ggplot2")){install.packages("ggplot2", dependencies = TRUE); require("ggplot2")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}
if(!require("Cairo")){install.packages("Cairo", dependencies = TRUE); require("Cairo")} # Install XQuartz 

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
  facet_wrap(~ activity, ncol = 4) +
  labs(x = "Activities", 
       y = "Probability of Differentiation", 
       color = "Race/Ethnicity") + 
  scale_color_jama()

ggsave("../Plots/pod_race_pilot.pdf", width = 9,  height = 6, 
       dpi = "retina", device = cairo_pdf)

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
  facet_wrap(~ activity, ncol = 4) +
  labs(x = "Activities", 
       y = "Probability of Differentiation", 
       color = "Gender") + 
  scale_color_jama()

ggsave("../Plots/pod_gender_pilot.pdf", width = 9,  height = 6, 
       dpi = "retina", device = cairo_pdf)

