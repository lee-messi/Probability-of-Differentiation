
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

ggsave("../Plots/pod_race_general_prompts.pdf", width = 9,  height = 6, 
       dpi = "retina", device = cairo_pdf)

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

ggsave("../Plots/pod_gender_general_prompts.pdf", width = 9,  height = 6, 
       dpi = "retina", device = cairo_pdf)

