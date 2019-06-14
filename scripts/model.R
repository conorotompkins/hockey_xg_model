library(tidyverse)
library(tidymodels)
library(janitor)
library(gganimate)

theme_set(theme_bw())

df <- read_csv("data/Shot Prediction Data for Comparison.csv") %>% 
  clean_names()

df %>% 
  count(game_date) %>% 
  ggplot(aes(game_date, n)) +
  geom_point()

df <- df %>% 
  #mutate(goal = as.numeric(goal)) %>% 
  select(game_date, goal, gametime, event_type, shot_type, 
         coords_x, coords_y, home_skaters, away_skaters, 
         game_score_state, game_strength_state, event_distance, event_angle) %>% 
  mutate_if(is.character, factor) %>% 
  select(game_date, goal, coords_x, coords_y, event_distance, event_angle, shot_type)

df_analysis <- df %>% 
  filter(game_date <= "2017-07-01")

df_assessment <- df %>% 
  filter(game_date > "2017-07-01")

df_analysis %>% 
  mutate(goal = as.numeric(goal)) %>% 
  ggplot(aes(event_distance, goal, color = shot_type, fill = shot_type)) +
  #geom_point(alpha = .1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_y_continuous(limits = c(0, 1))

df_analysis %>% 
  mutate(goal = as.numeric(goal)) %>% 
  ggplot(aes(event_angle, goal, color = shot_type, fill = shot_type)) +
  #geom_point(alpha = .1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_y_continuous(limits = c(0, 1))

df_analysis <- df_analysis %>% 
  select(-game_date)

model <- glm(goal ~ event_distance + event_angle + shot_type, data = df_analysis, family = "binomial")

model %>% 
  tidy() %>% 
  mutate(term = fct_reorder(term, estimate)) %>% 
  ggplot(aes(term, estimate)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  coord_flip()

model %>% 
  glance()

df_analysis_aug <- augment(model, df_analysis, type.predict = "response")

df_analysis_aug %>% 
  ggplot(aes(.resid)) +
  geom_density()

df_analysis_aug %>% 
  ggplot(aes(.fitted)) +
  geom_density()

df_xg_map <- df_analysis_aug %>% 
  group_by(coords_x, coords_y) %>% 
  summarize(xg = mean(.fitted),
            g = mean(goal))

df_xg_map %>% 
  ggplot(aes(coords_x, coords_y, fill = xg)) +
  geom_raster() +
  scale_fill_viridis_c()

df_xg_map %>% 
  ggplot(aes(coords_x, coords_y, fill = g)) +
  geom_raster() +
  scale_fill_viridis_c()

df_xg_map %>% 
  mutate(xg_diff = g - xg) %>% 
  ggplot(aes(coords_x, coords_y, fill = xg_diff)) +
  geom_raster() +
  scale_fill_viridis_c()

df_xg_map %>% 
  select(coords_x, coords_y, g, xg) %>% 
  gather(metric, measure, -c(coords_x, coords_y)) %>% 
  group_by(coords_x, coords_y, metric) %>% 
  summarize(percent = mean(measure)) %>% 
  ggplot(aes(coords_x, coords_y, fill = percent)) +
  geom_raster() +
  facet_wrap(~metric,
             ncol = 1) +
  scale_fill_viridis_c()

df_xg_map %>% 
  ggplot(aes(g, xg)) +
  geom_point(alpha = .1) +
  geom_smooth()

df_analysis_aug %>% 
  mutate(goal = as.numeric(goal)) %>% 
  metrics(goal, .fitted)

df_analysis_aug %>% 
  mutate(goal = as.factor(goal)) %>%
  roc_auc(goal, .fitted) 

df_assessment_aug <- augment(model, newdata = df_assessment, type.predict = "response")

df_assessment_aug %>% 
  mutate(goal = as.numeric(goal)) %>% 
  metrics(goal, .fitted)

df_assessment_aug %>% 
  mutate(goal = as.factor(goal)) %>%
  roc_auc(goal, .fitted) 

#df_assessment_aug %>% 
#  mutate(goal = as.factor(goal)) %>%
#  roc_curve(goal, .fitted)
