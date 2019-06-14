model <- glm(goal ~ event_distance + event_angle + shot_type, data = df_analysis, family = "binomial")

df_aug <- augment(model, df_analysis, type.predict = "response")

test <- df_aug[1:500,] %>% 
  select(goal, .fitted) %>% 
  mutate(goal = as.factor(goal))

test %>% 
  summary()

df_aug %>% 
  mutate(goal = as.factor(goal)) %>% 
  yardstick::roc_auc(goal, .fitted)

df_aug %>% 
  select(goal, .fitted) %>% 
  mutate(goal = as.factor(goal)) %>% 
  yardstick::roc_curve(goal, .fitted)

pROC::roc(goal ~ .fitted, data = df_aug)

pROC::roc(goal ~ .fitted, data = df_aug) %>% 
  plot()

dput(test)