
# load the libraries ------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(palmerpenguins)
library(cowplot)

# on penguins dataframe as trial 
glimpse(penguins)

penguins %>% 
  view


# explore the data
penguins %>% 
  ggplot(aes(bill_length_mm, flipper_length_mm, color = sex)) +
  geom_point(size = 2) +
  facet_wrap(~species, scales = 'free_x') +
  theme_half_open(15)


## clean the data ####

penguins_df = penguins %>% 
  drop_na() %>% 
  select(-island, -year)


# split the data
set.seed(123)
penguin_split = initial_split(penguins_df, prop = 0.8, strata = sex)
penguin_train = training(penguin_split)
penguin_test = testing(penguin_split)


# models ---------------------------


# Support Vector Machine
svm_spec = svm_linear() %>% 
  set_mode('classification') %>% 
  set_engine('kernlab')

# logistic regression
glm_spec = logistic_reg() %>% 
  set_engine('glm')

# random forest
rf_spec = rand_forest() %>% 
  set_mode("classification") %>% 
  set_engine('ranger')


# set-up the bootstraps for the data
penguin_boot = bootstraps(penguin_train)
penguin_boot

# data workflow 
penguin_wf = workflow() %>%
  add_formula(sex ~ .)

penguin_wf


# train models

## SVM
svm_rs = penguin_wf %>%
  add_model(svm_spec) %>%
  fit_resamples(
    resamples = penguin_boot,
    control = control_resamples(save_pred = TRUE)
  )

svm_rs

## GLM
glm_rs = penguin_wf %>%
  add_model(glm_spec) %>%
  fit_resamples(
    resamples = penguin_boot,
    control = control_resamples(save_pred = TRUE)
  )

glm_rs

## Random Forest
rf_rs = penguin_wf %>%
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = penguin_boot,
    control = control_resamples(save_pred = TRUE)
  )

rf_rs




# evaluate the models -----------------------------------------------------

# collect the metrics
collect_metrics(rf_rs)
collect_metrics(glm_rs)
collect_metrics(svm_rs)

rf_rs %>%
  conf_mat_resampled()

glm_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(sex, .pred_female) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal() +
  theme_cowplot(15)





# TESTING SET -------------------------------------------------------------

penguin_final = penguin_wf %>%
  add_model(glm_spec) %>%
  last_fit(penguin_split)

penguin_final

collect_metrics(penguin_final)

# confusion matrix

collect_predictions(penguin_final) %>%
  conf_mat(sex, .pred_class)


# predicting inputs
glm_wf = penguin_wf %>% 
  add_model(glm_spec) %>% 
  fit(penguin_train)

predict(glm_wf, penguin_test)
