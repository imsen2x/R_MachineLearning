# Goal: Check whether a standardized feature-set performs better than the unfiltered one
library(tidymodels)
library(tidyverse)
library(skimr)
library(vip)

red_wine <- read.csv("winequality-red.csv")
View(red_wine)

skim(red_wine)

# Categorize the wine quality
red_wine %>%
  mutate(quality_description = case_when(
    quality < 5 ~ "Poor",
    quality >= 5 & quality < 8 ~ "Average",
    quality >= 8 ~ "Great"
  ),
  quality_description = factor(quality_description,
                                  levels = c("Poor", "Average", "Great"))) %>%
ggplot(aes(quality, fill = quality_description)) +
  geom_histogram() +
  scale_fill_manual(values = c("tomato", "cornflowerblue", "springgreen2"))

red_wine_engineered <- red_wine %>%
  mutate(quality_description = case_when(
    quality < 5 ~ "Poor",
    quality >= 5 & quality < 8 ~ "Average",
    quality >= 8 ~ "Great"
  ),
  quality_description = factor(quality_description,
                               levels = c("Poor", "Average", "Great")))
  
# Average quality of red_wines are the most common
red_wine_engineered %>%
  count(quality_description) %>%
  mutate(pct = 100.0 * n / sum(n))


set.seed(1111)
# Stratify by quality description
data_split <- initial_split(red_wine_engineered, strata = quality_description)
red_wine_training <- training(data_split)
red_wine_testing <- testing(data_split)

# Normalize the dataset
red_wine_prepped <- recipe(quality ~ 
                             fixed.acidity + volatile.acidity + citric.acid +
                             residual.sugar + chlorides + free.sulfur.dioxide +
                             total.sulfur.dioxide + density + pH + sulphates +
                             alcohol,
                           data = red_wine_training) %>%
  step_normalize(all_numeric()) %>%
  step_zv(all_numeric()) %>%
  prep(retain = T)



rf_spec <- rand_forest(
  min_n = tune(),
  trees = 1000,
  mtry = tune()
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

set.seed(2222)
# Create the bootstrap samples
red_wine_boot <- bootstraps(red_wine_training, times = 15, strata = quality_description) 

# Initial tuning
set.seed(3333)
tune_rf1 <- tune_grid(
  rf_spec,
  red_wine_prepped,
  resamples = red_wine_boot,
  grid = 15,
  metrics = metric_set(rmse, rsq),
  control = control_grid(save_pred = T, verbose = T)
)

# Sanity Check
best_model_exp <- finalize_model(
  rf_spec,
  select_best(tune_rf1, "rsq")
)

# Shows which variable are the most important in our model
best_model_exp %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(quality ~ .,
      data = juice(red_wine_prepped)) %>%
  vip(geom = "point")


# A function that shows the r-squared performance of the tuning parameters
# mtry and min_n on 
rf_tuning_graph <- function(x){
  x %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = F) +
  facet_wrap(.~parameter, scales = "free_x")
}

rf_tuning_graph(tune_rf1)

# Check the tuning statistics
collect_metrics(tune_rf1)
# It seem that the average r-squared for each bootstrap and grid is about ~ 0.4
# min_n: 10:15
# mtry: 6:8


tune_rf1 %>%
  select_best("rsq") 
tune_rf1 %>%
  select_best("rmse")

# Retune the parameters using the information we got from the initial tuning
rf_grid1 <- grid_regular(
  min_n(range = c(10, 15)),
  mtry(range = c(6, 8)),
  levels = 15
)

set.seed(1111)
tune_rf2 <- tune_grid(
  rf_spec,
  red_wine_prepped,
  resamples = red_wine_boot,
  grid = rf_grid1,
  metrics = metric_set(rmse, rsq),
  control = control_grid(save_pred = T, verbose = TRUE)
)

collect_metrics(tune_rf2) 
select_best(tune_rf2, "rsq")
select_best(tune_rf2, "rmse")
# The best tuning configuration is:
# min_n: 6
# mtry: 10

### Test Set Modelling ###
best_model1 <- finalize_model(
  rf_spec,
  select_best(tune_rf2, "rsq")
)

best_model1 %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(quality ~ .,
      data = juice(red_wine_prepped)) %>%
  vip(geom = "point")

# Alcohol and Sulphates are the most important features

rf_final_spec1 <- rand_forest(
  min_n = 6,
  trees = 1000,
  mtry = 10
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

set.seed(1111)
rf_final_model1 <- rf_final_spec1 %>%
  fit(quality ~ .,
       data = juice(red_wine_prepped))

rf_final_model1

predicted_vals1 <- predict(rf_final_model1,
        new_data = bake(red_wine_prepped, new_data = red_wine_testing))

original_quality <- bake(red_wine_prepped, new_data = red_wine_testing) %>%
  select(quality)


predicted_vals1
rmse1 <- rmse(bind_cols(original_quality,
                        predicted_vals1), "quality", ".pred", na.rm = T)
rsq1 <- rsq(bind_cols(original_quality,
                      predicted_vals1), "quality", ".pred", na.rm = T)

rmse1
rsq1

juice(red_wine_prepped) %>% dim

n = 1200
p = 12
adjusted_rsq1 <- (1 - ((1 - rsq1$.estimate) * (n - 1) / (n - p - 1)))
adjusted_rsq1

### Part2 non-standardized Random Forest
rf_spec2 <- rand_forest(
  min_n = tune(),
  trees = 1000,
  mtry = tune()
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

red_wine_prepped2 <- recipe(quality ~ 
                              fixed.acidity + volatile.acidity + citric.acid +
                              residual.sugar + chlorides + free.sulfur.dioxide +
                              total.sulfur.dioxide + density + pH + sulphates +
                              alcohol,
                            data = red_wine_training) %>%
  prep(retain = T)

# Initial tuning
set.seed(3333)
tune_rf2 <- tune_grid(
  rf_spec2,
  red_wine_prepped2,
  resamples = red_wine_boot,
  grid = 15,
  metrics = metric_set(rmse, rsq),
  control = control_grid(save_pred = T, verbose = T)
)

collect_metrics(tune_rf1) %>%
  group_by(.metric) %>%
  summarize(average_metric = mean(mean)) 
collect_metrics(tune_rf2) %>%
  group_by(.metric) %>%
  summarize(average_metric = mean(mean))

# Sanity Check
best_model_exp2 <- finalize_model(
  rf_spec2,
  select_best(tune_rf2, "rsq")
)

best_model_exp2 %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(quality ~ .,
      data = juice(red_wine_prepped2)) %>%
  vip(geom = "point")


rf_tuning_graph(tune_rf1)
rf_tuning_graph(tune_rf2)

# Check the tuning statistics
collect_metrics(tune_rf2)
# It seem that the average r-squared for each bootstrap and grid is about ~ 0.4
# min_n: 10:15
# mtry: 4:8

tune_rf2 %>%
  select_best("rsq") 
tune_rf2 %>%
  select_best("rmse")

# Retune the parameters using the information we got from the initial tuning
rf_grid2 <- grid_regular(
  min_n(range = c(10, 15)),
  mtry(range = c(4, 8)),
  levels = 15
)

set.seed(1111)
tune_rf3 <- tune_grid(
  rf_spec2,
  red_wine_prepped2,
  resamples = red_wine_boot,
  grid = rf_grid2,
  metrics = metric_set(rmse, rsq),
  control = control_grid(save_pred = T, verbose = TRUE)
)

collect_metrics(tune_rf3) 
select_best(tune_rf3, "rsq")
select_best(tune_rf3, "rmse")

collect_metrics(tune_rf3) %>%
  group_by(.metric) %>%
  summarize(average_metric = mean(mean))


# The best tuning configuration is:
# min_n: 4
# mtry: 10

### Test Set Modelling ###
best_model2 <- finalize_model(
  rf_spec2,
  select_best(tune_rf3, "rsq")
)

best_model2 %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(quality ~ .,
      data = juice(red_wine_prepped2)) %>%
  vip(geom = "point")

# Alcohol and Sulphates are the most important features

rf_final_spec2 <- rand_forest(
  min_n = 4,
  trees = 1000,
  mtry = 10
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

set.seed(1111)
rf_final_model2 <- rf_final_spec2 %>%
  fit(quality ~ .,
      data = juice(red_wine_prepped2))


rf_final_model2

predicted_vals2 <- predict(rf_final_model2,
                           new_data = bake(red_wine_prepped2, new_data = red_wine_testing))

original_quality2 <- bake(red_wine_prepped2, new_data = red_wine_testing) %>%
  select(quality)


predicted_vals2
rmse2 <- rmse(bind_cols(original_quality2,
                        predicted_vals2), "quality", ".pred", na.rm = T)
rsq2 <- rsq(bind_cols(original_quality2,
                      predicted_vals2), "quality", ".pred", na.rm = T)

rmse2
rsq2

juice(red_wine_prepped2) %>% dim

n = 1200
p = 12
adjusted_rsq2 <- (1 - ((1 - rsq2$.estimate) * (n - 1) / (n - p - 1)))
adjusted_rsq2



rf_final_spec3 <- rand_forest(
  min_n = 4,
  trees = 1000,
  mtry = 4
) %>%
  set_engine("ranger") %>%
  set_mode("regression")


### Takeaways

### It seems that standardizing a dataset before fitting it into a Linear Model
### doesn't improve the model.In fact based on this test it got worse-since we got a higher
### RMSE value and the predictions lost its raw value. This proves that tree-based models
### are shielded to magnitude effects and it might still be feature selection that will drive
### better models.