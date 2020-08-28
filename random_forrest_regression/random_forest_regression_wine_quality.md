Random Forest Wine Quality
================
Jansen Lopez
8/28/2020

## Random Forest pre-processing experiment

### Goal: Compare if a standardizing the input features will result into better model performance

# <br/>

### \* The results might vary due to randomness

### Load the dataset and the required libraries

``` r
library(tidymodels)
```

    ## -- Attaching packages ---------------------------------------------------- tidymodels 0.1.1 --

    ## v broom     0.7.0      v recipes   0.1.13
    ## v dials     0.0.8      v rsample   0.0.7 
    ## v dplyr     1.0.0      v tibble    3.0.3 
    ## v ggplot2   3.3.2      v tidyr     1.1.0 
    ## v infer     0.5.3      v tune      0.1.1 
    ## v modeldata 0.0.2      v workflows 0.1.2 
    ## v parsnip   0.1.2      v yardstick 0.0.7 
    ## v purrr     0.3.4

    ## -- Conflicts ------------------------------------------------------- tidymodels_conflicts() --
    ## x purrr::discard() masks scales::discard()
    ## x dplyr::filter()  masks stats::filter()
    ## x dplyr::lag()     masks stats::lag()
    ## x recipes::step()  masks stats::step()

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------- tidyverse 1.3.0 --

    ## v readr   1.3.1     v forcats 0.5.0
    ## v stringr 1.4.0

    ## -- Conflicts -------------------------------------------------------- tidyverse_conflicts() --
    ## x readr::col_factor() masks scales::col_factor()
    ## x purrr::discard()    masks scales::discard()
    ## x dplyr::filter()     masks stats::filter()
    ## x stringr::fixed()    masks recipes::fixed()
    ## x dplyr::lag()        masks stats::lag()
    ## x readr::spec()       masks yardstick::spec()

``` r
library(skimr)
library(vip)
```

    ## 
    ## Attaching package: 'vip'

    ## The following object is masked from 'package:utils':
    ## 
    ##     vi

``` r
red_wine <- read.csv("winequality-red.csv")
View(red_wine)

skim(red_wine)
```

|                                                  |           |
| :----------------------------------------------- | :-------- |
| Name                                             | red\_wine |
| Number of rows                                   | 1599      |
| Number of columns                                | 12        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |           |
| Column type frequency:                           |           |
| numeric                                          | 12        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |           |
| Group variables                                  | None      |

Data summary

**Variable type: numeric**

| skim\_variable       | n\_missing | complete\_rate |  mean |    sd |   p0 |   p25 |   p50 |   p75 |   p100 | hist  |
| :------------------- | ---------: | -------------: | ----: | ----: | ---: | ----: | ----: | ----: | -----: | :---- |
| fixed.acidity        |          0 |              1 |  8.32 |  1.74 | 4.60 |  7.10 |  7.90 |  9.20 |  15.90 | ▂▇▂▁▁ |
| volatile.acidity     |          0 |              1 |  0.53 |  0.18 | 0.12 |  0.39 |  0.52 |  0.64 |   1.58 | ▅▇▂▁▁ |
| citric.acid          |          0 |              1 |  0.27 |  0.19 | 0.00 |  0.09 |  0.26 |  0.42 |   1.00 | ▇▆▅▁▁ |
| residual.sugar       |          0 |              1 |  2.54 |  1.41 | 0.90 |  1.90 |  2.20 |  2.60 |  15.50 | ▇▁▁▁▁ |
| chlorides            |          0 |              1 |  0.09 |  0.05 | 0.01 |  0.07 |  0.08 |  0.09 |   0.61 | ▇▁▁▁▁ |
| free.sulfur.dioxide  |          0 |              1 | 15.87 | 10.46 | 1.00 |  7.00 | 14.00 | 21.00 |  72.00 | ▇▅▁▁▁ |
| total.sulfur.dioxide |          0 |              1 | 46.47 | 32.90 | 6.00 | 22.00 | 38.00 | 62.00 | 289.00 | ▇▂▁▁▁ |
| density              |          0 |              1 |  1.00 |  0.00 | 0.99 |  1.00 |  1.00 |  1.00 |   1.00 | ▁▃▇▂▁ |
| pH                   |          0 |              1 |  3.31 |  0.15 | 2.74 |  3.21 |  3.31 |  3.40 |   4.01 | ▁▅▇▂▁ |
| sulphates            |          0 |              1 |  0.66 |  0.17 | 0.33 |  0.55 |  0.62 |  0.73 |   2.00 | ▇▅▁▁▁ |
| alcohol              |          0 |              1 | 10.42 |  1.07 | 8.40 |  9.50 | 10.20 | 11.10 |  14.90 | ▇▇▃▁▁ |
| quality              |          0 |              1 |  5.64 |  0.81 | 3.00 |  5.00 |  6.00 |  6.00 |   8.00 | ▁▇▇▂▁ |

### Check the quality of red wines across the dataset

``` r
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
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](random_forest_regression_wine_quality_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Create a new dataset having the column red\_wine\_engineered

``` r
red_wine_engineered <- red_wine %>%
  mutate(quality_description = case_when(
    quality < 5 ~ "Poor",
    quality >= 5 & quality < 8 ~ "Average",
    quality >= 8 ~ "Great"
  ),
  quality_description = factor(quality_description,
                               levels = c("Poor", "Average", "Great")))
```

### Average quality of red\_wines are the most common

``` r
red_wine_engineered %>%
  count(quality_description) %>%
  mutate(pct = 100.0 * n / sum(n))
```

    ##   quality_description    n       pct
    ## 1                Poor   63  3.939962
    ## 2             Average 1518 94.934334
    ## 3               Great   18  1.125704

### Make a stratified training and testing set, then use recipes to standardize the features

``` r
set.seed(1111)
# Stratify by quality description
data_split <- initial_split(red_wine_engineered, strata = quality_description)
red_wine_training <- training(data_split)
red_wine_testing <- testing(data_split)

# Prep the features by normalizing the dataset
red_wine_prepped <- recipe(quality ~ 
                             fixed.acidity + volatile.acidity + citric.acid +
                             residual.sugar + chlorides + free.sulfur.dioxide +
                             total.sulfur.dioxide + density + pH + sulphates +
                             alcohol,
                           data = red_wine_training) %>%
  step_normalize(all_numeric()) %>%
  step_zv(all_numeric()) %>%
  prep(retain = T)
```

### Use bootstrap samples for checking the overall model performance

``` r
red_wine_boot <- bootstraps(red_wine_training, times = 15, strata = quality_description) 
```

### Set the specifications for the Random Forest. Then explicitly state that min\_n(minimum number of node for a split to continue) and mtry(number of random features used in building each tree) will be tuned. Then start training the models

``` r
# Model specification
rf_spec <- rand_forest(
  min_n = tune(),
  trees = 1000,
  mtry = tune()
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# Initial tuning
set.seed(3333)
tune_rf1 <- tune_grid(
  rf_spec,
  red_wine_prepped,
  resamples = red_wine_boot,
  grid = 15,
  metrics = metric_set(rmse, rsq),
  control = control_grid(save_pred = T, verbose = F)
)
```

    ## i Creating pre-processing data to finalize unknown parameter: mtry

### Quick sanity check. Check the variables that are the most important in our model

``` r
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
```

![](random_forest_regression_wine_quality_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### Create a function that shows the r-squared performance of min\_n and mtry so we can get an idea what’s the range where we can get the best r-squared performance for mtry and min\_n

``` r
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
```

### It seems that the r-squaredes are high for min\_ns 11 - 15 and mtrys between 3 - 8. Let’s disregard mtrys between 1 and 2 since using a small number of features in creating trees tend to overfit models

``` r
# min_n: 11-15
# mtry: 6-8
rf_tuning_graph(tune_rf1)
```

![](random_forest_regression_wine_quality_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Check the best tuning parameters for r-squared and rmse

``` r
tune_rf1 %>%
  select_best("rsq") 
```

    ## # A tibble: 1 x 3
    ##    mtry min_n .config
    ##   <int> <int> <chr>  
    ## 1     2    15 Model11

``` r
tune_rf1 %>%
  select_best("rmse")
```

    ## # A tibble: 1 x 3
    ##    mtry min_n .config
    ##   <int> <int> <chr>  
    ## 1     7     2 Model04

### Focus the retuning based on the initial results: min\_n: 11-15 & mtry: 6-8

``` r
rf_grid1 <- grid_regular(
  min_n(range = c(11, 15)),
  mtry(range = c(6, 8)),
  levels = 5
)

set.seed(1111)
tune_rf2 <- tune_grid(
  rf_spec,
  red_wine_prepped,
  resamples = red_wine_boot,
  grid = rf_grid1,
  metrics = metric_set(rmse, rsq),
  control = control_grid(save_pred = T, verbose = F)
)
```

### The best tuning configuration for the best r-squared are: min\_n: 12 & mtry: 6. The best configuration for min\_n and mtry for r-squared and rmse are the same. These really seems to be the best configuration for our model.

``` r
collect_metrics(tune_rf2) 
```

    ## # A tibble: 30 x 8
    ##     mtry min_n .metric .estimator  mean     n std_err .config
    ##    <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>  
    ##  1     6    11 rmse    standard   0.739    15 0.00972 Model01
    ##  2     6    11 rsq     standard   0.440    15 0.00781 Model01
    ##  3     6    12 rmse    standard   0.739    15 0.00978 Model02
    ##  4     6    12 rsq     standard   0.440    15 0.00773 Model02
    ##  5     6    13 rmse    standard   0.739    15 0.00948 Model03
    ##  6     6    13 rsq     standard   0.440    15 0.00762 Model03
    ##  7     6    14 rmse    standard   0.739    15 0.00947 Model04
    ##  8     6    14 rsq     standard   0.440    15 0.00781 Model04
    ##  9     6    15 rmse    standard   0.740    15 0.00962 Model05
    ## 10     6    15 rsq     standard   0.439    15 0.00783 Model05
    ## # ... with 20 more rows

``` r
select_best(tune_rf2, "rsq")
```

    ## # A tibble: 1 x 3
    ##    mtry min_n .config
    ##   <int> <int> <chr>  
    ## 1     6    12 Model02

``` r
select_best(tune_rf2, "rmse")
```

    ## # A tibble: 1 x 3
    ##    mtry min_n .config
    ##   <int> <int> <chr>  
    ## 1     6    12 Model02

### Check the most important features in our model

### alcohol and sulphates are the most important features

``` r
best_model2 <- finalize_model(
  rf_spec,
  select_best(tune_rf2, "rsq")
)

best_model2 %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(quality ~ .,
      data = juice(red_wine_prepped)) %>%
  vip(geom = "point")
```

![](random_forest_regression_wine_quality_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### Use the best parameters to model the test set

### Train with a Random Forest model using the standardized training set

### rsquared \~ 0.47

``` r
rf_final_spec1 <- rand_forest(
  min_n = 12,
  trees = 1000,
  mtry = 6
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

set.seed(1111)
rf_final_model1 <- rf_final_spec1 %>%
  fit(quality ~ .,
       data = juice(red_wine_prepped))

rf_final_model1
```

    ## parsnip model object
    ## 
    ## Fit time:  931ms 
    ## Ranger result
    ## 
    ## Call:
    ##  ranger::ranger(formula = quality ~ ., data = data, mtry = ~6,      num.trees = ~1000, min.node.size = ~12, num.threads = 1,      verbose = FALSE, seed = sample.int(10^5, 1)) 
    ## 
    ## Type:                             Regression 
    ## Number of trees:                  1000 
    ## Sample size:                      1200 
    ## Number of independent variables:  11 
    ## Mtry:                             6 
    ## Target node size:                 12 
    ## Variable importance mode:         none 
    ## Splitrule:                        variance 
    ## OOB prediction error (MSE):       0.5263239 
    ## R squared (OOB):                  0.4736761

### Get test set performance

``` r
predicted_vals1 <- predict(rf_final_model1,
        new_data = bake(red_wine_prepped, new_data = red_wine_testing))

original_quality <- bake(red_wine_prepped, new_data = red_wine_testing) %>%
  select(quality)


predicted_vals1
```

    ## # A tibble: 399 x 1
    ##      .pred
    ##      <dbl>
    ##  1 -0.505 
    ##  2 -0.858 
    ##  3 -0.505 
    ##  4 -0.474 
    ##  5 -0.399 
    ##  6 -0.338 
    ##  7 -0.779 
    ##  8 -0.507 
    ##  9 -0.108 
    ## 10 -0.0600
    ## # ... with 389 more rows

``` r
rmse1 <- rmse(bind_cols(original_quality,
                        predicted_vals1), "quality", ".pred", na.rm = T)
rsq1 <- rsq(bind_cols(original_quality,
                      predicted_vals1), "quality", ".pred", na.rm = T)

juice(red_wine_prepped) %>% dim
```

    ## [1] 1200   12

``` r
n = 1200
p = 12
adjusted_rsq1 <- (1 - ((1 - rsq1$.estimate) * (n - 1) / (n - p - 1)))
```

### Test Set RMSE

``` r
rmse1
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 rmse    standard       0.732

### Test Set r-squared

``` r
rsq1
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 rsq     standard       0.463

### Test Set adjusted r-squared

``` r
adjusted_rsq1
```

    ## [1] 0.4575906

# Now we have used standardized features in our Random Forest model. Let’s now see if there’s a significant imporvement in the model’s performance

# <br>

### Again set the specifications for the Random Forest. Then explicitly state that min\_n(minimum number of node for a split to continue) and mtry(number of random features used in building each tree) will be tuned. Then prep the dataset but this time we skip the standardization step

``` r
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
```

### Do the initial tuning of the non-standardized Random Forest Model

``` r
set.seed(3333)
tune_rf3 <- tune_grid(
  rf_spec2,
  red_wine_prepped2,
  resamples = red_wine_boot,
  grid = 15,
  metrics = metric_set(rmse, rsq),
  control = control_grid(save_pred = T, verbose = F)
)
```

    ## i Creating pre-processing data to finalize unknown parameter: mtry

### Compare the initial tuning results of the the standardized and non-standardized model

### The r-squared values are roughly equal but the rmse is lower in the non-standardized dataset

``` r
collect_metrics(tune_rf1) %>%
  group_by(.metric) %>%
  summarize(average_metric = mean(mean)) 
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 2
    ##   .metric average_metric
    ##   <chr>            <dbl>
    ## 1 rmse             0.745
    ## 2 rsq              0.434

``` r
collect_metrics(tune_rf3) %>%
  group_by(.metric) %>%
  summarize(average_metric = mean(mean))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 2
    ##   .metric average_metric
    ##   <chr>            <dbl>
    ## 1 rmse             0.604
    ## 2 rsq              0.434

### Another quick sanity check. Check the variables that are the most important in our model

``` r
# Sanity Check
best_model_exp2 <- finalize_model(
  rf_spec2,
  select_best(tune_rf3, "rsq")
)

best_model_exp2 %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(quality ~ .,
      data = juice(red_wine_prepped2)) %>%
  vip(geom = "point")
```

![](random_forest_regression_wine_quality_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

### Compare the initial parameter configuations between the standardized and the non-standardized models

### They look roughly the same so we’ll again choose min\_n: 11-15 and mtry: 6-8

``` r
rf_tuning_graph(tune_rf1)
```

![](random_forest_regression_wine_quality_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
rf_tuning_graph(tune_rf3)
```

![](random_forest_regression_wine_quality_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

### Check the best tuning parameters for r-squared and rmse

``` r
tune_rf1 %>%
  select_best("rsq") 
```

    ## # A tibble: 1 x 3
    ##    mtry min_n .config
    ##   <int> <int> <chr>  
    ## 1     2    15 Model11

``` r
tune_rf1 %>%
  select_best("rmse")
```

    ## # A tibble: 1 x 3
    ##    mtry min_n .config
    ##   <int> <int> <chr>  
    ## 1     7     2 Model04

``` r
tune_rf3 %>%
  select_best("rsq") 
```

    ## # A tibble: 1 x 3
    ##    mtry min_n .config
    ##   <int> <int> <chr>  
    ## 1     2    15 Model11

``` r
tune_rf3 %>%
  select_best("rmse")
```

    ## # A tibble: 1 x 3
    ##    mtry min_n .config
    ##   <int> <int> <chr>  
    ## 1     7     2 Model04

### Focus the retuning based on the initial results: min\_n: 11-15 & mtry: 6-8

``` r
rf_grid2 <- grid_regular(
  min_n(range = c(11, 15)),
  mtry(range = c(6, 8)),
  levels = 15
)

set.seed(1111)
tune_rf4 <- tune_grid(
  rf_spec2,
  red_wine_prepped2,
  resamples = red_wine_boot,
  grid = rf_grid2,
  metrics = metric_set(rmse, rsq),
  control = control_grid(save_pred = T, verbose = F)
)
```

### The best tuning configuration for the best r-squared are: min\_n: 12 & mtry: 6. Same for the standardized model.

``` r
collect_metrics(tune_rf4) 
```

    ## # A tibble: 30 x 8
    ##     mtry min_n .metric .estimator  mean     n std_err .config
    ##    <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>  
    ##  1     6    11 rmse    standard   0.600    15 0.00612 Model01
    ##  2     6    11 rsq     standard   0.440    15 0.00779 Model01
    ##  3     6    12 rmse    standard   0.600    15 0.00613 Model02
    ##  4     6    12 rsq     standard   0.440    15 0.00780 Model02
    ##  5     6    13 rmse    standard   0.600    15 0.00593 Model03
    ##  6     6    13 rsq     standard   0.440    15 0.00764 Model03
    ##  7     6    14 rmse    standard   0.600    15 0.00601 Model04
    ##  8     6    14 rsq     standard   0.440    15 0.00787 Model04
    ##  9     6    15 rmse    standard   0.600    15 0.00600 Model05
    ## 10     6    15 rsq     standard   0.439    15 0.00780 Model05
    ## # ... with 20 more rows

``` r
select_best(tune_rf4, "rsq")
```

    ## # A tibble: 1 x 3
    ##    mtry min_n .config
    ##   <int> <int> <chr>  
    ## 1     6    12 Model02

``` r
select_best(tune_rf4, "rmse")
```

    ## # A tibble: 1 x 3
    ##    mtry min_n .config
    ##   <int> <int> <chr>  
    ## 1     6    12 Model02

### For the best configurations it seems that the r-squared is roughly the same for the standardized model and the non-standardized but the rmse of the non-standardized model is lower

``` r
collect_metrics(tune_rf2) %>%
  group_by(.metric) %>%
  summarize(average_metric = mean(mean))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 2
    ##   .metric average_metric
    ##   <chr>            <dbl>
    ## 1 rmse             0.741
    ## 2 rsq              0.436

``` r
collect_metrics(tune_rf4) %>%
  group_by(.metric) %>%
  summarize(average_metric = mean(mean))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 2
    ##   .metric average_metric
    ##   <chr>            <dbl>
    ## 1 rmse             0.602
    ## 2 rsq              0.436

### Another quick sanity check. Check the variables that are the most important in our model. Again alcohol and sulphates are still the most important features.

``` r
best_model_non_stand <- finalize_model(
  rf_spec2,
  select_best(tune_rf4, "rsq")
)

best_model_non_stand %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(quality ~ .,
      data = juice(red_wine_prepped2)) %>%
  vip(geom = "point")
```

![](random_forest_regression_wine_quality_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

### Use the best parameters to model the test set

### Train with a Random Forest model using the non-standardized training set

### rsquared \~ 0.47

``` r
rf_final_spec2 <- rand_forest(
  min_n = 12,
  trees = 1000,
  mtry = 6
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

set.seed(1111)
rf_final_model2 <- rf_final_spec2 %>%
  fit(quality ~ .,
      data = juice(red_wine_prepped2))


rf_final_model2
```

    ## parsnip model object
    ## 
    ## Fit time:  1.2s 
    ## Ranger result
    ## 
    ## Call:
    ##  ranger::ranger(formula = quality ~ ., data = data, mtry = ~6,      num.trees = ~1000, min.node.size = ~12, num.threads = 1,      verbose = FALSE, seed = sample.int(10^5, 1)) 
    ## 
    ## Type:                             Regression 
    ## Number of trees:                  1000 
    ## Sample size:                      1200 
    ## Number of independent variables:  11 
    ## Mtry:                             6 
    ## Target node size:                 12 
    ## Variable importance mode:         none 
    ## Splitrule:                        variance 
    ## OOB prediction error (MSE):       0.3433301 
    ## R squared (OOB):                  0.4739122

### Re-check the final model of the standardized dataset

``` r
rf_final_model1
```

    ## parsnip model object
    ## 
    ## Fit time:  931ms 
    ## Ranger result
    ## 
    ## Call:
    ##  ranger::ranger(formula = quality ~ ., data = data, mtry = ~6,      num.trees = ~1000, min.node.size = ~12, num.threads = 1,      verbose = FALSE, seed = sample.int(10^5, 1)) 
    ## 
    ## Type:                             Regression 
    ## Number of trees:                  1000 
    ## Sample size:                      1200 
    ## Number of independent variables:  11 
    ## Mtry:                             6 
    ## Target node size:                 12 
    ## Variable importance mode:         none 
    ## Splitrule:                        variance 
    ## OOB prediction error (MSE):       0.5263239 
    ## R squared (OOB):                  0.4736761

### Get test set performance of the non-standardized dataset

``` r
predicted_vals2 <- predict(rf_final_model2,
                           new_data = bake(red_wine_prepped2, new_data = red_wine_testing))

original_quality2 <- bake(red_wine_prepped2, new_data = red_wine_testing) %>%
  select(quality)


predicted_vals2
```

    ## # A tibble: 399 x 1
    ##    .pred
    ##    <dbl>
    ##  1  5.23
    ##  2  4.95
    ##  3  5.23
    ##  4  5.26
    ##  5  5.33
    ##  6  5.36
    ##  7  5.01
    ##  8  5.23
    ##  9  5.56
    ## 10  5.60
    ## # ... with 389 more rows

``` r
rmse2 <- rmse(bind_cols(original_quality2,
                        predicted_vals2), "quality", ".pred", na.rm = T)
rsq2 <- rsq(bind_cols(original_quality2,
                      predicted_vals2), "quality", ".pred", na.rm = T)

juice(red_wine_prepped2) %>% dim
```

    ## [1] 1200   12

``` r
n = 1200
p = 12
adjusted_rsq2 <- (1 - ((1 - rsq2$.estimate) * (n - 1) / (n - p - 1)))
```

### Test Set RMSE

``` r
rmse2
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 rmse    standard       0.591

### Test Set r-squared

``` r
rsq2
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 rsq     standard       0.464

### Test Set adjusted r-squared

``` r
adjusted_rsq2
```

    ## [1] 0.4587659

# Takeaways

### It seems that standardizing a dataset before fitting it into a Tree-Based Model doesn’t improve the model.In fact based on this test it the metrics were slightly worse-since we got a higher RMSE on the standardized model and the predictions lost its raw value. This seems to prove that Tree-Based models are shielded from the magnitude effects from our features and better feature selection might be the key to get better models.
