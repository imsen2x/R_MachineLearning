library(tidymodels)
library(tidyverse)
library(readr)
library(skimr)
library(rsample)

used_cars <- read_csv("usedcars.csv")
used_cars %>% skim
used_cars %>% View
# 6 X 150
# no missing values
used_cars %>%
  ggplot(aes(x = price)) + geom_boxplot()

used_cars %>%
  ggplot(aes(x = mileage)) + geom_boxplot()

# year, model, price, mileage, color, transmission
# price, mileage

# Predict Price
used_cars <- used_cars %>%
  mutate(car_age = case_when((365 * (2012 - year) != 0) ~ 365 * (2012 - year),
                             (365 * (2012 - year) == 0) ~ 1))

# Price Difference in car models seems evident
used_cars %>%
  ggplot(aes(x = model, y = price)) +
  geom_boxplot()

# Price Difference in car transmission seems non-evident
used_cars %>%
  ggplot(aes(x = transmission, y = price)) +
  geom_boxplot()

auto_t <- used_cars %>% filter(transmission == "AUTO")
manual_t <- used_cars %>% filter(transmission == "MANUAL")
auto_t$price %>% skim
manual_t$price %>% skim
# Based on the t-test the difference in difference in means are by chance
t.test(auto_t$price, manual_t$price)

# h0: The means are equal
# h1: The means are not equal
library(ggcorrplot)
p_x_m_correlation <- cor(used_cars %>% select(price, mileage, car_age))
used_cars %>%
  ggplot(aes(x = mileage, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)
ggcorrplot(p_x_m_correlation)

cor_pmat(used_cars)
# Remove the outlier
used_cars <- subset(used_cars, !((used_cars$price > 12000) & (used_cars$mileage > 120000)))
used_cars %>% View

used_cars %>% glimpse
used_cars_pre_train <- used_cars %>% select(price, mileage, car_age, model)
# Features: model, price, mileage, car_age

set.seed(seed = 1972)
  
train_test_split <-
  rsample::initial_split(
    data = used_cars_pre_train,
    prop = 0.8
  )

train_table <- train_test_split %>% training()
test_table <- train_test_split %>% testing()


recipe_prepped <- recipe(price ~ model + mileage + car_age, data = train_table) %>%
  step_log(price, mileage, car_age, base = 10) %>%
  step_string2factor(model) %>%
  prep(data = train_table)


recipe_prepped <- recipe(price ~ model + mileage, data = train_table) %>%
  step_log(price, mileage, car_age, base = 10) %>%
  step_string2factor(model) %>%
  prep(data = train_table)

train_baked <- bake(recipe_prepped, new_data = train_table)
test_baked  <- bake(recipe_prepped, new_data = test_table)

linear_regressor <-
  linear_reg() %>%
  set_engine("lm") %>%
  fit(price ~ model + mileage + car_age, data = train_baked)

linear_regressor <-
  linear_reg() %>%
  set_engine("lm") %>%
  fit(price ~ model + mileage, data = train_baked)

linear_regressor %>% tidy
linear_regressor$fit %>% glance

predictions <- predict(linear_regressor, new_data = test_baked)
(10^test_baked$price - 10^predictions)^2 %>% sum
(test_baked$price - predictions)^2 %>% sum
# 10 ^ Residuals
(10^test_baked$price - 10^predictions) %>% 
  ggplot(aes(x = .pred)) +
  geom_density()

# Raw Residuals 
(test_baked$price - predictions) %>% 
  ggplot(aes(x = .pred)) +
  geom_density()


?predict
linear_reg() %>% 
  set_engine("lm") %>%
  
lm(price ~ mileage + car_age, data = train_table)


cor(used_cars %>% select(price, mileage, car_age))
used_cars$mileage %>% min 
used_cars$price%>% min 
used_cars$car_age%>% min 
