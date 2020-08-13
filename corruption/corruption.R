cpi_raw <- read.csv("CPI_2019_final_dataset.csv")

View(cpi_raw)

# add features
# happiness
# weighted hdi index

library(skimr)
library(tidymodels)
library(tidyverse)

library(ggmap)
library(maptools)
library(maps)  
library(ggrepel)

# Remove some corruption ratings due to lack of data

cpi_filtered <- cpi_raw %>%
  select(-standard.error, -Number.of.sources, -Lower.CI, -Upper.CI, -African.Development.Bank.CPIA,
         -Bertelsmann.Foundation.Sustainable.Governance.Index, -PERC.Asia.Risk.Guide,
         -Freedom.House.Nations.in.Transit.Ratings)

skim(cpi_filtered) 
# PRS.International.Country.Risk.Guide Distribution seems less normal

names(cpi_filtered)

names(cpi_filtered) <- c("Country",
                         "ISO3",
                         "Region",
                         "CPI_score_2019",
                         "Rank",
                         "Bertelsmann_Foundation_Transformation_Index",
                         "Economist_Intelligence_Unit_Country_Ratings",
                         "Global_Insight_Country_Risk_Ratings",
                         "IMD_World_Competitiveness_Yearbook",
                         "PRS_International_Country_Risk_Guide",
                         "Varieties_of_Democracy_Project",
                         "World_Bank_CPIA",
                         "World_Economic_Forum_EOS",
                         "World_Justice_Project_Rule_of_Law_Index")
dim(cpi_filtered)

cpi_filtered <- cpi_filtered %>%
  filter(Country != "p")

dim(cpi_filtered)

View(cpi_filtered)


happy_2019 <- read.csv("happiness_2019.csv")
happy_2019_filtered <- happy_2019 %>%
  select(Country.or.region, Score)

names(happy_2019_filtered) <- c("Country", "Happiness_Score")




happy_2019_filtered %>% arrange(Country) %>% .$Country


# Congo (Brazzaville) : Congo
# "Congo (Kinshasa)": Democratic Repul
# Ivory Coast
# South Korea
# United States of America

# Rename some countries
happy_2019_filtered[happy_2019_filtered$Country == "Congo (Brazzaville)", "Country"] <- "Congo"
happy_2019_filtered[happy_2019_filtered$Country == "Congo (Kinshasa)", "Country"] <- "Democratic Republic of the Congo"
happy_2019_filtered[happy_2019_filtered$Country == "Ivory Coast", "Country"] <- "Cote d'Ivoire"
happy_2019_filtered[happy_2019_filtered$Country == "United States", "Country"] <- "United States of America"

cpi_filtered[cpi_filtered$Country == "Dominica", "Country"] <- "Dominican Republic"
cpi_filtered[cpi_filtered$Country == "Korea, South", "Country"] <- "South Korea"

left_join(cpi_filtered, happy_2019_filtered) %>% filter(is.na(Happiness_Score)) %>%
  arrange(Country) %>% .$Country

cpi_happy_filtered <- left_join(cpi_filtered, happy_2019_filtered)


ihdi_2018_raw <- read.csv("IHDI_2018.csv")
names(ihdi_2018_raw) <- c("Rank", "Country", "IHDI")
ihdi_2018_filtered <- ihdi_2018_raw %>%
  mutate(Country = str_remove_all(Country, "Â"),
         Country = str_extract(Country, "\\w{1}.*$")) %>%
  select("Country", "IHDI")
         
ihdi_2018_filtered$Country[1]
ihdi_2018_filtered$Country

ihdi_2018_filtered[ihdi_2018_filtered$Country == "Guinea-Bissau", "Country"] <- "Guinea Bissau"
ihdi_2018_filtered[ihdi_2018_filtered$Country == "United States", "Country"] <- "United States of America"

cpi_happy_ihdi_filtered <- left_join(cpi_happy_filtered, ihdi_2018_filtered, by = "Country")

View(cpi_happy_ihdi_filtered)

# Create reversed rankings
cpi_happy_ihdi_filtered <- cpi_happy_ihdi_filtered %>%
  mutate(Reversed_Rank = (180 - Rank) + 1)

View(cpi_happy_ihdi_filtered)
skim(cpi_happy_ihdi_filtered)


# Pre-modelling
cpi_recipe <- recipe(Rank ~ .,
                  data = cpi_happy_ihdi_filtered) %>%
  update_role(Country, new_role = "Country") %>%
  update_role(ISO3, new_role = "Country_Code") %>%
  update_role(Region, new_role = "Region") %>%
  step_knnimpute(all_predictors(),
    columns = c("Bertelsmann_Foundation_Transformation_Index",
                 "Economist_Intelligence_Unit_Country_Ratings",
                 "IMD_World_Competitiveness_Yearbook",
                 "PRS_International_Country_Risk_Guide",
                 "Varieties_of_Democracy_Project",
                 "World_Bank_CPIA",
                 "World_Economic_Forum_EOS",
                 "World_Justice_Project_Rule_of_Law_Index",
                 "Happiness_Score",
                 "IHDI"),
                 neighbors = 5) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric()) %>%
  prep()

cpi_recipe
juice(cpi_recipe) %>% View
skim(juice(cpi_recipe))

cpi_matrix <- juice(cpi_recipe) %>%
  select(-Rank, -Country, -ISO3, -Region)

set.seed(1992)
kmeans_results <- sapply(1:10, function(x){
  kmeans_result <- kmeans(cpi_matrix, centers = x)
}) 

group_tot_withinss <- sapply(kmeans_results[5, ], function(x){
  x[1]
})

data.frame(k = 1:10, tot_withinss = group_tot_withinss) %>%
  ggplot(aes(k, tot_withinss)) +
  geom_point() +
  geom_line()

### Choose k = 4
kmeans_chosen <- kmeans_results[, 4]
kmeans_chosen$cluster %>%
  table %>%
  prop.table * 100.0 

### MAP 
map_location <- read.csv("countries_capitals_long_lat.csv")
map_location

world_map <- borders("world", colour = "darkgrey", fill = "white")
cpi_final <- left_join(cpi_happy_ihdi_filtered, map_location)
cpi_final_filtered <- cpi_final %>% 
  select(Country, Longitude, Latitude, Country, Rank, IHDI) %>%
  mutate(cluster = kmeans_chosen$cluster)

c1 <- cpi_final_filtered %>%
   filter(cluster == 1)
c2 <- cpi_final_filtered %>%
  filter(cluster == 2)
c3 <- cpi_final_filtered %>%
  filter(cluster == 3)
c4 <- cpi_final_filtered %>%
  filter(cluster == 4)



ggplot(c1) + world_map +
  geom_point(aes(Longitude,
                Latitude,
                color = as.factor(cluster)),
                size = 2.5) +
  geom_text_repel(aes(Longitude, Latitude, label = Country),
                  size  = 3.5,
                  segment.color = "black")


ggplot(c2) + world_map +
  geom_point(aes(Longitude,
                 Latitude,
                 color = as.factor(cluster)),
             size = 2.5) +
  geom_text_repel(aes(Longitude, Latitude, label = Country),
                  size  = 3.5,
                  segment.color = "black")


ggplot(c3) + world_map +
  geom_point(aes(Longitude,
                 Latitude),
             size = 2.5) +
  geom_text_repel(aes(Longitude, Latitude, label = Country),
                  size  = 3.5,
                  segment.color = "black") +
  scale_color_manual(values = c("red")) +
  labs(x = "", y = "", title = "Corrupt and Underdeveloped") +
  theme(plot.title = element_text(hjust = 0.5),)
theme_bw()


ggplot(c4) + world_map +
  geom_point(aes(Longitude,
                 Latitude,
                 color = as.factor(cluster),
                 alpha = 0.5),
             size = 3) +
  geom_text_repel(aes(Longitude, Latitude, label = Country),
                  size  = 3.5,
                  segment.color = "black") +
  scale_color_manual(values = c("red")) +
  labs(x = "", y = "", title = "Corrupt and Underdeveloped") +
  theme(plot.title = element_text(hjust = 0.5))
  theme_bw()


c1 %>%
  .$Rank %>% median

c1 %>%
  .$IHDI %>% median(na.rm = T)



c2 %>%
  .$Rank %>% median
c2 %>%
  .$IHDI %>% median(na.rm = T)



c3 %>%
  .$Rank %>% median

c3 %>%
  .$IHDI %>% median(na.rm = T)


c4 %>%
  .$Rank %>% median


# References:
  # https://en.wikipedia.org/wiki/List_of_countries_by_inequality-adjusted_HDI


# relative honesty 
# fair
# corrupt
# 
# developed
# developing
# underdeveloped
# 
# c1: relatively honest x developed
# 
# c2: corrupt x developing

# c3: fair x developed

# c4: corrupt x underdeveloped

