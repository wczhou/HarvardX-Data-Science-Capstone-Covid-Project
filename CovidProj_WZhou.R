# Title: HarvardX Data Science: Capstone Project 2 Covid-19 Cases and world economy
# Weichen Zhou
# Date last edited: 01/08/2021
# Reference: Edx Data Science course notes 
# and textbook "Introduction to Data Science Data Analysis and Prediction Algorithms with R" By Prof. Rafael A. Irizarry

# Data source: Covid-19 and world economy data: credit to Shashwat Tiwari on Kaggle
#              https://www.kaggle.com/shashwatwork/impact-of-covid19-pandemic-on-the-global-economy?select=raw_data.csv
#              Country to continent data: credit to Chaitanya Gokhale on Kaggle
#              https://www.kaggle.com/statchaitya/country-to-continent?select=countryContinent.csv

# Overall description:  This is the R script for the second project in HarvardX Data Science Capstone.
#                       The world has been greatly affected by Covid-19 in the year 2020. 
#                       I found the data on Kaggle that documented number of cases in differenct countries and their GDP per capita
#                       My goal in this project is to explore the data 
#                       and see if there's relationship between the world economy and Covid-19 cases.

# Install necessary packages: 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")


# Load packages
library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggrepel)


# I tried downloading the data directly with R from Kaggle and for some reason it wouldn't work.
# I have included the two csv files needed for the project with the submission.
# Please be sure to include them in the working directory before running this script.
# Credit for reference data is states above.

# Read in the raw data containing country, covid-19 and economy information
dat <- read.csv("raw_data.csv")

# I want to include continent information in my analysis 
# but the original raw_data doesn't have it
# So I'm including it from another source
continent <- read.csv("countryContinent.csv")

# Explore the raw data
head(dat)

# Notice that there are columns with unclear information, 
# those column names are X, X.1,X.2,X.3 and X.4
# Not sure what they do, so remove them
# And we rename the columns so the column names are shorter
dat <- dat %>% select(-X,-X.1,-X.2,-X.3,-X.4) %>% rename(
  iso = iso_code,
  country = location,
  cases = total_cases,
  deaths = total_deaths,
  stringency = stringency_index,
  gdp = gdp_per_capita,
  human_development = human_development_index
)
head(dat)

# Explore the continent data
head(continent)
continent_info <- continent %>% rename(iso = code_3) %>% select(iso, continent, sub_region,region_code)

# Join the continent data
covidecon <- left_join(dat,continent_info,by="iso")
head(covidecon)

# We only want to look at data in the year 2020, so we'll extract those data
covidecon <- covidecon %>% mutate(date = as_datetime(date), month = month(date)) %>% filter(year(date) == 2020)
head(covidecon)

# Notice that there are many NA data in the file
temp <- covidecon %>% filter(is.na(cases) | is.na(deaths) | is.na(gdp) | is.na(stringency))
nrow(temp)
# This indicate that I didn't get a very nice dataset, 
# but I do not have time to find a new one so I'll stick with this.

# we'll treat the NA's for cases, deaths, and stringency as one's. 
covidecon$cases[is.na(covidecon$cases)] <- 1
covidecon$deaths[is.na(covidecon$deaths)] <- 1
covidecon$stringency[is.na(covidecon$stringency)] <- 1

# Take a look at gdp in particular, if we have absolutely no gdp info,
# we'll have to remove the country
temp <- covidecon %>% filter(is.na(gdp))
head(temp)
country_no_gdp <- temp %>% distinct(country)
country_to_rm <-  covidecon %>% group_by(country) %>% summarize(temp_gpd = max(gdp,na.rm=TRUE)) %>% filter(temp_gpd == -Inf) %>% select(country)

# Note: contry to remove and country with no gdp are the same,
country_no_gdp$country == country_to_rm$country

# so we'll just remove these countries
# define a not-in operator first
`%!in%` = Negate(`%in%`)
covidecon <- covidecon %>% filter(country %!in% country_to_rm$country)

# Now our data set should be good and we can explore some
# First, we split our data into a training set and a test set
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = covidecon$cases, times = 1, p = 0.1, list = FALSE)
train_set <- covidecon[-test_index,]
temp <- covidecon[test_index,]

# Make sure the countries in the validation set are all included in the train set
validation <- temp %>% 
  semi_join(train_set, by = "country") 

# Add the removed rows from the validation set back to the train set
removed <- anti_join(temp, validation)
train_set <- rbind(train_set, removed)


# First thing to look at the number of total covid-cases by country
total_by_country <- train_set %>% group_by(country) %>% summarize(total_cases = sum(cases)) 
total_by_country %>% filter(total_cases > 10000000) %>%
  ggplot(aes(reorder(country,total_cases),total_cases)) +
  geom_bar(stat = "identity") +coord_flip() + ggtitle("Countries with most cases")

train_set<-left_join(train_set,total_by_country,by="country")
train_set %>% filter(total_cases > 10000000) %>% ggplot(aes(reorder(country,total_cases),population)) +
  geom_bar(stat = "identity") +coord_flip() + ggtitle("Population of Countries with most cases")



# Gdp and cases
train_set %>% filter(total_cases > 10000000) %>% ggplot(aes(total_cases,gdp, label = country)) +
  geom_point() + geom_text(aes(label=country))+ ggtitle("GDP and total cases")


# Cases and stringency
train_set %>% group_by(stringency) %>% summarise(str_sum_cases=sum(cases)) %>%
  ggplot(aes(stringency,str_sum_cases)) + geom_point()

# Lets look at how continents affect the case number
continent_avg <- train_set %>% group_by(continent) %>% summarise(sum_cases = sum(cases)) 
continent_avg %>% ggplot(aes(continent,sum_cases)) + 
  geom_bar(stat="identity") + ggtitle("Continent and total cases") 

# We want to predict the number of covid-19 cases by 
# building a model considering the effect of population, gdp, and continent
# here we'll use a few models we learned to see which one gives the highest accuracy

# lm
lm_fit <- train(cases ~ population+gdp+continent+stringency,method = "lm", data=train_set,na.action=na.omit)
y_hat_lm <- predict(lm_fit,newdata = na.omit(validation))
mean(y_hat_lm == na.omit(validation)$cases)

# rpart
rpart_fit <- train(cases ~ population+gdp+continent+stringency,method = "rpart", data=train_set,na.action=na.omit)
y_hat_rpart <- predict(rpart_fit,newdata = na.omit(validation))
mean(round(y_hat_rpart) == na.omit(validation)$cases)

# glm
glm_fit <- train(cases ~ population+gdp+continent+stringency,method = "glm", data=train_set,na.action=na.omit)
y_hat_glm <- predict(glm_fit,newdata = na.omit(validation))
mean(round(y_hat_glm) == na.omit(validation)$cases)

# rf is taking very long time to run and given the previous few tries, 
# I'm not sure if this is the right model to try 
#rf_fit <- train(cases ~ population+gdp+continent+stringency,method = "rf", data=train_set,na.action=na.omit)
#y_hat_rf <- predict(rf_fit,newdata = na.omit(validation))
#mean(round(y_hat_rf) == na.omit(validation)$cases)


