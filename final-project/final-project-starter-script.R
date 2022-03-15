####################################################
# Final Project Starter Script
# by Isaac J. Faber
####################################################

# In this assignment you will build a model and then
# push it into production within a web application.
# This script will build the same model from the last
# assignment.

library(tidyverse)
library(lubridate)
library(caTools)

#####################################################################
# Reading a csv file into R and putting it in data table format
# Step 1 set the working directory in the session tab above to the 
# 'To Source File Location'
#####################################################################

weather <- read.csv("data/Vineyard_weather_1948-2017.csv")

weather$PRCP <- as.numeric(weather$PRCP)

#these next commands replaces missing values with an average of the rest of the columns
weather$PRCP = ifelse(is.na(weather$PRCP),
                      ave(weather$PRCP, FUN = function(x) mean(x, na.rm = TRUE)),
                      weather$PRCP)

weather$RAIN = ifelse((( weather$PRCP > 0 ) & ( is.na(weather$RAIN ))), TRUE, weather$RAIN)

####################################################################
# Harvest is in september
# So we want to separate the date field into different fields
# This will allow us to the calculate weekly statistics
#####################################################################

weather$DAY <- day(weather$DATE)
weather$MONTH <- month(weather$DATE)
weather$YEAR <- year(weather$DATE)
weather$WEEK <- week(weather$DATE)

#####################################################################
# We now have a column for Day, Month, Year and Week
# How should we think about accumulating rows
# The final output should be a row per week
#####################################################################

weekly_weather <- weather %>%
  group_by(YEAR, WEEK)  %>%
  summarise(PRCP = sum(PRCP), 
            TMAX = max(TMAX), 
            TMIN = min(TMIN),
            RAIN = sum(RAIN))

#####################################################################
# Our aggregation by week has converted our RAIN column to numeric
# If we want a categorical value, we must pick a way to measure it
# Lets assume that for the harvest to be impacted PRCP is greater than 0.35 level
# and TMAX must be less than 80. Let's also change the name of the column to STORM
#####################################################################

names(weekly_weather)[6]<-"STORM"

weekly_weather$STORM <- ifelse(weekly_weather$PRCP >= 0.35 & weekly_weather$TMAX<=80, 1, 0)

#####################################################################
# We know that the harvest happens usually in the 40th week of the year
# This means we are interested in storms in the 5 weeks prior to the 40th
# Lets subset our data to only the relevant fields
#####################################################################

weekly_weather$WEEK <- as.numeric(weekly_weather$WEEK)

weekly_relevant <- weekly_weather %>% 
  filter(WEEK >= 35 & WEEK < 40)

#####################################################################
# What else can we do with all this data
# Let's build our Machine Learning model for prediction (Logistic Regression)
# Because we won't know the information for the week in question
# We need to structure the data set as a time series
# we want to predict next week based on the last two week's values
#####################################################################

TS_weather = data.frame("STORM" = NA, 
                        "PRCPt1"  = NA, 
                        "TMAXt1" = NA, 
                        "TMINt1" = NA, 
                        "PRCPt2" = NA, 
                        "WEEK" = NA)

for (i in 1:nrow(weekly_weather)){
  TS_weather[i,1] <- weekly_weather[i,6]
  if (i == 1){
    TS_weather[i,2] = 0
    TS_weather[i,3] = 0
    TS_weather[i,4] = 0
    TS_weather[i,5] = 0
    TS_weather[i,6] = 0
  }else if (i==2){
    TS_weather[i,2] = weekly_weather[i-1,3]
    TS_weather[i,3] = weekly_weather[i-1,4]
    TS_weather[i,4] = weekly_weather[i-1,5]
    TS_weather[i,5] = 0
    TS_weather[i,6] = weekly_weather[i-1,2]
  }else{
    TS_weather[i,2] = weekly_weather[i-1,3]
    TS_weather[i,3] = weekly_weather[i-1,4]
    TS_weather[i,4] = weekly_weather[i-1,5]
    TS_weather[i,5] = weekly_weather[i-2,3]
    TS_weather[i,6] = weekly_weather[i-1,2]
  }
}

TS_weather$STORM = factor(TS_weather$STORM, levels = c(0, 1))

#####################################################################
# The next step is to separate training and test set
# Lets randomly take 75% of our data as a training set
#####################################################################

set.seed(123)
split = sample.split(TS_weather$STORM, SplitRatio = 0.75)
training_set = subset(TS_weather, split == TRUE)
test_set = subset(TS_weather, split == FALSE)


#####################################################################
# Now let's create our regression
# glm means: Generalized Linear Model
# family = binomial means its a logit regression
#####################################################################

classifier = glm(formula = STORM ~ PRCPt1 + TMAXt1,
                 family = binomial,
                 data = training_set)

#####################################################################
# Let's save the classifier as an object.
# This is done so we can call use it elsewhere (like a production app)
# It comes in handy when hosting a solution that uses an object
#####################################################################

saveRDS(classifier, file = "data/prediction.RData")

#######################################
# now go open the app.R file 
# follow the directions from there
#######################################
