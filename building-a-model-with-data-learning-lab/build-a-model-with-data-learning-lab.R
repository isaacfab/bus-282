##########################################################
# Build a Model from Data Learning Lab
# by Isaac J. Faber
# Exploratory Data Analysis and Training a Model
##########################################################

############ Background #########################
# In this section we will look at the process of 
# analyzing data and creating a model
# the data of interest is of weather collected over
# many decades. Our hope is to explore the data and
# see if it can be used to predict the event of a 
# storm before harvest.

############ Exploratory Data Analysis ###################
# The following code should be run line by line by placing 
# the cursor on a line or highlighting a section and 
# pressing the run button (in the upper right corner of this 
# window pane) 

#####################################################################
# Loading Libraries of functions that will be needed
#####################################################################

library(tidyverse)
library(lubridate)
library(caTools)
library(ROCR)

#####################################################################
# Reading a csv file into R and putting it in data table format
# Step 1 set the working directory in the session tab above to the 
# 'To Source File Location'
#####################################################################

weather <- read.csv("data/Vineyard_weather_1948-2017.csv")

#####################################################################
# Lets do some exploration of this data set in the console (pane below)
# We can use commands such as summary(weather) to view some statistics
# We should be looking for missing values or inconsistencies that jump at us
#####################################################################

#####################################################################
# There are some NAs in the rain data (missing data), lets explore that column more
#####################################################################

rain <- weather %>%
  filter(RAIN)

norain <- weather %>%
  filter(!RAIN)

nodata <- weather %>% 
  filter(is.na(RAIN))

summary(rain)

summary(norain)

summary(nodata)

# Question: What do you notice of interest about these values?
# Answer:

# Now that we have explored these subsets we can remove them from the session to save memory
rm(nodata)
rm(norain)
rm(rain)

#####################################################################
# There are some NAs in the PRCP column
# what are interesting ways to get rid of them?
# what impact does this manipulation have on my analysis
#####################################################################

weather$PRCP <- as.numeric(weather$PRCP)

#these next commands replaces missing values with an average of the rest of the columns
weather$PRCP = ifelse(is.na(weather$PRCP),
                      ave(weather$PRCP, FUN = function(x) mean(x, na.rm = TRUE)),
                      weather$PRCP)

weather$RAIN = ifelse((( weather$PRCP > 0 ) & ( is.na(weather$RAIN ))), TRUE, weather$RAIN)

# Question: Is it reasonable to replace missing values with an average of other values? 
# What other options are there?
# Answer:

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
# Lets do a couple of plots and see what we can glimpse from the data
#####################################################################

weekly_relevant %>%
  ggplot(aes(x = WEEK, y = STORM)) +
  geom_point(color = "darkorchid4") +
  labs(title = "Storm on weeks 35 to 40 - Vineyard",
       y = "Storm",
       x = "Week") + theme_bw(base_size = 15)

#####################################################################
# That wasn't very helpful, lets facet it by year
#####################################################################

weekly_relevant %>%
  ggplot(aes(x = WEEK, y = STORM)) +
  geom_point(color = "darkorchid4") +
  facet_wrap(~ YEAR, ncol = 6) +
  labs(title = "Storm on weeks 35 to 40 - Vineyard",
       subtitle = "Data plotted by year",
       y = "Storm",
       x = "Month") + theme_bw(base_size = 15)

#####################################################################
# Lets see which variables have a higher effect on STORM (click zoom on the plot window)
# this is very crowded but we can see some patterns
#####################################################################

pairs(weekly_relevant)

#####################################################################
# lets look at the last 20 years only (click zoom on the plot window)
#####################################################################

relevant2 <- weekly_relevant%>%
  filter(YEAR >= 1997)

relevant2 %>%
  ggplot(aes(x = WEEK, y = STORM)) +
  geom_point(color = "darkorchid4") +
  facet_wrap(~ YEAR, ncol = 4) +
  labs(title = "Storm on weeks 35 to 40 - Vineyard",
       subtitle = "Data plotted by year",
       y = "Storm",
       x = "Month") + theme_bw(base_size = 15)

#####################################################################
# Lets see which variables have a higher effect on STORM (click zoom on the plot window)
#####################################################################

pairs(relevant2)

# Question: What do you notice as you inspect the data visually? 
# What, if anything, seems to be correlated with STORM?
# Answer: 

#####################################################################
# Lets start our model development with a basic summary statistics
# lets assume we only need a storm in one of the five weeks
# And we want to know how many years did this rain happen
#####################################################################

frequentist_count <- weekly_relevant %>% 
  group_by(YEAR) %>%
  summarise(STORM = sum(STORM))

frequentist_count$STORM <- ifelse(frequentist_count$STORM > 1,
                                  TRUE,
                                  FALSE)
storm = nrow(frequentist_count%>%filter(STORM == TRUE))
nostorm = nrow(frequentist_count%>%filter(STORM == FALSE))
prob_storm = storm/(storm + nostorm)

print(paste('number of storms:',storm))
print(paste('number of no storms:',nostorm))
print(paste('historical proportion of storms:',prob_storm))

# Question: What is interesting about the historical observations?
# Answer: 

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

print(head(training_set))

# Question: What are the features and labels of this dataset?
# Answer: 

#####################################################################
# Now let's create our regression
# glm means: Generalized Linear Model
# family = binomial means its a logit regression
#####################################################################

classifier = glm(formula = STORM ~ .,
                 family = binomial,
                 data = training_set)

# Question: Why do we only 'train' on the training set?
# Answer:


#####################################################################
# Lets get probabilities for the data in the test set
# then we pick a Threshold for predicting Storm or no Storm
# And create a confusion matrix to show us how we did
#####################################################################

prob_pred = predict(classifier, type = 'response', newdata = test_set)
test_set$y_pred = ifelse(prob_pred > 0.5, 1, 0)

test_set <- test_set %>% mutate(correct = ifelse(((STORM == 1 & y_pred == 1)|
                                                    (STORM == 0 & y_pred == 0)),1,0)) 

# accuracy of our model
mean(test_set$correct)

#the confusion matrix compares the prediction to the actual outcome
#this gives an indication of model quality
#0 and 1 rows are the predictions, 0 and 1 columns are the actual outcomes
#both are no storm and storm respectively
cm = table(test_set$STORM, test_set$y_pred)
print(cm)

# Question: How does our model compare with simple historical averages?
# what about does the confusion matrix tell us about the model?
# Answer:

#####################################################################
# So if we knew the expected precipitation and MAX and MIN temp
# Say 0.5 inches of precipitation with a high of 65 and a low of 55 on week 39
# We can obtain a probability for the chance of a storm
#####################################################################

week40 <- data.frame("PRCPt1" = 0.5,"TMAXt1" = 65, "TMINt1" = 55, "PRCPt2" = 0.5, "WEEK" = 39)
probability_storm = predict(classifier, type = 'response', newdata = week40)
probability_storm
#####################################################################
# Lets try this again and get rid of WEEK, TMIN, and PRCPt2 in our model
# Remember we are hoping for a light warm rain
#####################################################################

classifier2 = glm(formula = STORM ~ PRCPt1 + TMAXt1,
                  family = binomial,
                  data = training_set)

#####################################################################
# Lets get probabilities for the data in the test set
# then we pick a Threshold for predicting Storm or no Storm
# And create a confusion matrix to show us how we did
#####################################################################

prob_pred2 = predict(classifier2, type = 'response', newdata = test_set)
test_set$y_pred = ifelse(prob_pred2 > 0.5, 1, 0)

test_set <- test_set %>% mutate(correct = ifelse(((STORM == 1 & y_pred == 1)|
                                                    (STORM == 0 & y_pred == 0)),1,0)) 

# accuracy of our model
mean(test_set$correct)

# Question: What happened to the accuracy with fewer features? why? 
# what does this mean for data ecosystem strategies?
# Answer: 

#####################################################################
# So if we knew the Precipitation and WEEK
# Say 0.5 inches of precipitation and MAXTEMP = 85
# We can obtain a probability for the chance of a storm
#####################################################################

week40 <- data.frame("PRCPt1" = 0.5,"TMAXt1" = 65)
probability_storm2 = predict(classifier2, type = 'response', newdata = week40)
probability_storm2
#####################################################################
# Let's save the classifier as an object.
# This is done so we can call use it elsewhere (like a production app)
# It comes in handy when hosting a solution that uses an object
#####################################################################

saveRDS(classifier2, file = "data/prediction.RData")

# Question: This process of exporting a model is important in the data driven ecosystem. why?
# Answer: 

#####################################################################
# Let's do an interesting Viz now that we have only 2 features
#####################################################################

#some tedious munging here
drops <- c("TMINt1","PRCPt2","WEEK")
set = test_set[, !(names(test_set) %in% drops)]
X1 = seq(min(set$PRCPt1) - 1, max(set$PRCPt1) + 1, by = 0.01)
X2 = seq(min(set$TMAXt1) - 1, max(set$TMAXt1) + 1, by = 0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PRCPt1', 'TMAXt1')
prob_set = predict(classifier2, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
#add a color variable
set$Color <- 'red'
set$Color[set$STORM == 1] <- 'green'

#now plot and view our decision boundary
plot(x = set$PRCPt1,
     y = set$TMAXt1,
     main = 'Logistic Regression Result (Test Set)',
     xlab = 'Percipitation Week t-1', 
     ylab = 'Max Tempature Week t-1',
     xlim = range(X1), 
     ylim = range(X2),
     col = set$Color)
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)

# This line on this plot is our decision boundary for the model
# above this line we predict a storm below we do not

# Question: Would you be comfortable with this model being used for predictions? Justify your answer.
# Answer: