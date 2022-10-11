####################################################
# Data Pipeline Example
# by Isaac J. Faber
####################################################
# Answer the questions in this script as you go. Then adjust the code to solve the problem at the end.
#
# A data pipeline is a scripted or automated movement of data from a sensor or
# data storage system to another.
# 
# Pipelines are a core feature of a data management system. Often times a pipeline is referred to 
# with the term 'ETL' or Extract, Transform, Load. Meaning that the pipeline Extracts data from 
# one source, Transforms in within the pipeline, then Loads it to another system. 
# As systems become more complex so do the pipelines. This lab is a basic example of pipeline 
# and represents the typical components.

# step one import software libraries that provide our pipeline functionality

library(jsonlite) # an R library for collecting web data
library(tidyverse) # a collection of R libraries useful for data manipulation

# !!!!!!! Important from the session menu above change the working directory to the 'source file location'

# step two Extract the Data
# there are two broad categories of data sources; internal and external: 
# those you own and those you borrow from (either open source or paid).

# for this assignment we are using an external data source 
# https://openweathermap.org/api
# this is a service that provides historical and real time weather data relevant to our 
# winemaker case-study.

# the components of the data call
url <- 'https://api.openweathermap.org/data/2.5/weather?' #base url for the API

lat <- 38.2919 #latitude of location
  
lon <- -122.4580#longitude of location
  
units <- 'imperial' #units for the API to return
  
api_key <- 'replace this string with the api key provided on canvas' #security token for the API


api_url <- paste0(url,'lat=',lat,'&lon=',lon,'&units=',units,'&appid=',api_key)

#call the API
api_response <- jsonlite::fromJSON(api_url, flatten = TRUE)

str(api_response)
# Question 1: In your own words what happened during this 'Extract' step and what 
# problems might you encournter when setting something like this up?
# Answer: 

# step three Transform the data
# now that we have the data from the API we want to manipulate it into something we can use
# in our case for now we only care about temp, pressure, dt (date time in unix, UTC)

api_data <- data.frame(temp = api_response$main$temp,
                       pressure = api_response$main$pressure,
                       dt = api_response$dt)

str(api_data)
# Question 2: Why does the data need to be 'Transformed' like we did here?
# Answer:
#
# step four Load the data
# with the data manipulated into the format you want to keep now you need to store it somewhere
# there are, again two, broad options here. Store it as a raw file, or write it into a database.
# here is an example of storing it as a text file (comma separated values)

write.csv(api_data, file = paste0('weather-data-',Sys.Date()))

# here is an example of storing the same data in a database
# this is a sqlite database stored in the file system here (bus282.db)

library(RSQLite)
library(DBI)

con <- dbConnect(RSQLite::SQLite(),'bus282.db')

dbSendQuery(con, 'INSERT INTO weather (temp, pressure, dt) VALUES (:temp, :pressure, :dt);', api_data)

dbGetQuery(con,'SELECT * FROM weather')

dbDisconnect(con)

# Question 3: In this section of the code we 'Loaded' the data into a small local database. What are some
# important considerations when choosing the type of database? What is an alternative we could have 
# used here?
# Answer: 
#

################### problem ###########################
# change step three to capture one more piece of data from the api_response and write it to the file system

api_data <- data.frame(temp = api_response$main$temp,
                       pressure = api_response$main$pressure,
                       dt = api_response$dt,
                       new_piece_of_data = 'replace this with your new data')

write.csv(api_data, file = paste0('weather-data-new',Sys.Date()))

# Question 4: What is the difference in how the data was saved in the problem vs. step four above?
# Answer:
#
# download (or copy/paste) and submit this script as a text file when you are complete
