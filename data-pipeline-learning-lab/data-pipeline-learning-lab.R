####################################################
# Data Pipeline Example
# by Isaac J. Faber
####################################################

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

# step 2 Transform the data
# now that we have the data from the API we want to manipulate it into something we can use
# in our case for now we only care about temp, pressure, dt (date time in unix, UTC)

api_data <- data.frame(temp = api_response$main$temp,
                       pressure = api_response$main$pressure,
                       dt = api_response$dt)

str(api_data)

# step 3 Load the data
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

################### problem ###########################
# change step 2 to capture one more piece of data from the api_response and write it to the file system

api_data <- data.frame(temp = api_response$main$temp,
                       pressure = api_response$main$pressure,
                       dt = api_response$dt,
                       new_piece_of_data = 'replace this with your new data')

write.csv(api_data, file = paste0('weather-data-new',Sys.Date()))

# download and submit this script when you are complete
