#Obtaining of data from different sources
#Using webscraping to obtain data from wikipedia
library(rvest)
library(httr)
library(tidyverse)
library(RSQLite)
library(tidymodels)
library(stringr)



url <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"
#Getting the rootnode
root_node <- read_html(url)
#Read the rootnode into table node since the url contains 3 table nodes
table_nodes <- html_nodes(root_node, "table")

#Read the table nodes into dataframes
df1 <- html_table(table_nodes, fill = TRUE)
bike_sharing <- print(df1[[1]])

#summary of the dataframe
summary(bike_sharing)
#Exporting the dataframe into a csv file
write.csv(bike_sharing, "raw_bike_sharing_systems.csv")



#Getting the current weather data of seoul using OpenWeather API
current_weather_url <- 'https://api.openweathermap.org/data/2.5/weather'

#Creation of a list to hold url parameters
my_api_key <- '0619a22dcba87aa6150db645c1bd750a'
current_query <- list(q = 'Seoul',         #q is the city name
                      appid = my_api_key,  #appid is the api key variable
                      units = 'metric')    #unit is the preferred unit system

#Getting the response using the query
response <- GET(current_weather_url, query = current_query)
http_type(response)

#Reading the content of the json response output
json_result <- content(response, as = 'parsed')
class(json_result)



#creating a dataframe with a function that returns a 5 day weather for a list of cities
# Get forecast data for a given city list
get_weather_forecasting <- function(city_names) {
  city <- c()
  weather <- c()
  visibility <- c()
  temp <- c() 
  temp_min <- c()
  temp_max <- c() 
  pressure <- c() 
  humidity <- c() 
  wind_speed <- c()
  wind_deg <- c()
  forecast_datetime <- c()
  season <- c()
  
  for (city_name in city_names) {
    
    forecast_url <- 'https://api.openweathermap.org/data/2.5/forecast'
    
    forecast_quer <- list(q = city_name, 
                          appid = my_api_key, 
                          units= 'metric')
    
    response <- GET(forecast_url,query= forecast_quer)
    
    jsonresult <- content(response, as='parsed')
    
    results <- jsonresult$list
    
    for (result in results) {
      
      city <- c(city, city_name)
      
      weather <- c(weather, result$weather[[1]]$main)
      
      visibility <- c(visibility, result$visibility)
      
      temp <- c(temp, result$main$temp)
      
      temp_min <- c(temp_min, result$main$temp_min)
      
      temp_max <- c(temp_max, result$main$temp_max)
      
      pressure <- c(pressure, result$main$pressure)
      
      humidity <- c(humidity, result$main$humidity)
      
      wind_speed <- c(wind_speed, result$wind$speed)
      
      wind_deg <- c(wind_deg, result$wind$deg)
      
      forecast_datetime <- c(forecast_datetime, result$dt_txt)
      
      season <- c(season, "Spring")
      
    }
    
  }
  
  df <- data.frame(city=city, 
                   weather=weather, 
                   visibility=visibility, 
                   temp=temp, 
                   temp_min=temp_min, 
                   temp_max=temp_max, 
                   pressure=pressure, 
                   humidity=humidity, 
                   wind_speed=wind_speed, 
                   wind_deg=wind_deg, 
                   forecast_datetime= forecast_datetime, 
                   season= season)
  
  return(df)
  
}

cities <- c('Seoul', 'Washington, D.C.', 'Paris', 'Suzhou')

cities_weather_df <- get_weather_forecasting(cities)

forecasting_csv <- write.csv(cities_weather_df, 'cities_weather_df.csv', row.names = FALSE)

##PERFORMING DATA WRANGLING ON THE DATASETS
#USING REGULAR EXPRESSIONS
dataset_list <- c('raw_bike_sharing_systems.csv', 
                  'raw_seoul_bike_sharing.csv', 
                  'raw_cities_weather_forecast.csv', 
                  'raw_worldcities.csv')

#standardizing the column names and changing them to upper cases
for (dataset_name in dataset_list){
  # Read dataset
  dataset <- read_csv(dataset_name)
  # Standardized its columns:
  colnames(dataset) <- str_replace_all(colnames(dataset),' ','_')
  # Convert all column names to uppercase
  colnames(dataset) <- toupper(colnames(dataset))  
  # Replace any white space separators by underscores, using the str_replace_all function
  
  # Save the dataset 
  write.csv(dataset, dataset_name, row.names=FALSE)
}

#reading of the data set to view changes
for (dataset_name in dataset_list){
  print(summary(dataset_name))
}

#preprocessing of the web scaped data
bike_sharing_df <- read_csv("raw_bike_sharing_systems.csv")
head(bike_sharing_df)

#Selecting of columns for processing [country, city, system, bicycles]
sub_bike_sharing_df <- bike_sharing_df %>% 
  select(COUNTRY, CITY, SYSTEM, BICYCLES)
#checking the types of columns
sub_bike_sharing_df %>% 
  summarize_all(class) %>%
  gather(variable, class)

#Bicycle is supposed to be a numeric type but it has a character type
# grepl searches a string for non-digital characters, and returns TRUE or FALSE
# if it finds any non-digital characters, then the bicyle column is not purely numeric
find_character <- function(strings) grepl("[^0-9]", strings)
sub_bike_sharing_df %>% 
  select(BICYCLES) %>% 
  filter(find_character(BICYCLES)) %>%
  slice(0:10)

#checking for reference links in the other variables
#COUNTRY
ref_pattern <- "\\[[A-z0-9]+\\]"
find_reference_pattern <- function(strings) grepl(ref_pattern, strings)
sub_bike_sharing_df %>% 
  select(COUNTRY) %>% 
  filter(find_reference_pattern(COUNTRY)) %>%
  slice(0:10)
#SYSTEM
sub_bike_sharing_df %>% 
  select(CITY) %>% 
  filter(find_reference_pattern(CITY)) %>%
  slice(0:10)
#SYSTEM
sub_bike_sharing_df %>%
  select(SYSTEM) %>% 
  filter(find_reference_pattern(SYSTEM)) %>% 
  slice(0:10)

#Removing links using regular expressions
remove_ref <- function(strings) {
  ref_pattern <- "\\[[A-z@0-9]+\\]"
  # Replace all matched substrings with a white space using str_replace_all()
  result <- str_replace_all(strings, ref_pattern, " ")
  return(result)
}
result <- sub_bike_sharing_df %>% 
  mutate(CITY=remove_ref(CITY),
         SYSTEM = remove_ref(SYSTEM))
#checking to see all ref links are removed
result %>% 
  select(CITY, SYSTEM, BICYCLES) %>% 
  filter(find_reference_pattern(CITY) | find_reference_pattern(SYSTEM) | find_reference_pattern(BICYCLES))

#Extracting numeric values using regular expressions
extract_num <- function(columns){
  # Define a digital pattern
  digitals_pattern <- "[0-9]+"
  # Find the first match using str_extract
  result <- str_extract(columns, digitals_pattern)
  # Convert the result to numeric using the as.numeric() function
  return(as.numeric(result))
}
result <- result %>%
  mutate(BICYCLES = extract_num(BICYCLES))
#returning the summary of the result
write_csv(result, 'bike_sharing_systems.csv')

#PERFORMING DATA WRANGLING WITH DPLYR
#Loading of data seoul bike sharing
bike_sharing_df <- read_csv("raw_seoul_bike_sharing.csv")
#focusing on the rented bike column with 295 missing values out 8760
dropNA <- bike_sharing_df %>% drop_na(RENTED_BIKE_COUNT)

#handing of NA in the temperature column
bike_sharing_df %>% 
  filter(is.na(TEMPERATURE))
#all NA values are found in the column of that corresponds with summer
#imputing the summer average temperature and replacing the NA with the average
summer_df <- dropNA %>% 
  select(TEMPERATURE, SEASONS) %>% 
  filter(SEASONS == 'Summer') %>% 
  pull(TEMPERATURE) %>% 
  mean(na.rm = TRUE)
#replacing NA with the mean value
replaceNAdf <- dropNA %>% 
  replace_na(list(TEMPERATURE = summer_df))
#saving the final dataSET
write_csv(replaceNAdf, 'seoul_bike_sharing.csv')

#working on categorical variables
#changing the type of hour variable to a a character
newdf <- replaceNAdf %>% 
  mutate(HOUR = as.character(HOUR))

# Convert SEASONS, HOLIDAY, FUNCTIONING_DAY, and HOUR columns into indicator columns
#starting with the SEASONS columns
newdf1 <- newdf %>% 
  mutate(dummy = 1) %>% 
  spread(
    key = SEASONS, #name of column to spread
    value = dummy,
    fill = 0
    
  )
#HOLIDAY
newdf2 <- newdf1 %>% 
  mutate(dummy = 1) %>% 
  spread(
    key = HOLIDAY, 
    value = dummy, 
    fill = 0
  )
#FUNCTIONING_DAY
newdf3 <- newdf2 %>% 
  mutate(dummy = 1) %>% 
  spread(
    key = FUNCTIONING_DAY,
    value = dummy,
    fill = 0
  )
#HOUR
newdf4 <- newdf3 %>% 
  mutate(dummy = 1) %>% 
  spread(
    key = HOUR,
    value = dummy,
    fill = 0
  )

write_csv(newdf4, 'seoul_bike_sharing_converted.csv')


#NORMALIZING OF THE DATA  USING MIN-MAX SCALE 
# `RENTED_BIKE_COUNT`, `TEMPERATURE`, `HUMIDITY`, `WIND_SPEED` ETC
#READ_BIKE_COUNT COLUMN
minmaxRBC <- (newdf4$RENTED_BIKE_COUNT - min(newdf4$RENTED_BIKE_COUNT))/
  (max(newdf4$RENTED_BIKE_COUNT)- min(newdf4$RENTED_BIKE_COUNT))
#assigning value to a dataframe 
RBCminmax <- newdf4 %>% 
  mutate(RENTED_BIKE_COUNT = minmaxRBC)

#TEMPERATURE
minmaxTEMP <- (newdf4$TEMPERATURE - min(newdf4$TEMPERATURE))/
  (max(newdf4$TEMPERATURE)- min(newdf4$TEMPERATURE))
TEMPminmax <- RBCminmax %>% 
  mutate(TEMPERATURE = minmaxTEMP)

#HUMIDITY
minmaxHum <- (newdf4$HUMIDITY - min(newdf4$HUMIDITY))/
  (max(newdf4$HUMIDITY)- min(newdf4$HUMIDITY))
HUMminmax <- TEMPminmax %>% 
  mutate(HUMIDITY = minmaxHum)

#WINDSPEED
ws <- (newdf4$WIND_SPEED - min(newdf4$WIND_SPEED))/
  (max(newdf4$WIND_SPEED)- min(newdf4$WIND_SPEED))
Wsminmax <- HUMminmax %>% 
  mutate(WIND_SPEED = ws)

#VISIBILITY
vs <- (newdf4$VISIBILITY - min(newdf4$VISIBILITY))/
  (max(newdf4$VISIBILITY)- min(newdf4$VISIBILITY))
vsminmax <- Wsminmax %>% 
  mutate(VISIBILITY = vs)

#DEW POINT TEMPERATURE
dpt <- (newdf4$DEW_POINT_TEMPERATURE - min(newdf4$DEW_POINT_TEMPERATURE))/
  (max(newdf4$DEW_POINT_TEMPERATURE)- min(newdf4$DEW_POINT_TEMPERATURE))
dptminmax <- vsminmax %>% 
  mutate(DEW_POINT_TEMPERATURE =dpt)

#SOLAR RADIATION  
sr <- (newdf4$SOLAR_RADIATION - min(newdf4$SOLAR_RADIATION))/
  (max(newdf4$SOLAR_RADIATION)- min(newdf4$SOLAR_RADIATION))
srminmax <- dptminmax %>% 
  mutate(SOLAR_RADIATION = sr)

#RAINFALL   
rf <- (newdf4$RAINFALL - min(newdf4$RAINFALL))/
  (max(newdf4$RAINFALL)- min(newdf4$RAINFALL))
rfminmax <- srminmax %>% 
  mutate(RAINFALL = rf)

#SNOWFALL   
sf <- (newdf4$SNOWFALL - min(newdf4$SNOWFALL))/
  (max(newdf4$SNOWFALL)- min(newdf4$SNOWFALL))
sfminmax <- rfminmax %>% 
  mutate(SNOWFALL = sf)

write_csv(sfminmax, 'seoul_bike_sharing_converted_normalized.csv')

#standardizing the columns of the new datasets
#Data Set list
dataset_list <- c('seoul_bike_sharing.csv', 
                  'seoul_bike_sharing_converted.csv', 
                  'seoul_bike_sharing_converted_normalized.csv')
for (dataset_name in dataset_list){
  # Read dataset
  dataset <- read_csv(dataset_name)
  names(dataset) <- toupper(names(dataset))
  # Replace any white space separators by underscore, using str_replace_all function
  names(dataset) <- str_replace_all(names(dataset), " ", "_")
  # Save the dataset back
  write.csv(dataset, dataset_name, row.names=FALSE)
}


#EDA WITH GGPLOT
#loading of the seoul_bike into a dataframe
sbsdf <- read_csv('seoul_bike_sharing.csv')

#casting the date as date
sbsdf$DATE <- dmy(sbsdf$DATE)

#changing hour into categorical variables
seoul_bike_sharing <- sbsdf %>% 
  mutate(HOUR= factor(HOUR, 
                      ordered = TRUE,
                      levels = c(0:23)))

#ensuring there are no missing values
sum(is.na(seoul_bike_sharing))  #no missing values detected

#description of the data set using the summary function
summary(seoul_bike_sharing)

#based on the stats, calculating how many holidays there are
Hc <- seoul_bike_sharing %>% 
  filter(HOLIDAY == 'Holiday') %>%   #408 holidays there are
  count()

#percentage of records that fall on a holiday
THc <- seoul_bike_sharing %>% 
  select(HOLIDAY) %>% 
  count()
Prc <- (Hc/THc) * 100 #percentage of holidays is 4.8198

#Grouping according to seasons and get the total rainfall and snowfall
TRFSF <- seoul_bike_sharing %>% 
  group_by(SEASONS) %>% 
  select(RAINFALL, SNOWFALL) %>% 
  summarise(Total_RainFall = sum(RAINFALL), Total_SnowFall = sum(SNOWFALL))

#functional day records
fcr <- seoul_bike_sharing %>% 
  filter(FUNCTIONING_DAY == 'Yes') %>%    #8465 records
  count()
  

#VISUALIZING THE DATA
#scatterplot of rented_bike vrs date
ggplot(seoul_bike_sharing, aes(x = DATE,
                               y = RENTED_BIKE_COUNT)) + 
  geom_point(alpha = 0.07) + 
  labs(x ='DATE', 
       y = 'RENTED_BIKE_COUNT',
       title = 'RENTED_BIKE_COUNT VRS DATE')

#plotting a time series of the bike count using hours as colors
ggplot(seoul_bike_sharing, aes(x = DATE,
                               y = RENTED_BIKE_COUNT,
                               color = HOUR)) + 
  geom_line() + 
  labs(x ='DATE', 
       y = 'Rented_Bike_Count',
       title = 'Rented_Bike_Count VRS DATE')

#creating a histogram overlaid with density
ggplot(seoul_bike_sharing, aes(x = RENTED_BIKE_COUNT)) + 
  geom_histogram(aes(y = ..density..), 
                 color = 'black',
                 fill = 'white') + 
  geom_density(color = 'red')

#Correlation between two variables(rented bike and temperature)
ggplot(seoul_bike_sharing, aes(x = TEMPERATURE,
                               y = RENTED_BIKE_COUNT, 
                               color = HOUR)) + 
  geom_point(alpha = 0.2) + 
  labs(x = 'Temperature',
       y = 'Rented_bike_count',
       title = 'Plot of Rented Bikes vrs Temperature') + 
  facet_wrap(~SEASONS) + 
  theme_minimal()

#box plot in outliers detection 
#using the rented bike count and hour grouped by seasons
ggplot(seoul_bike_sharing, aes(x = HOUR, 
                               y = RENTED_BIKE_COUNT)) + 
  geom_boxplot(alpha = 0.3) +
  facet_wrap(~SEASONS) + 
  theme_minimal()

#Daily total rainfall and snowfall
dtrs <- seoul_bike_sharing %>% 
  group_by(DATE) %>% 
  select(RAINFALL, SNOWFALL) %>% 
  summarize(Total_daily_rain = sum(RAINFALL), 
            Total_daily_snowfall = sum(SNOWFALL))
ggplot(dtrs, aes(x = DATE,
                 y = Total_daily_rain)) + 
  geom_bar(stat = 'identity',
          position = 'dodge') + 
  theme_minimal()

ggplot(dtrs, aes(x = DATE,
                 y = Total_daily_snowfall)) + 
  geom_bar(stat = 'identity',
           position = 'dodge') + 
  theme_minimal()
#How many days had snowfall
sfd <- seoul_bike_sharing %>% 
  filter(SNOWFALL != 0) %>% 
  count()    #ans: 443 days


#PERFORMING LINEAR REGRESSION ON THE DATASET
#FOCUSING ON THE NORMALIZED SEOUL BIKING DATA SET
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
spec(bike_sharing_df)

bike_sharing_df <- read_csv('seoul_bike_sharing_converted_normalized.csv')
#taking away the date and function day columns out 
bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)

#splitting of the data
set.seed(1234)
bike_data_split <- initial_split(bike_sharing_df, prop = 3/4)
train_data <- training(bike_data_split)
test_data <- testing(bike_data_split)

#defining a linear model specification
lm_spec <- linear_reg() %>%
  # Set engine
  set_engine(engine = "lm")
lm_spec

#fitting the model using the training data
train_fit <- lm_spec %>% 
  fit(RENTED_BIKE_COUNT ~ TEMPERATURE + 
        HUMIDITY + 
        WIND_SPEED + 
        VISIBILITY + 
        DEW_POINT_TEMPERATURE + 
        SOLAR_RADIATION + 
        RAINFALL + 
        SNOWFALL, data = train_data)
train_fit

#fitting another model called lm model weather
lm_model_weather <- lm_spec %>% 
  fit(RENTED_BIKE_COUNT ~ TEMPERATURE + 
        HUMIDITY + 
        WIND_SPEED + 
        VISIBILITY + 
        DEW_POINT_TEMPERATURE + 
        SOLAR_RADIATION + 
        RAINFALL + 
        SNOWFALL,  data = train_data)

#Building a linear regression using all variables
lm_model_all <- lm_spec %>% 
  fit(RENTED_BIKE_COUNT ~ ., data = train_data)
summary(lm_model_all)

#making predictions on the testing data for both models
#LM_MODEL_WEATHER
test_results_weather <- lm_model_weather %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

#LM_MODEL_ALL
test_results_all <- lm_model_all %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

#calculating the R square and RMSE
rsq_weather <- rsq(test_results_weather, truth = truth, estimate = .pred)
rsq_all <- rsq(test_results_all, truth = truth, estimate = .pred)

rmse_weather <- rmse(test_results_weather, truth = truth, estimate = .pred)
rmse_all <- rmse(test_results_all, truth = truth, estimate = .pred)

#Printing of all coefficients. A larger coeffcient means the variable contribute
#more to the predicting variable
lm_model_all$fit$coefficients
#sorting the coefficients in descending order and plotting a chart for the data
sorted_coefficients <- sort(abs(lm_model_all$fit$coefficients), decreasing = TRUE)
coefficients_df <- as.data.frame(sorted_coefficients)
newdf <- coefficients_df %>% 
  mutate(variable = rownames(coefficients_df),
         scf = sorted_coefficients)

sdf <- newdf %>% 
  select(variable, scf)

sdf$variable <- reorder(sdf$variable, sorted_coefficients, order = TRUE)

# Create plot
ggplot(sdf, aes(x = variable)) +
  geom_bar(aes(y = scf),
           stat = "identity" ) + 
  coord_flip()






