library(tidyverse)
#the tar file has already been downloaded
#untar the file to get csv file
untar('lax_to_jfk.tar.gz', tar = 'internal')

#assign the csv file to subairline
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols(
                          'DivDistance' = col_number(),
                          'DivArrDelay' = col_number()
                        ))

#saving the resulting csv file
#write file contains the syntax write_csv(dataframe, file)
write_csv(sub_airline,'lax_to_jfk.csv')

sapply(sub_airline, typeof)

#group_by or summarize workflow
sub_airline %>% 
  group_by(Reporting_Airline) %>% 
  summarize(avg_carrier_delay = mean(CarrierDelay, na.rm = TRUE))

#USE SUBAIRLINE TO GET THE MEAN OF ARRDELAY FOR EACH REPORTING TIME
sub_airline %>% 
  group_by(Reporting_Airline) %>%
  summarize(avg_arrDelay = mean(ArrDelay, na.rm = TRUE, na.NaN = TRUE))

#DROPPING NA COLUMNS IN THE DATA SET
drop_na_cols <- sub_airline %>% select(-DivDistance, -DivArrDelay)

#DROPPING NA ROWS
drop_na_rows <- drop_na_cols %>% drop_na(CarrierDelay)

#REPLACING MISSING VALUES WITH NA IN SOME COLUMNS
replace_na <- drop_na_rows %>% replace_na(list(CarrierDelay = 0,
                                               WeatherDelay = 0, 
                                               NASDelay = 0, 
                                               SecurityDelay = 0,
                                               LateAircraftDelay = 0))
sub_airline %>% summarize_all(class) %>% gather(variable, class)

#FORMATTING THE FLIGHT DATE AS NUMERIC BY SEPARATING IT INTO DISTINCT COLUMNS

date_airline <- replace_na %>% separate(FlightDate, sep = '-', 
                                        into = c("year", "month", "day"))


#CHANGING THE DATA TYPE FROM CHARACTER TO NUMERIC IN FLIGHTDATE
date_airline %>% 
  select(year, month, day) %>%
  mutate_all(type.convert) %>%
  mutate_if(is.character, as.numeric)

#APPLYING NORMALIZATION TECHNIQUES
simple_scale <- sub_airline$ArrDelay / max(sub_airline$ArrDelay)
sim_DepD <- sub_airline$DepDelay / max(sub_airline$DepDelay)

minmax_scale <- (sub_airline$ArrDelay - min(sub_airline$ArrDelay)) / 
  (max(sub_airline$ArrDelay)-min(sub_airline$ArrDelay))
z_scale <- (sub_airline$ArrDelay - mean(sub_airline$ArrDelay)) / 
  sd(sub_airline$ArrDelay)

#BINNING AIRDELAY
binning <- sub_airline %>% 
  mutate(quartile_rank = ntile(sub_airline$ArrDelay, 4))

#ASSIGNING FLIGHT DELAY VALUES TO EACH FEATURE
sub_airline %>% 
  spread(Reporting_Airline, ArrDelay) %>% 
  slice(1:5)
                  
          #USING EXPLORATORY DATA ANALYSIS
#USING BOXPLOTS TO VISUALIZE THE FEATURES OF DISTRIBUTION
ggplot(data = sub_airline, mapping = aes(x = Reporting_Airline, y = ArrDelay)) +
  geom_boxplot(fill = "bisque",color = "black", alpha = 0.3) +
  geom_jitter(aes(color = 'blue'), alpha=0.2) +
  labs(x = "Airline") +
  ggtitle("Arrival Delays by Airline") +
  guides(color = FALSE) +
  theme_minimal() +
  coord_cartesian(ylim = quantile(sub_airline$ArrDelay, c(0, 0.99)))

#COMPARING TWO CONTINUOUS VARIABLES
alaska_flights <- sub_airline %>%
  filter(Reporting_Airline == "AS") %>%
  filter(!is.na(DepDelay) & !is.na(ArrDelay)) %>%
  filter(DepDelay < 40)
ggplot(data = alaska_flights, mapping = aes(x = DepDelay, y = ArrDelay)) +
  geom_point() +
  ggtitle("Alaska Flight Depature Delays vs Arrival Delays")

#FINDING THE CORRELATION BETWEEN DEPPDELAYMIN AND ARRDELAYMIN
cor(sub_airline$DepDelayMinutes, sub_airline$ArrDelayMinutes)
ggplot(data = sub_airline, mapping = aes(x = DepDelayMinutes, y = ArrDelayMinutes)) + 
  geom_point() + 
  geom_smooth(method = "lm" , na.rm = TRUE)

#FINDING IF WEATHERDEALAY IS A GOOD PREDICTOR OF ARRDELAY
cor(sub_airline$WeatherDelay, sub_airline$ArrDelayMinutes, use = "complete.obs") #gives NA as output
ggplot(data = sub_airline, mapping = aes(x = WeatherDelay, y = ArrDelayMinutes)) + 
geom_point() +
geom_smooth(method = "lm" , na.rm = TRUE) #weak correlation

#FINDING THE CORRELATION BETWEEN CARRIERDELAY AND ARRDELAYMINUTES
cor(sub_airline$CarrierDelay, sub_airline$ArrDelayMinutes, use = "complete.obs")
ggplot(data = sub_airline, mapping = aes(x = CarrierDelay, y = ArrDelayMinutes)) + 
  geom_point() +
  geom_smooth(method = "lm", na.rm = TRUE)

#DESCRIPTIVE STATISTICS WITH SUMMARIZE
summary_airline_delays <- sub_airline %>% 
  group_by(Reporting_Airline) %>% 
  summarize(count = n(),
            mean = mean(ArrDelayMinutes, na.rm = TRUE),
            std_dev = sd(ArrDelayMinutes, na.rm = TRUE),
            min = min(ArrDelayMinutes, na.rm = TRUE),
            max = max(ArrDelayMinutes, na.rm = TRUE),
            med = median(ArrDelayMinutes, na.rm = TRUE),
            iqr = IQR(ArrDelayMinutes, na.rm = TRUE)
            )
#counting the airlines
airline_count <- sub_airline %>% 
  count(Reporting_Airline)

#FINDING THE AVG DELAY OF FLIGHT AND HOW THEY DIFFER B/N REPORTING AIRLINE AND DAY OF THE WEEK
avg_delay <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>% 
  summarize(mean_delays = mean(ArrDelayMinutes), .groups = 'keep')
sorted <- avg_delay %>%     #sorting the variables in descending order
  arrange(desc(mean_delays))
#transform the sorted table into a heatmap
avg_delay %>% 
  ggplot(aes(x = Reporting_Airline,
             y = DayOfWeek,
             fill = mean_delays)) + 
  geom_tile(color = 'white', size = 0.2) + #set tile border to white
  scale_fill_gradient(low = 'yellow', high = 'red') #defines gradient color scales

#USING THE GROUP_BY, SUMMARIZE TO FIND THE AVERAGE DELAY OF EACH REPORTING AIRLINE
avg_delay_2 <- sub_airline %>%
  group_by(Reporting_Airline) %>% 
  summarize(meandelay2 = mean(ArrDelayMinutes), .groups = 'keep')
sorted_delay2 <- avg_delay_2 %>% 
  arrange(desc(meandelay2))
#
#EXPLORING CORRELATION AND CAUSATION
sub_airline %>%
  select(DepDelayMinutes, ArrDelayMinutes) %>%
  cor(method = "pearson")
#FINDING THE P-VALUE OR SIGNIFANCE
sub_airline %>%
  cor.test(~DepDelayMinutes + ArrDelayMinutes, data = .)

#CALCULATING THE CORRELATION OF MULTIPLE VARIABLES
correlation <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes,
         CarrierDelay, WeatherDelay, NASDelay, 
         SecurityDelay, LateAircraftDelay)
cor(correlation, use = "pairwise.complete.obs", method = "pearson")
correlation

#PLOTTING THE CORRELATION OF ALL VARIABLES
library(corrplot)
numerics_airline <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, CarrierDelay,
         WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay)
airlines_cor <- cor(numerics_airline, method = "pearson", use='pairwise.complete')
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(airlines_cor, method = "color", col = col(200),
         type = "upper", order = "hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 45, #Text label color and rotation
)

#ANALYSIS OF VARIANCE
summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(Average_Delays = mean(ArrDelayMinutes, na.rm = TRUE))
#plotting a bar chart
summary_airline_delays %>%
  ggplot(aes(x = Reporting_Airline, y = Average_Delays)) +
  geom_bar(stat = "identity") +
  ggtitle("Average Arrival Delays by Airline")

#analysing AA(american Airline) and As(alaska airline)
aa_as_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'AS')
ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_as_subset)
summary(ad_aov)

                 #MODEL DEVELOPMENT WITH TIDYVERSE
#definig the data set with AA flight delays
aa_delay <- sub_airline %>% 
  filter(Reporting_Airline == 'AA', CarrierDelay != "NA")
#using a simple linear regression to predict arrdelay
linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delay)
summary(linear_model)
#input data for prediction
new_dep_delay <- data.frame(DepDelayMinutes = c(12, 19, 24))
#predict the new arrdelayminutes using the predict() function
Pred_val <- predict(linear_model, newdata = new_dep_delay, interval = 'confidence')
Pred_val 

#creating a linear model with carrierdelay as the predictor variable
# and Arrdelay as the response varibale with AA as the reporting variable
linearModel <- lm(ArrDelayMinutes ~ CarrierDelay, data = aa_delay)
summary(linearModel)
newvalPred <- data.frame(CarrierDelay = c(12, 19, 24))
PreVal <- predict(linearModel, newdata = newvalPred, interval = 'confidence')
PreVal

#predicting using multiple linear regression
#predicting ArrDelay uisng other good factors such as AircraftDelay and DepDelay
#mlr is multiple linear regression
mlr <- lm(ArrDelayMinutes ~ DepDelayMinutes + 
            LateAircraftDelay, 
          data = aa_delay)
summary(mlr)

#creating mlr2 to predict Arrdelay with DepDelay and other 2
mlr2 <- lm(ArrDelayMinutes ~ DepDelayMinutes + 
             LateAircraftDelay + 
             CarrierDelay, 
           data = aa_delay)
summary(mlr2)
 
        #ASSESSING MODELS VISUALLY
ggplot(aa_delay, aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")
#creating a regression plot of carrier delay and Arrdelay
ggplot(aa_delay, 
       aes( x= CarrierDelay, y = ArrDelayMinutes)) + 
  geom_point() + 
  stat_smooth(method = 'lm', col = "yellow")

#between DepDelay and CarrierDelay which has a stronger correlation ArrDelay
#using the cor() function
cor(aa_delay$DepDelay, aa_delay$ArrDelayMinutes) 
#complete.obs removes NA and work on numericals
cor(aa_delay$CarrierDelay, aa_delay$ArrDelayMinutes, use = "complete.obs")

#viewing residual plot and other plots
plot(linear_model)

#polynomials regressions second order 
time <- 6:19
temp <- c(4,6,7,9,10,11,11.5,12,12,11.5,11,10,9,8)
ggplot(data = NULL, aes(time, temp)) +
  geom_point()
polyfit2 <- lm(temp ~ poly(time, 2, raw = TRUE))
summary(polyfit2)
ggplot(data = NULL, aes(time, temp)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2))

#4th order polynomial regression using same data
polyfit4 <- lm(temp ~ poly(time, 4, raw = TRUE))
summary(polyfit4)

#assessing models with R-square and MSE 
linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, aa_delay)
mse <- mean(linear_model$residuals^2)
mse
rmse <- sqrt(mse)
rmse
summary(linear_model)$r.squared

mlr <- lm(ArrDelayMinutes ~ DepDelayMinutes + LateAircraftDelay, data = aa_delay)
mse_mlr <- mean(mlr$residuals^2)
mse_mlr
rmse_mlr <- sqrt(mse_mlr)
rmse_mlr

      #MODEL EVALUATION
#TRAINING AND TESTING YOUR MODEL
flight_delays <- sub_airline %>%
  replace_na(list(CarrierDelay = 0,
                  WeatherDelay = 0,
                  NASDelay = 0,
                  SecurityDelay = 0,
                  LateAircraftDelay = 0)) %>%
  select(c(ArrDelayMinutes, DepDelayMinutes, CarrierDelay, WeatherDelay, NASDelay))
#set a random seed so that anytime the code run, the data splitted will be the same
set.seed(1234)
flight_split <- initial_split(flight_delays)
train_data <- training(flight_split)
test_data <- testing(flight_split)

#after splitting a linear regression object is created
lm_spec <- linear_reg() %>%
  # Set engine
  set_engine(engine = "lm")
lm_spec
#train the model using train_data
train_fit <- lm_spec %>%
  fit(ArrDelayMinutes ~ DepDelayMinutes, data = train_data)
train_fit

#to look at some of the predicted models
train_results <- train_fit %>%
  # Make the predictions and save the predicted values
  predict(new_data = train_data) %>%
  # Create a new column to save the true values
  mutate(truth = train_data$ArrDelayMinutes)
head(train_results)

#evaluating test
test_results <- train_fit %>%
  # Make the predictions and save the predicted values
  predict(new_data = test_data) %>%
  # Create a new column to save the true values
  mutate(truth = test_data$ArrDelayMinutes)
head(test_results)

#Evaluating the model using RMSE
rmse(train_results, truth = truth,
     estimate = .pred)
rmse(test_results, truth = truth,
     estimate = .pred)
#calculating the R-squared on the train and test data
rsq(train_results, truth = truth,
    estimate = .pred)
rsq(test_results, truth = truth,
    estimate = .pred)

#performing cross validation to solve overfitting models
set.seed(1234)
cv_folds <- vfold_cv(train_data, v = 10) #v =10 means 10 folds
results <- fit_resamples(lm_spec,
                         ArrDelayMinutes ~ DepDelayMinutes,
                         resamples = cv_folds)
results %>% collect_metrics()




















