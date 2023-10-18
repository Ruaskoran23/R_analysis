#COVID19 DEATH
#loading of data
library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)

cdd <- read_csv('Provisional_COVID-19_Deaths.csv')
str(cdd)
#data preprocessing
map(cdd, ~sum(is.na(.))) #identification of NA in all columns


#repalcing NA with zero for some selected columns
cddc <- cdd %>% 
  replace_na(list('COVID-19 Deaths' = 0, 
                        'Total Deaths' = 0, 
                        'Pneumonia Deaths' = 0, 
                        'Pneumonia and COVID-19 Deaths' = 0, 
                        'Influenza Deaths' = 0, 
                        'Pneumonia, Influenza, or COVID-19 Deaths' = 0))

colnames(cddc) <- str_replace_all(colnames(cddc), ' ', '_')
colnames(cddc) <- str_replace_all(colnames(cddc), '-', '_')

#EXPLORATORY DATA ANALYSIS
#VISUALIZING DATA USING HISTOGRAM
ncddc <- cddc %>% 
  filter(State != 'United States', Sex != 'All Sexes', `Age_Group` != 'All Ages') %>% 
  select(!Group)

ncddc$State <- as.character(ncddc$State)

#DEATHS  BASED ON COVID 19 

color <- c('#808080', '#00FF00')
color1 <- c('#445d48', '#d6dd99')
color2 <- c('#808080', '#d6dd99')
color3 <- c('#445d48', '#00ff00')
color4 <- c('#d6dd99', '#00ff00')

#total covid-19 deaths based on each state

ndf <- ncddc %>% 
  group_by(State, Sex) %>%
  select(State, COVID_19_Deaths, Sex) %>% 
  summarise(Total_Deaths = sum(COVID_19_Deaths), .groups = 'keep' )

  ggplot(ndf, aes(x = reorder(ndf$State, ndf$Total_Deaths),
                  y = Total_Deaths,
                  fill = Sex
                  )) +
  geom_bar(stat = 'identity', 
           position = 'stack') + 
  theme_minimal() +
  labs(x = 'State',
       y = 'Number of Deaths',
       title = 'Deaths Based on Covid-19') +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = color) +
  coord_flip()
  
  #average total covid19 deaths
ndf1 <-  ncddc %>% 
  group_by(State, Sex) %>%
  select(State, COVID_19_Deaths, Sex) %>% 
  summarise(Avg_Deaths = mean(COVID_19_Deaths), .groups = 'keep' )

ggplot(ndf1, aes(x = reorder(ndf1$State, -ndf1$Avg_Deaths),
                 y = Avg_Deaths, 
                 fill = Sex)) + 
  geom_col(stat = 'identity',
           position = position_dodge()) + 
  theme_minimal() + 
  labs(x = 'State', 
       y = 'Average number of deaths') + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_fill_manual(values = color1)
    
#total number of deaths based on age groups
ndf2 <- ncddc %>% 
  group_by(Age_Group, Sex) %>%
  select(Age_Group, Sex, COVID_19_Deaths) %>% 
  summarise(Total = sum(COVID_19_Deaths), .groups = 'keep')

ggplot(ndf2, aes(x = reorder(ndf2$Age_Group, -ndf2$Total),
                 y = Total,
                 fill = Sex)) + 
  geom_bar(stat = 'identity', 
           position = 'stack') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = 'Age Group', 
       y = 'Number of Deaths',
       title = 'Covid-19 Deaths') + 
  scale_fill_manual(values = color2)

#deaths based on other illness
#pneumonia deaths

ndf3 <- ncddc %>% 
  group_by(Sex) %>%
  select(Sex, Pneumonia_Deaths) %>%
  summarise(Total_deaths = sum(Pneumonia_Deaths))

ggplot(ndf3, aes(x = ' ',
                 y = Total_deaths,
                 fill = Sex)) + 
  geom_bar(stat = 'identity', color = 'black') + 
  geom_text(aes(label = Total_deaths), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = 'y') +
  labs(title = 'Gender based Pneumonia deaths') +
  theme_void() + 
  scale_fill_manual(values = color3) + 
  theme(legend.position = 'bottom')

#Influenza deaths based on sex

Idf <- ncddc %>% 
  group_by(Sex) %>% 
  select(Sex, Influenza_Deaths) %>% 
  summarise(Total = sum(Influenza_Deaths))

ggplot(Idf, aes(x = ' ',
                y = Total, 
                fill = Sex)) + 
  geom_bar(stat = 'identity', color = 'black') + 
  coord_polar(theta = 'y') + 
  geom_text(aes(label = Total), 
            position = position_stack(vjust = 0.5)) + 
  theme_void() + 
  labs(title = 'Influenza Deaths based on Gender') + 
  scale_fill_manual(values = color4) 

#covid deaths based on gender
cdf <- ncddc %>% 
  group_by(Sex) %>% 
  select(Sex, COVID_19_Deaths) %>% 
  summarise(Total = sum(COVID_19_Deaths))

ggplot(cdf, aes(x = ' ',
                y = Total,
                fill = Sex)) + 
  geom_bar(stat = 'identity',
           color = 'black') + 
  coord_polar(theta = 'y') + 
  geom_text(aes(label = Total), 
            position = position_stack(vjust = 0.5)) + #adjust the text position
  theme_void() + 
  labs(title = 'COVID-19 Deaths in the States') + 
  scale_fill_manual(values = color1)

#casting the character dates as date formats
ncddc$Start_Date <- mdy(ncddc$Start_Date)
ncddc$End_Date <- mdy(ncddc$End_Date)

#determining the intensity of covid-19 through the year
cdfI <- ncddc %>% 
  group_by(Start_Date) %>% 
  select(Start_Date, COVID_19_Deaths) %>% 
  summarise(total = sum(COVID_19_Deaths)) 

#plotting an area chart to visualize the data
ggplot(cdfI, aes(x = Start_Date, 
                 y = total)) +
  geom_area(stat = 'identity', 
            position = 'stack',
            color = 'black',
            fill = '#d6dd99') + 
  theme_minimal() + 
  scale_x_date(breaks = '2 months', date_labels = '%b %y') + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = 'Date',
       y = 'Total Deaths',
       title = 'COVID-19 Deaths in the States since 2020')

