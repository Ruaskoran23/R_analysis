#COVID-19 vaccine distribution
#loading of the data
library(tidyverse)
library(stringr)
library(lubridate)
library(leaflet)
library(corrplot)

ds  <- read_csv('COVID-19_Vaccine_distribution.csv')
head(ds, 10)
tail(ds,10)

#cleaning of data
#replacing whitespaces in column names
colnames(ds) <- str_replace_all(colnames(ds), ' ', '_')
colnames(ds) <- toupper(colnames(ds))
head(ds)

#checking the data types of all columns
str(ds)

#identifying missing values
map(ds, ~sum(is.na(.)))

#changing the data type & form of the week allocations column and 
date_reformated <- ds %>% 
  separate(WEEK_OF_ALLOCATIONS, sep = '/', into = c('Day', 'Month', 'Year'))

new_date_ref <- date_reformated %>%
  filter(Month > 12)
ndf <- new_date_ref %>% 
  filter(Month > 12) %>% 
  mutate(Day = Month,
         Month = 03)
other_data <- date_reformated %>% 
  filter(Month <= 12) 

cdf <- rbind(other_data, ndf)

wkdf <- cdf %>% 
  mutate(WEEKLY_ALLOCATION = paste(Day, Month, Year))
wkdf$Day <- as.numeric(wkdf$Day)
wkdf$Month <- as.numeric(wkdf$Month)
wkdf$Year <- as.numeric(wkdf$Year)
wkdf$WEEKLY_ALLOCATION <- dmy(wkdf$WEEKLY_ALLOCATION)
#writing it into a new dataframe
write_csv(wkdf, 'COVID_19_VAC_DIST.csv' )
#grouping of the jurisdiction columns as factors
new_file <- read_csv('COVID_19_VAC_DIST.csv')

#converting the jurisdiction variable as a factor
new_file$JURISDICTION <- factor(new_file$JURISDICTION)

#renaming columns with numbers
new_file <-new_file %>% 
  rename("DOSE_ALLOCATIONS" = "1ST_DOSE_ALLOCATIONS")
str(new_file)

##PERFORMING EDA
#descriptive statistics
summary(new_file)

#outlier detection using box plot
ggplot(new_file, aes(x = JURISDICTION,
                     y = DOSE_ALLOCATIONS)) + 
  geom_boxplot(fill = 'bisque', 
               color = 'black', 
               alpha = 0.3) + 
  labs(x = 'Area', 
       y = 'Dose Allocations') + 
  ggtitle('Dose Allocations by States') + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) 

#visualizing the data using bar chart in descending order 
new_file$JURISDICTION <- reorder(new_file$JURISDICTION,
                                 new_file$DOSE_ALLOCATIONS,
                                 order = TRUE)
ggplot(new_file, aes(x = JURISDICTION, 
                     y = DOSE_ALLOCATIONS, 
                     fil = 'black')) + 
  geom_bar(stat = 'identity', 
           position = 'dodge') + 
  labs(x = 'Area',
       y = 'First Dose Allocations') +
  theme_minimal() + 
  coord_flip() + 
  scale_fill_grey(start = 0.2, end = 1,aesthetics = 'fill')

#identifying the days to dose allocation relationship
dA <- new_file %>% 
  select(Day, DOSE_ALLOCATIONS) %>%
  group_by(Day) %>% 
  summarize(Total = sum(DOSE_ALLOCATIONS)) %>% 
  arrange(desc(Total))

#identifying monthly dose allocations
mA <- new_file %>%
  select(Month, DOSE_ALLOCATIONS) %>%
  group_by(Month) %>%
  summarize(Total = sum(DOSE_ALLOCATIONS)) %>% 
  arrange(desc(Total))

#visualizing the trend of weekly dose allocations
ggplot(new_file, aes(x = WEEKLY_ALLOCATION,
                     y = DOSE_ALLOCATIONS)) + 
  geom_area(linetype = 'solid',
            fill = 'bisque',
            color = 'black',
            alpha = 0.7) +
  theme_minimal()

#visualizing the data using a correlation chart
numeric_var <- new_file %>% 
  select(Day, Month, DOSE_ALLOCATIONS)
dt_correlation <- cor(numeric_var, method = 'pearson', use = 'pairwise.complete.obs')
col = colorRampPalette(c('#BB4444', '#EE9988', '#77AADD'))
corrplot(dt_correlation, method = 'color', type = 'upper',
         col = col(200), order = 'hclust', addCoef.col = 'black',
         tl.col = 'black', tl.srt = 45)


  
