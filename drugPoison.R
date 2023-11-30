library(tidyverse)
library(stringr)
library(lubridate)

#FUNCTION 1 IS TO CLEAN THE DATA
 dataset1 <- read_csv('NCHS_DRUGPOISONING.csv')
 colnames(dataset1) <- str_replace_all(colnames(dataset1), " ", "_")
 colnames(dataset1) <- str_replace_all(colnames(dataset1), "-", "_")
 map(dataset1, ~sum(is.na(.)))
 
 fnc1 <- function(){
   for(dataset in dataset1){
     df1 <- dataset1 %>% 
       select(State, Sex, Year, Age_Group,Race_and_Hispanic_Origin, Deaths, Population,
              Crude_Death_Rate, US_Crude_Rate) %>% 
       filter_all(all_vars(State == "United States" & 
                             Sex != "Both Sexes" &
                             Age_Group != "All Ages" & 
                             Race_and_Hispanic_Origin != "All Races-All Origins"))
              
     write.csv(df1, 'dataset2.csv', row.names = FALSE)
   }
  return(df1)
 }
 fnc1()
 
 
##FUNCTION 2 IS TO EXPLORE THE DATA USING BOXPLOT AND SCATTERPLOTS
 #loading of dataset2 and using the data exploring the data
 
 dataset2 <- read_csv("dataset2.csv")
 
fnc2 <- function(var1, var2){
  #first determining if the variables are in the dataframe
  if(!(var1 %in% names(dataset2) && var2 %in% names(dataset2))){
    stop("COlumn names are not in data frame.")
  }
  color <- c('#ff5b22','#f1eb90')
  #boxplot for numerical categorical variables
  bxplot <- ggplot(dataset2, 
                   aes(x = !!as.symbol(var1),
                       y = !!as.symbol(var2),
                       fill = Sex)) + 
    geom_boxplot() +
    theme_minimal() + 
    labs(title = paste("Plot of", var1,"distribution" )) + 
    scale_fill_manual(values = color)
  
  #scatter plot of numerical variables
  sctplot <- ggplot(dataset2, 
                    aes(x = !!as.symbol(var1),
                        y = !!as.symbol(var2),
                        color = Sex)) + 
    geom_point() + 
    theme_minimal() +
    labs(title = paste('Plot of ', var1, "vrs", var2))
  
  return(list(box_plot = bxplot, sct_plot = sctplot))
}
plots <- fnc2('Race_and_Hispanic_Origin', 'Deaths')

#viewing of plots
print(plots$box_plot)

##FUNCTION 3 IS TO CORRECT OUTLIERS
#from function 3, the variables with outliers in relation to deaths are
#Age_Group and Population

fnc3 <- function(columns, threshold = 1.5){
  dataset2 %>% 
    filter(across(all_of(columns), 
                  ~between(., 
                           quantile(., 0.25) - threshold * IQR(.), 
                           quantile(., 0.75) + threshold * IQR(.)
                           )))
}

#filtered data set
filtered_data <- fnc3(c('Deaths', 'Population'))
write.csv(filtered_data, 'clean_data.csv', row.names = FALSE)

ggplot(filtered_data, aes(x = Race_and_Hispanic_Origin,
                          y = Deaths,
                          fill = Sex)) + 
  geom_boxplot() + 
  theme_minimal() + 
  scale_fill_manual(values = color) + 
  ylim(c(0,10000))


#analyzing the dataset
##exploring the population versus race and hispanic origin
ggplot(filtered_data) + 
  geom_point(mapping = aes(Population, Deaths),
             filter(filtered_data, Race_and_Hispanic_Origin == 'Non-Hispanic Black'), 
             color = '#b31312', alpha = 0.7, size = 4) + 
  geom_point(aes(Population, Deaths),
             filter(filtered_data, Race_and_Hispanic_Origin != 'Non-Hispanic Black'),
             color = 'grey45', shape = 1, size = 2) + 
  labs(title = 'Plot of Drug Poisoning in Non Hispanic Blacks vrs Other Races') + 
  theme_minimal()

#exploring the deaths count among age groups
ag <- filtered_data %>% 
  select(Age_Group, Deaths, Sex) %>% 
  group_by(Age_Group,Sex) %>%
  summarize(Total = sum(Deaths), .groups = 'keep') %>% 
  arrange(desc(Total))
ag$Age_Group <- reorder(ag$Age_Group,
                        ag$Total, 
                        order = TRUE)
#plotting the graph of the data
ggplot(ag, 
       aes(x = Age_Group,
           y = Total,
           fill = Sex)) + 
  geom_bar(stat = 'identity',
           position = 'stack') + 
  theme_minimal() + 
  scale_fill_manual(values = color) +
  labs(y = 'Deaths')
  




