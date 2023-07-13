library(httr)
library(rvest)


#getting of root node from the html page using read html
root_node <- read_html("https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country")
root_node
#getting the table node
table_node <- html_node(root_node, 'table')
table_node
#reading the table and converting it to a dataframe
table_data_frame <- html_table(table_node)
new_table <- as.data.frame(table_data_frame)
new_table

covid_csv_file <- download.file("https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0101EN-Coursera/v2/dataset/covid.csv", destfile="covid.csv")
covid_data_frame_csv <- read.csv("covid.csv", header=TRUE, sep=",")

#getting the data from row 5 to 10
my_data <- read.csv('covid.csv')
my_data[5:10,c("country", "confirmed")]

#getting the total confirmed cases, with 172 rows
confd <- my_data$confirmed
total_confirmed <- sum(confd, na.rm = FALSE)

#getting the total tested cases world wide
tested_cases <- my_data$tested
total_tested_cases <- sum(tested_cases, na.rm = TRUE)
total_tested_cases

#positive ratio of confirmed and tested
positive_ratio <- total_confirmed/total_tested_cases
positive_ratio

#country list
country_list <- my_data$country
country_list
converted_country <- as.character(country_list)
converted_country

#sorting of countries
country_sortedAZ <- sort(converted_country)
country_sortedAZ
country_sortedzA <- sort(converted_country, decreasing = TRUE)
country_sortedzA

#using regular expressions to find country names with united
utd <- regexpr('United.+', my_data$country)
utdmatch <- regmatches(my_data$country, utd)
utdmatch

#reviewing the testing data of two countries
#country number 20
rev_country_A <- my_data[20, c('country', 'tested', 'confirmed', 'confirmed.tested.ratio', 'tested.population.ratio')]
rev_country_A
#country number 120
rev_country_B <- my_data[120, c('country', 'tested', 'confirmed', 'confirmed.tested.ratio', 'tested.population.ratio' )]
rev_country_B

#comparing ratio of confirmed to population of the two countries above
#get the confirmed population ratio for the two countries
cfpr <- my_data$confirmed.population.ratio
nm <- my_data$country[120]
bh <- my_data$country[20]

if(cfpr[120] > cfpr[20]){ 
  print(nm)
  print('Has a greater ratio')
}else{ 
  print(bh)
  print('Has a less ratio')
}

#getting a subset of countries with confirmed to population ratio less than a threshold, lets say 1% = 0.01
cwcpr <- my_data[1:172, c('country', 'confirmed.population.ratio')]
cwcpr

#using the subset condition to subset
scwcpr <- subset(cwcpr, confirmed.population.ratio < 0.01)
scwcpr

#create a function to determine the confirmed ration after entering a country name
#get the confirmed population ratio for country
cfpr <- my_data$confirmed.population.ratio
cwcpr <- my_data[1:172, c('country', 'confirmed.population.ratio')]
cwcpr

cal_country <- function(countryname){
  ndf1 <- cwcpr[cwcpr$country == countryname, c('confirmed.population.ratio')]
  return(ndf1)
 
}




