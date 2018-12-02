# Purpose: Analyze Boston crime data
# Date: November 2018
# Team: 

# SOURCE LIBRARIES & FUNCTIONS
source('code/crime_library.R')
library(tidyverse)
library(ggplot2)

# A)
# IMPORT RAW DATA
neighborhood.list = c('allston','beacon-hill','back-bay','brighton','charlestown','dorchester',
                      'downtown','east-boston','hyde-park','jamaica-plain','mattapan','south-boston',
                        'roxbury','west-roxbury','roslindale','south-end','north-end','fenway')

crime.data.list <- foreach(neighborhood=neighborhood.list, .combine='rbind') %dopar% {
  filename <- paste0('https://www.universalhub.com/crime/', neighborhood, '.html')
  content <- get_site_content(filename)
  parsed_html<-content_to_parsed_html(content)
  tibble(crime = extract_all_crime_names(parsed_html),
         hour= extract_all_hours(parsed_html), 
         neighborhood = neighborhood)
}
crime.data.list

# Get mission-hill data separately since it does not has 'html', then combine it with the crime data.
mission_hill_type<- 'https://www.universalhub.com/crime/mission-hill' %>% get_site_content() %>% 
  content_to_parsed_html() %>% extract_all_crime_names()
mission_hill_hour<- 'https://www.universalhub.com/crime/mission-hill' %>% get_site_content() %>% 
  content_to_parsed_html() %>% extract_all_hours()
mission_hill<- tibble(crime = mission_hill_type,hour = mission_hill_hour,neighborhood='mission hill')

# Page 2 of Dorchester
dorchester_2_type <- 'https://www.universalhub.com/crime/dorchester.html?page=1' %>% get_site_content() %>% 
  content_to_parsed_html() %>% extract_all_crime_names()
dorchester_2_hour <- 'https://www.universalhub.com/crime/dorchester.html?page=1' %>% get_site_content() %>% 
  content_to_parsed_html() %>% extract_all_hours()
dorchester_2<- tibble(crime = dorchester_2_type,hour = dorchester_2_hour,neighborhood='dorchester')

# Combine all neighborhood data
crime.data <- rbind(crime.data.list, mission_hill, dorchester_2)



# B) Five most common crimes
# Correct all misspellings
crime.data$crime[crime.data$crime == "Shootin" |crime.data$crime == "shooting" |crime.data$crime == "Shootin"| crime.data$crime == "Shotting"] <- "Shooting" 
crime.data$crime <- as.factor(crime.data$crime)
# Count the occurence of the each crime and extract the 5 most common ones
common_5_count<- sort(table(crime.data$crime),decreasing =TRUE)[1:5]


# C) Total number of crimes by hour
crime.data$hour <- as.factor(crime.data$hour)
barplot(table(crime.data$hour), xlab="hour",main="Crimes by Hour")
###comment##
# Crimes mostly occured at night, between 9pm-12am

# D) 
# convert type back to character such that only the top 5 crimes will be kept in filter.
crime.data$crime <- as.character(crime.data$crime)
common_5_crimes<- crime.data %>% filter(crime %in% c("Shooting","Gunfire","Murder","Stabbing","Illegal gun possession"))
#ggplot(data = common_5_crimes, aes(x=hour, y=count_by_hour)) + geom_line(aes(colour=crime))
common_5_crimes_table<-as.data.frame(table(common_5_crimes$hour,common_5_crimes$crime))
colnames(common_5_crimes_table) <- c("hour","crime","Freq")
ggplot(common_5_crimes_table, aes(hour, Freq, group=crime, color=crime)) + 
  geom_line()
##comment##


# E)
crime_2 <-crime.data %>% filter(crime.data$neighborhood %in% c("dorchester","downtown"))
crime_2_table<-as.data.frame(table(crime_2$hour,crime_2$neighborhood))
colnames(crime_2_table) <- c("hour","neighborhood","Freq")
ggplot(crime_2_table, aes(hour, Freq, group=neighborhood, color=neighborhood)) + 
  geom_line()

##comment##
