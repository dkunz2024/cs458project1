#install.packages("tidyverse") #run once to install
library(tidyverse)

#get raw data from CSV
setwd("C:/Users/Denton/Documents/.School/cs458/project1")
dataAll <- read.csv("vehicles.csv")

#Pipe Operator: %>% ctrl+shift+m; result from previous line is first parameter in next function call

#construct our dataset
dataset <- dataAll %>% 
  select(make, model, UCity, UHighway, year, fuelCost08, trany) %>% 
  rename(fuelCost=fuelCost08, transmission=trany, cityMPG=UCity, hwyMPG=UHighway)

##examining datasets
#view(dataset)
#str(dataset)
#glimpse(dataset)
#head(dataset) #first 6 rows
#class(dataset$make) #datatype
#length(dataset) #number of vars/columns
#length(dataset$make) #number of observations
#names(dataset) #names of variables
#unique(dataset$make) #shows unique values in categorical vars

#get all of the rows with any missing information
missing <- !complete.cases(dataset)
missing.df <- dataset[missing,]

#change make, model, transmission into categorical variables
dataset$make <- as.factor(dataset$make)
dataset$model <- as.factor(dataset$model)
dataset$transmission <- as.factor(dataset$transmission)
#dataset$year <- as.factor(dataset$year)

##Other useful functions?
#levels(dataset$make)
#dataset %>% 
#  filter(hwyMPG > 50)
#  filter(order %in% c(...,...))
#mutate()?
#mean(dataset$hwyMPG, na.rm=T)
#distinct(dataset) #remove duplicates
#if_else(<condition>,<true_case>, <false_case>)

#install.packages("gapminder")
library(gapminder) #used for reshaping data, making pivot tables

wide_data <- dataset %>% 
  select(make, year, cityMPG) %>% 
  pivot_wider(names_from = year, values_from = cityMPG, values_fn = {mean})

#pivot_longer() to reverse ^^^

##range/spread :#min(), max(), range(), IQR()
##centrality: #mean(), median(), mode()
##Variance: #var()
#summary()

cityMPG_by_make <- dataset %>% 
  select(make, cityMPG) %>% 
  group_by(make) %>% 
  summarise(
    'Lower' = min(cityMPG), 
    'Average' = mean(cityMPG),
    'Upper' = max(cityMPG),
    'Difference' = max(cityMPG)-min(cityMPG))

##################
# VISUALIZATION  #
##################
#data, mapping, geometry
#color, shape, size

#Vehicle MPG and fuel cost by year
dataset %>% 
  select(fuelCost, cityMPG, year) %>% 
  filter(fuelCost>0) %>% 
  filter(cityMPG>0) %>% 
  ggplot(mapping = aes(x=fuelCost, y=cityMPG, color=year)) + 
  geom_point() +
  theme_minimal() +
  labs(title = "Vehicle MPG and fuel cost by year",
      x = "Fuel Cost",
      y = "City MPG")

#get the top 5 transmissions by count
transmission_count <- dataset %>%
  group_by(transmission) %>%
  summarise(count = n()) %>%
  top_n(n = 6, wt = count)
toptransmissions = as.vector(transmission_count[,1])
class(toptransmissions)

#Vehicle MPG and fuel cost by year and transmission type
dataset %>% 
  select(fuelCost, cityMPG, year, transmission) %>% 
  filter(fuelCost>0) %>% 
  filter(cityMPG>0) %>% 
  filter(transmission %in% c("Automatic (S6)", "Automatic (S8)", "Automatic 3-spd", "Automatic 4-spd", "Manual 5-spd", "Manual 6-spd")) %>% 
  ggplot(mapping = aes(x=fuelCost, y=cityMPG, color=year)) + 
  geom_point() +
  facet_wrap(~transmission) +
  theme_minimal() +
  labs(title = "Vehicle MPG and Fuel Cost by Year and Transmission Type",
       x = "Fuel Cost",
       y = "City MPG")

# dataset %>% 
#   mutate(top5 = if_else(as.character(transmission) == c("Automatic (A1)"), 1, 0)) %>% 
#   filter(top5 == 1) %>% 
#   view()
# 
# dataset %>% 
#   mutate(top5 = if_else(as.integer(transmission) %in% top5transmissions, 1, 0)) %>% 
#   filter(top5 == 1) %>% 
#   view()

#dataset[top5transmissions]







