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
  select(make, cityMPG, year) %>% 
  group_by(make) %>% 
  summarise(
    count = n(),
    'Average Year' = floor(mean(year)),
    'Lower' = min(cityMPG), 
    'Average' = mean(cityMPG),
    'Upper' = max(cityMPG),
    'Difference' = max(cityMPG)-min(cityMPG)) %>%
  filter(count >= 5) %>% 
  slice_max(n=6, order_by=Average)

##################
# VISUALIZATION  #
##################
#data, mapping, geometry
#color, shape, size

#get the top 5 transmissions by count
transmission_count <- dataset %>%
  group_by(transmission) %>%
  summarise(count = n()) %>%
  top_n(n = 5, wt = count)

transmission_count <- transmission_count %>% 
  add_row(transmission = "Other", count = sum(!dataset$transmission %in% transmission_count$transmission))

#Vehicle MPG and fuel cost by year and transmission type
dataset %>% 
  select(fuelCost, cityMPG, year, transmission) %>% 
  filter(fuelCost>0) %>% 
  filter(cityMPG>0) %>% 
  mutate(transmission = if_else(transmission %in% transmission_count$transmission, transmission, "Other")) %>% 
  ggplot(mapping = aes(x=fuelCost, y=cityMPG, color=year)) + 
  geom_point() +
  facet_wrap(~transmission) +
  theme_minimal() +
  labs(title = "Vehicle MPG and Fuel Cost by Year and Transmission Type",
       x = "Fuel Cost",
       y = "City MPG",
       color = "Year")

dataset %>% 
  select(cityMPG, make, transmission) %>% 
  filter(cityMPG>0) %>% 
  filter(make %in% cityMPG_by_make$make) %>%
  ggplot(mapping = aes(x=cityMPG, y=reorder(make, cityMPG))) + 
  geom_boxplot() +
  labs(title = "Top 6 Vehicle Makes for City MPG",
       x = "City MPG",
       y = "")
