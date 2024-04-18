pacman::p_load(pacman, dplyr)

setwd("C:/Users/Denton/Documents/.School/cs458/project1")
dataAll <- read.csv("vehicles.csv")
dataset <- select(dataAll, fuelCost08, make, model, trany, UCity, UHighway, year)
dataset <- rename(dataset, fuelCost=fuelCost08, transmission=trany, cityMPG=UCity, hwyMPG=UHighway)

plot(dataset$fuelCost)
