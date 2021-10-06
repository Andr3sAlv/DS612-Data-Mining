library(readxl)
install.packages("dplyr")
library("dplyr")
# First of all, if you have not install the package "ISLR" on your machine yet, you will need to run the following command.
install.packages("ISLR")

# Once the package is installed, you can get access to the libraries within the package
library(MASS)
library(ISLR)

AA_house_data <- read.csv("~/Downloads/kc_house_data.csv")
//nice this is my vim practice

#Giving a summary of each column of data,
summary(AA_house_data)
#Replace NA's from dataset, There was one found in waterfront

print(AA_house_data[8])
which(is.na(AA_house_data[8]))
print(AA_house_data[1086, 8])
AA_house_data[1086, 8] <- 0
summary(AA_house_data)
#removed several columns that aren't relevant towards the project
cleaned_data = select(AA_house_data,-grade,-sqft_living15,-long,-lat,-waterfront,-view,-sqft_above,-sqft_living)
summary(cleaned_data)
#Plotted the data to find relevant relations between price and yr_built
plot(cleaned_data$price,cleaned_data$yr_built)
#Plotted data to see relevance of zipcode to price
plot(cleaned_data$price,cleaned_data$zipcode)


