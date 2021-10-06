7##### Final Exam #####

# In this exam, you are given a data set to solve the following problems in R.

# Dataset: Bank Marketing Data
# Variables:
# 1. age (numberical)
# 2. job (employed, unemployed, non-labor)
# 3. marital (divorced, married, single)
# 4. education (basic, high.school, professional.course, university.degree, illiterate)
# 5. default: has credit in default? (yes, no)
# 6. housing: has housing loan? (yes, no)
# 7. loan: has personal loan? (yes, no)
# 8. contact: contact communication type (cellular, telephone)
# 9. month: last contact month of year (jan, feb, mar, ..., nov, dec)
# 10. day_of_week: last contact day of the week (mon, tue, wed, thu, fri)
# 11. duration: last contact duration, in seconds (numerical)
# 12. campaign: number of contacts performed during the campaign (numberical)
# 13. response: has the client subscribed a term deposit? (yes, no)
install.packages("MASS")
install.packages("tree")
install.packages("randomForest")
install.packages("ISLR")
# Once the package is installed, you can get access to the libraries within the package
library(MASS)
library(tree)
library(randomForest)
library(ISLR)
install.packages("dplyr")
install.packages("descr")
install.packages("plyr")
library("dplyr")
library("plyr")
library(randomForest)
library(tidyverse)
library(descr)
library(ggplot2)
##### Complete the following tasks (10 points)#####

# Remove all the existing variables in the global environment (1 point)
rm(list=ls(all=TRUE))

# Chech your current working directory (1 point)
getwd()

# Set the working directory for this test to your desktop (1 point)
setwd("~/Code/DS612_data_mining")

# Load the data set from the CSV file to R and attach it (1 point)
data <- read.csv("~/Code/DS612_data_mining/final_data.csv")
attach(data)
# Print the column names of the data frame (1 point)
colnames(data)

# Print the data frame dimension (1 point)
dim(data)
# Print the summary statistics for all the columns (1 point)
summary(data)

# What is the average age in this data set? (1 point)
mean(data$age)

# What is the proportion (%) of the observations subscribed a term deposit? (1 point)
CrossTable(data$X)

# What is the total number of observations belong to these categories? (1 point)
# education = high.school

count("high.school", "education")
#education = 7697
# marital = single
count("single", "marital")
#single = 9443
# job = non-labor
count("", "job")
#non-labor = 1826

##### Visualize the Data (10 points) #####

# Use a grey color scatter plot to show the correlation between age (x-asix) 
# and duration (y-asix), label the axes and give it a title  (2 points)
attach(data)
plot(age, duration, main = "Age x Duration", xlab = "age", ylab = "duration")
# Use a boxplot plot to show the correlation between response (x-asix) and 
# duration (y-axis), label the axes and give it a title  (2 points)
boxplot(response~duration,
        data = data,
        main = "Box plot",
        xlab="response",
        ylab="duration"
        )

# Use a bar chart to demonstrate which month has the most term deposit subscription (frequency)
# The plot does not need to be ordered by months. (2 points)
counts <- table(data$month)
barplot(counts)
#may
# Use a bar chart to demonstrate which day of the week has the most term deposit subscription (frequency)
# The plot do not need to be ordered by the day of the week. (2 points)
most_day <- table(data$day_of_week)
barplot(most_day)
#Thursday
# Calcuate the success rate for each education class (number of yes / number of yes + number of no)
# Use one of the R graphic tool to demonstrate the success rate in each education class. (2 points)




##### Predictive Model (50 points) #####

# Split the data into training and testing set with 70/30 ratio.  (10 points)
train <- sample(1:nrow(data), 0.75*nrow(data))
test <- -train
trainingData = data[train,]
testingData  = data[test,]


# Fitting the Logistic Regression to predict "response"
# You can choose any columns as the predictors.
# It would be wise to use some of the techniques we have learned in this class, such as ANOVA test
# Select two other statistical learning models of your choice to predict "response"
# Again, you should show the model selection process, 
# such as tuning parameters, predictor selection, and complexity level.
# Include the summary outputs and report the model fits (error rate).
# Hint: you can determine the threshold for the prediction from the logistic model.
# Document your steps and make sure your instructor can understand your code by reading your comments.
# Hint: month and day_of_week should be ignored in the modeling process.
# Hint: The final score depends on the model predictive performance and interpretability,
# make sure you select a model or it's specification with reasons to support.
#MSE variable
testing <- data$hage[test]
testing_outcome<-data$age[test]


bank_data <- as.data.frame(data)
class(bank_data)


# Summary output:
plot( age, predl)
summary(predl)
# Logistic Regression: (10 points)
# Training Misclassification Rate
mod_lm <- lm(age~education,data = testingData)
mod_lm.pred <- predict(mod_lm,trainingData)

mse <- mean((mod_lm.pred - testing_outcome)^2)
mse
#110.2792

# 3rd Model: (10 points)
bag.housing = randomForest(age~., data=trainingData, mtry=18, importance=TRUE)

predict.bag = predict(bag.housing, newdata=testingData, type = "class")

# Training Misclassification Rate
#calculate MCR
MCR = mean(predict.bag != testing)
MCR

names(bag.housing)
# Summary output
summary(bag.housing)

importance(bag.housing)

varImpPlot(bag.housing)

predict.bag = predict(bag.housing, newdata=testingData)

# Plot the prediction and testing outcome
plot(predict.bag,testing_outcome)
abline(0,1)

#MSE
MSE.bag = mean((predict.bag - testing_outcome )^2)
MSE.bag




# Calculate the testing error rate from each model. 
# Which model has the lowest testing error rate? (10 points)
#model1




##### Cross Validation (30 points)#####

# Calculate the following cross validation testing error rate from the logistic regression model
# Hint: Use the cv.glm() function and fit logistic model with the full data set.


# 2-Fold Cross Validation (5 points)


# 5-Fold Cross Validation (5 points)


# 10-Fold Cross Validation (5 points)


# 500-Fold Cross Validation  (5 points)
# Note: this may take a moment to run! You should break it if it takes longer than 5 minutes to run.



# Explain why you don't want to use the Leave-One-Out Cross Validation method?  (10 points)




#####  Final Model (50 points)  #####

# The final task is to make a prediction from your final model (20 points)
# based on the average value of the features.  
# Hint: For quantitative features, use the mean from the data.
# For qualitative features, choose the class with the highest frequency in the data.

# Model Prediction:



# Explain how this analysis can be useful in business decision. (30 points)
# What are the business insights we found in this study?

