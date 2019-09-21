## Business understanding

# Our example concerns a big company that wants to understand 
# why some of their best and most experienced employees are leaving prematurely. 
# The company also wishes to predict which valuable employees will leave next.


## Analytic solution 

# We have two goals: first, we want to understand why valuable employees leave, 
# and second, we want to predict who will leave next.
# 
# Therefore, we propose to work with the HR department to gather relevant data 
# about the employees and to communicate the significant effect that could explain 
# and predict employees' departure.

# For our 15 000 employees we know: 
#   
# satisfaction level
# latest evaluation (yearly)
# number of project worked/working on
# average monthly hours
# time spend in the company (in years)
# work accident (within the past 2 years)
# promotion within the past 5 years
# department
# salary

## Analytical Base Table

# This is the database from the HR department: 
# (Note that it doesn't take into account the person that have been fired, 
# transferred or hired in the past year...)
install.packages("DT")
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggvis)
library(corrplot)
library(DT)

setwd("D:\\R programming\\Techdata Notes\\R\\Logistic_Regression")

hr = read.csv('HR_comma_sep.csv')

head(hr)


### Summary

summary(hr)

## Data quality report

# This table describe the characteristics of each features. 
# We can see different statistical measures of central tendency and variation. 
# For example we can see that our attrition rate is equal to 24%, 
# the satisfaction level is around 62% and the performance average is around 71%. 
# We see that on average people work on 3 to 4 projects a year and about 200 hours per months.

## First visualisations

### Graph

# This graph present the correlations between each variables. 
# The size of the bubbles reveal the significance of the correlation, 
# while the colour present the direction (either positive or negative).

HR_correlation <- hr %>% 
  select(satisfaction_level:promotion_last_5years)

M <- cor(HR_correlation)
corrplot(M, method="circle")



# On average people who leave have a low satisfaction level

## Who is leaving?

# Let's create a data frame with only the people that have left the company, 
# so we can visualise what is the distribution of each features:

hr_hist <- hr %>% filter(left==1)

par(mfrow=c(1,3))
hist(hr_hist$satisfaction_level,col="#3090C7", main = "Satisfaction level") 
hist(hr_hist$last_evaluation,col="#3090C7", main = "Last evaluation")
hist(hr_hist$average_montly_hours,col="#3090C7", main = "Average montly hours")

# We can see why we don't want to retain everybody. 
# Some people don't work well as we can see from their evaluation, 
# but clearly there are also many good workers that leave.

par(mfrow=c(1,2))
hist(hr_hist$Work_accident,col="#3090C7", main = "Work accident")
plot(hr_hist$salary,col="#3090C7", main = "Salary")

# In the total of 15 000 employees that compose our database, 
# here are the people that have left:

hr_leaving_people <- hr %>% filter(left==1)
nrow(hr_leaving_people)

# More problematic, here are the total of employees that received an evaluation above average, 
# or spend at least four years in the company, 
# or were working/worked on more than 5 projects and still have left the company. 
# **These are the people company should have retained.**

hr_good_leaving_people <- hr_leaving_people %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
nrow(hr_good_leaving_people)

## Why good people leave?

# Let's re-use the data table created above that contain only the most valuable employees and see why they tend to leave.

hr_good_leaving_people <- hr %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
hr_good_people_select <- hr_good_leaving_people %>% select(satisfaction_level, number_project: promotion_last_5years)
M <- cor(hr_good_people_select)
corrplot(M, method="circle")

# Here it's much clearer. 
# On average valuable employees that leave are not satisfayed, 
# work on many projects, spend many hours in the company each month and aren't promoted.

summary(hr_good_leaving_people)

# Modeling 

# Now we want to predict which valuable employe will leave next.

## Select database

# Let's use the same database than above where we kept the most valuable employees. 
# Here is the summary of that database.

hr_model <- hr %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
summary(hr_model)
str(hr_model)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(hr_model, 
                            function(x) data.frame(model.matrix(~x-1,data =hr_model))[,-1]))

# Final dataset
hr_model_final<- cbind(hr_model[,-c(9:10)],dummies) 

############ Logistic Regression ###############

# train the model 
model_1 <- glm(left~., data=hr_model_final, family = "binomial")
summary(model_1)

############# Analysis of Model ############

# Z-Value

# The z-value is the regression coefficient divided by standard error.

# Deviance

# R reports two forms of deviance the null deviance and the residual deviance. 
# The null deviance shows how well the response variable is predicted by a model 
# that includes only the intercept (grand mean).

# Deviance is a measure of goodness of fit of a generalized linear model. 
# Or rather, its a measure of badness of fit, higher numbers indicate worse fit.

# AIC

# The Akaike Information Criterion (AIC) provides a method for assessing the quality of your model 
# through comparison of related models.  It’s based on the Deviance, but penalizes you for making the
# model more complicated.  Much like adjusted R-squared, it’s intent is to prevent you from including
# irrelevant predictors.

# If you have more than one similar candidate models (where all of the variables of the simpler model occur 
# in the more complex models), then you should select the 
# model that has the smallest AIC.


# Stepwise selection
#install.packages("MASS")
library("MASS")
model_2 <- stepAIC(model_1, direction="both")
summary(model_2)

# Removing multicollinearity through VIF check
library(car)
vif(model_2)

# As VIF values are not much high lets look at p-value & remove sales.xRandD

model_3 <- glm(formula = left ~ satisfaction_level + last_evaluation + number_project + 
                 average_montly_hours + time_spend_company + Work_accident + 
                 promotion_last_5years + sales.xhr + sales.xmanagement + sales.xproduct_mng + 
                 sales.xsupport + sales.xtechnical + salary.xlow + 
                 salary.xmedium, family = "binomial", data = hr_model_final)

summary(model_3)
vif(model_3)


# As VIF values are not much high lets look at p-value & remove sales.xproduct_mng 

model_4 <- glm(formula = left ~ satisfaction_level + last_evaluation + number_project + 
                 average_montly_hours + time_spend_company + Work_accident + 
                 promotion_last_5years + sales.xhr + sales.xmanagement + 
                 sales.xsupport + sales.xtechnical + salary.xlow + 
                 salary.xmedium, family = "binomial", data = hr_model_final)

summary(model_4)

vif(model_4)


# As VIF values are not much high lets look at p-value & remove sales.xmanagement 

model_5 <- glm(formula = left ~ satisfaction_level + last_evaluation + number_project + 
                 average_montly_hours + time_spend_company + Work_accident + 
                 promotion_last_5years + sales.xhr + 
                 sales.xsupport + sales.xtechnical + salary.xlow + 
                 salary.xmedium, family = "binomial", data = hr_model_final)

summary(model_5)

vif(model_5)

# As all the p-values look significant lets remove salary.xmedium and check AIC 

model_6 <- glm(formula = left ~ satisfaction_level + last_evaluation + number_project + 
                 average_montly_hours + time_spend_company + Work_accident + 
                 promotion_last_5years + sales.xhr + 
                 sales.xsupport + sales.xtechnical + salary.xlow
                 , family = "binomial", data = hr_model_final)


summary(model_6)
vif(model_6)

# It increases so we should not remove salary.xmedium

# make predictions

predictions<- predict(model_5,hr_model_final,type = 'response')
gmlmodelbinded <- cbind(hr_model_final,predictions)

gmlmodelbinded$predictions <- ifelse(gmlmodelbinded$predictions >= 0.50, 1, 0)


# summarize results
# install.packages("caret")
#install.packages("e1071")
library(caret)

gmlmodelbinded$left <- as.factor(gmlmodelbinded$left)
gmlmodelbinded$predictions <- as.factor(gmlmodelbinded$predictions)

confusionMatrix<- confusionMatrix(gmlmodelbinded$left,gmlmodelbinded$predictions)
confusionMatrix

# help: http://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/


# Actionable insights

# Here is a plot that show the probability to leave, of the employees and their performance. 
# We need to focus on the top right. To do that we build a data table were we rank the probability 
# to leave found in the logistic regression model and the performance, we therefore find the priority 
# for the company.

set.seed(100)

# Keep some data to test again the final model
inTraining <- createDataPartition(hr_model_final$left, p = .75, list = FALSE)
training <- hr_model_final[ inTraining,]
testing  <- hr_model_final[-inTraining,]

# Estimate the drivers of attrition
logreg = glm(formula = left ~ satisfaction_level + last_evaluation + number_project + 
               average_montly_hours + time_spend_company + Work_accident + 
               promotion_last_5years + sales.xhr + 
               sales.xsupport + sales.xtechnical + salary.xlow + 
               salary.xmedium, family = "binomial", data = training)


# Make predictions on the out-of-sample data
probaToLeave = predict(logreg,newdata=testing,type="response")

# Structure the prediction output in a table
predattrition = data.frame(probaToLeave)

# Add a column to the predattrition dataframe containing the performance
predattrition$performance = testing$last_evaluation
plot(predattrition$probaToLeave,predattrition$performance)


# Here we display the first 300 employees that the company should retain. 
# After grouping them per department we could email the different managers 
# to tell them which valuable employees might leave soon.

predattrition$priority=predattrition$performance*predattrition$probaToLeave

orderpredattrition=predattrition[order(predattrition$priority,decreasing = TRUE),]
orderpredattrition <- head(orderpredattrition, n=50)
datatable(orderpredattrition)

