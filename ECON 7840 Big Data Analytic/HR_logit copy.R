# Attributes:
#   1.	Satisfaction_level: Level of employee satisfaction
#   2.	Last_evaluation: Rating from last rating
#   3.	Number_project: Number of projects worked
#   4.	Average_monthly_hours: Monthly average hours worked
#   5.	Time_spend_company: Years of work in the company
#   6.	Work_accident: 0 - Did not have an accident at work and 1 - Had an accident at work
#   7.	Left: Dismissal indicator, 0 - Contracted and 1 - Off
#   8.	Promotion_last_5years: Promotion indicative, 0 - No and 1 - Yes
#   9.	Sales: Department
#   10.	Salary: Salary, Low, Medium and High



rm(list=ls()) # Clear environment
cat("\014")   # Clear Console
dev.off()     # Clear plots

setwd("~/Documents/AEcon/Summer term/Big Data/R")

# import data
library(readr)
HR <- read_csv("HR.csv")

# OLS linear reg
attach(HR)
m1 = lm(left~., data=HR)
summary(m1)

# Training 
a = seq(1,nrow(HR),1) 

set.seed(1)
ind   = sample(a, floor(nrow(HR)*0.8), replace=FALSE)
train = HR[ind,] # 11999
test  = HR[-ind,] # 3000

# Logistic regression Estimation, Generalized Linear Models
m2 = glm(left~satisfaction_level+average_montly_hours, #y=1 for off, y=0 for contracted
         data=train, 
         family=binomial(link='logit')) # logit can be replaced by probit
summary(m2) # AIC: 11325

m3 = glm(left~., data=train, family=binomial(link='logit'))
summary(m3) # AIC: 10336, smaller AIC the better the model, choose m3 > m2

# Prediction (Log-odds ratio)
# lambda(z)=e(z)/1+e(z) 
m3.logodd = predict(m3, newdata=test[,-7], type="link") # data without the left vars
# m4.logodd is the ez value

# P(LEAVE=1/.)
m3.prob   = predict(m3, newdata=test[,-7], type="response") #p1=p(leave=1)
z         = log(m3.prob/(1-m3.prob)) # z should = to m4.logodd

m3.class  = as.numeric(m3.logodd>=0) # Convert z-value into class (0,1)
# if logodd > 0 the point is above the hyperplane.

m3.right  = sum(test$left == m3.class) # actual == model prediction, if TURE gives you 1. 
# m3.right gives you the number of correct predictions

m3.acc    = mean(test$left == m3.class) # accuracy in the testing sample.
