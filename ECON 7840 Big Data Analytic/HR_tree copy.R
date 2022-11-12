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

# summary
library(psych)
vars <- c('satisfaction_level', 'last_evaluation', 'number_project', 'average_montly_hours', 'time_spend_company',  'Work_accident', 'left', 'promotion_last_5years', 'sales', 'salary')
describe(HR[vars], fast = T)


### Data prep & Data understanding

unique(HR$sales)
table(HR$sales) # there are in total 10 department in the company

# pie chart for department
department <- c(unique(HR$sales))
dep_number <- c(767, 739, 1227, 630, 858, 902, 787, 4140, 2229, 2720)
pie(dep_number, labels = department, main = "department pie chart", 
    col = rainbow(length(dep_number)), cex = 1, cex.main = 1, radius = 2)

# 
attach(HR)
class(HR$left)
(left.freq  <- table(left)) # turnover in each department
(freq    <- table(sales,by=left)) # sales distribution by left

### Tree model

# Set 80% training and 20% testing data
set.seed(1) # to sync what we do right now
a <- seq(1,nrow(HR),by=1) # 1,2...,14999, create a sequence
i <- sample(a,nrow(HR)*0.8, replace=FALSE) #11999, index numbers
train <- HR[i,] # For building a tree model
test  <- HR[-i,] # Assume 4000 are new, verify the model

n1 <- sum(train$left == 0) 
n0 <- nrow(train)-n1
# Within train samples, 9149 are "contracted", 2850 are "off". Data is not very balanced but similar prop with the population data.

library(C50)
vars <- c('satisfaction_level', 'last_evaluation', 'number_project', 'average_montly_hours', 'time_spend_company',  'Work_accident', 'promotion_last_5years', 'sales', 'salary')

# Model 1
# Estimation (Supervised)
mod1 <- C5.0(x=train[,vars], y=as.factor(train$left),      # C5.0 models require a factor outcome
             control=C5.0Control(minCases=100)) # size of leaf/terminal node
mod1
out1 <- summary(mod1)
out1

# Plotting
library(partykit)
plot(mod1, 
     main="Classification Tree (Model 1)", 
     type="extended", # type="simple"
     gp = gpar(fontsize=8))
# if working more than 287 hours per month employee will for sure leave!

# Prediction (Testing data = 3000)
predict1 <- predict(mod1, newdata = test[,vars], type="class") # no need to include LEAVE 
predict1
acc1     <- mean(test$left == predict1) # 0.967666....

# Prediction (Training data)
predict0 <- predict(mod1, newdata = train[,vars], type="class")
acc0     <- mean(train$left==predict0) # 0.9679....
# Performance is slightly better in training data. 

# Feature Importance 
C5imp(mod1, metric="usage", pct=TRUE) # average_montly_hours = 100%
C5imp(mod1, metric="splits")

# Model 2
# Estimation
mod2 <- C5.0(x=train[,vars], y=as.factor(train$left),
             control=C5.0Control(minCases=500)) # more stringent,tree smaller 
summary(mod2) # 100.00%	satisfaction_level

# Plotting
plot(mod2, 
     main="Classification Tree (Model 2)", 
     type="extended",
     gp = gpar(fontsize = 8))

# Prediction
predict2 <- predict(mod2, newdata = test[,vars], type="class")
acc2     <- mean(test$left==predict2) # 0.9453

# Feature Importance
C5imp(mod2,metric="usage",pct=TRUE)
# when using different tree size the root is different, affect company's decision.