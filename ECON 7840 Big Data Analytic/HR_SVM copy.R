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

library(e1071)

##########################################
## e1071 contains svm()
## y is the target
## ~ separate target and features
## . means all features are included
## kernel can be linear, polynomial, radial basis, sigmoid
## cost is the cost parameter
## When cost is small, margin will be wide (opposite to the argument in the note)
## and more support vectors.
## scale=FALSE tells the svm() function NOT to standardize the features to N(0,1)
##########################################

##########################################
# Huge cost parameter
# cost = penalty factor on the misclassified obs
# ***Huge cost -> Hate errors -> Margin narrower -> No. of SV decreases
##########################################
svm10 = svm(y~. , data=dat , kernel ="linear", 
            cost=10, scale=FALSE )
plot(svm10, dat, 
     svSymbol=16,  # 16 stands for solid circle
     dataSymbol=1, # Check "pch in R" in Google
     color.palette = terrain.colors)
mtext("SVM - Cost=10", side=3)

##########################################
# solid points are support vectors
##########################################
svm10$index
summary(svm10)

##########################################
# Small cost parameter
# Small cost -> Welcome errors -> Margin wider -> More SV
##########################################
svm1 =svm(y~., data=dat , kernel ="linear", # this is the classifier not the machine
          cost=0.1, scale =FALSE ) # less stringent, more sv
svm1$index
svm1$SV
nrow(svm1$SV)

plot(svm1, dat, 
     svSymbol=16, dataSymbol=1,
     color.palette = terrain.colors)
mtext("SVM - Cost=0.1", side=3)
svm1$index
summary(svm1)

##########################################
# Testing Jun 6 [2:51]
##########################################
xtest = matrix(rnorm(20*2), ncol=2) # create hypothetical data
ytest = sample(c(-1,1), 20, replace=TRUE) # Actual
xtest[ytest==1,] = xtest[ytest==1,] + 3 # we do the same thing, so the distrbution follows the training data
colnames(xtest) = c("z1","z2")

testdat = data.frame(x=xtest, y=as.factor(ytest))

ypred1 = predict(svm1, newdata=testdat)
acc.svm1 = mean(testdat$y == ypred1) # as a proportion

table(ypred1, testdat$y) # confusion matrix
table(predict=ypred1, truth=testdat$y)

ypred10 = predict(svm10, newdata=testdat)
table(predict=ypred10, truth= testdat$y)

##########################################
# Cross-Validation Jun 7 [21:00]
##########################################
set.seed(1)
tune.out=tune(svm , # type of models,(not the model name: svm1 nor svm10)
              y~.,
              data=dat ,
              kernel ="linear",
              ranges =list(cost=c(0.001 , 0.01, 0.1, 5,10,100) ))
summary(tune.out) # The optimal cost parameter is 0.1, there are many attributes in this function

bestmod = tune.out$best.model 
summary(bestmod)

tune.out$best.performance  # objective func value

tune.out$sampling

ypred = predict(bestmod, testdat)
table(predict=ypred , truth= testdat$y)

##########################################
# Nonlinear
##########################################
set.seed(1)
x = matrix(rnorm(200*2), ncol =2) # First 100 (train) 
x[1:100 ,]   = x[1:100,]+2 # add 2 to 1st 100 obs 
x[101:150 ,] = x[101:150,]-2 # minus 2 to last 50 obs
y            = c( rep(-1,150), rep(1,50) )
dat          = data.frame(x=x, y=as.factor(y))
plot(x, col=3-y)

##########################################
# Small Cost
##########################################
ind = sample(1:200, 100, replace=FALSE) # from the seq(1:200) draw 100 randomly
train = dat[ind,]
test  = dat[-ind,]

# library(e1071)
svm.small = svm(y~., data=train, 
                kernel ="polynomial", degree=2, 
                cost=1)
plot(svm.small, train, 
     svSymbol=16, dataSymbol=1,
     color.palette = terrain.colors)
mtext("Nonlinear with Cost=1", side=3)


svm.small$index
summary(svm.small)
ypred.small = predict(svm.small, newdata=test[,1:2])
acc.small = mean(as.numeric(ypred.small==test[,3]))


##########################################
# Large Cost
##########################################
svm.large = svm(y~., data=train, 
                kernel="polynomial", degree=2, 
                cost=1e5) # 1e5 = 1*10^5
plot(svm.large, train, 
     svSymbol=16, dataSymbol=1,
     color.palette = terrain.colors)
mtext("Nonlinear with Cost=100000", side=3)
summary(svm.large)
ypred.large = predict(svm.large, newdata=test[,1:2])
acc.large = mean(as.numeric(ypred.large==test[,3]))


##########################################
# Cross-Validation
##########################################
set.seed(1)
tune.out = tune(svm, y~., data=dat[ind,], kernel ="polynomial",
                ranges =list(cost=c(0.1, 1, 10, 100), degree=2))
summary(tune.out) # should choose the one with smallest dispersion (s.d)

table(true=test[,"y"], pred=predict (tune.out$best.model ,
                                     newdata=test[,]))

