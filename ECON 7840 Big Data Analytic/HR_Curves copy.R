rm(list=ls()) # Clear environment
cat("\014")   # Clear Console
dev.off()     # Clear plots

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

setwd("~/Documents/AEcon/Summer term/Big Data/R")

# import data
library(readr)
HR <- read_csv("HR.csv")

# rebalance the sample, create a new data set
table(HR$left) # much more stay then left
Yes <- which(HR$left==0) # List of all the entries with left = 0, 11428
No <- which(HR$left==1)# List of all the entries with left = 1, 3571
length(Yes)
length(No)
down <- c(sample(Yes,length(No)),No) # "Paste" the indices
length(down) # 3571*2 = 7142
HR.down <- HR[down,] # Create new dataframe with the indices in "down"
str(HR.down)
table(HR.down$left)

# Required Packages
library(ROCR)     # For evaluation metrics
library(caret)    # For confusion matrix
library(kernlab)  # Another package for SVM
library(foreign)  # For importing data
library(e1071)    # SVM
library(C50)      # Tree

set.seed(1)
j <- sample(1:nrow(HR.down),size=0.8*nrow(HR.down),replace = FALSE)
traindata <- HR.down[j,]
testdata  <- HR.down[-j,]

# Model 1: Logistic regression
logit <- glm(left ~ ., data = traindata, 
             family = binomial(link="logit"))
summary(logit)
BIC(logit)
logLik(logit) # log likelyhood the larger tge better


# Model 2a: Support Vector Machine, ::two packages have the same function name.
svm_model <- e1071::svm(left ~ ., data= traindata, 
                        kernel="linear", cost =10) 
print(svm_model)
summary(svm_model)

# Model 2b: Support Vector Machine
svm_model2 <- kernlab::ksvm(left ~ ., data= traindata, 
                            kernel="vanilladot", cost =10) 
print(svm_model2)
summary(svm_model2)

# Model 3: Tree Model
traindata$left <- as.factor(traindata$left)
tree.model      <- C5.0(left ~., data=traindata, 
                        control = C5.0Control(minCases = 500))
tree.model
summary(tree.model)
plot(tree.model)

# Model 4: knn
vars <- c('satisfaction_level', 'last_evaluation', 'number_project', 'average_montly_hours', 'time_spend_company',  
          'Work_accident', 'promotion_last_5years') # 'sales''salary'
knn <- knn3(traindata[,vars], as.factor(traindata$left), k=17)
df.knn <- data.frame(
  Yhat_knn = predict(knn, newdata = testdata[,vars])[,2],
  left    = testdata$left )


# Store probability predictions in testdata, for ROC 
testdata$Yhat_log <- predict(logit, testdata, type = "response")
testdata$Yhat_svm <- predict(svm_model2, testdata, type = "response")[,1]
testdata$Yhat_dt  <- predict(tree.model, testdata, type = "prob")[,2]
testdata$Yhat_knn <- df.knn$Yhat_knn
attach(testdata)

# Setting threshold x (Class prediction)
class_log <- function(x) ifelse(Yhat_log > x, 1, 0) # 1 for yes offer,
class_svm <- function(x) ifelse(Yhat_svm > x, 1, 0)
class_dt  <- function(x) ifelse(Yhat_dt > x, 1, 0)
class_knn  <- function(x) ifelse(Yhat_knn > x, 1, 0)

# Generate confusion matrix using caret::confusionMatrix(predict_values, actual_values)
library(caret)
(
  x <- confusionMatrix(factor(class_log(0.5)), 
                       factor(testdata$left))
)#'Positive' Class : 0 
(
x_log <- confusionMatrix(factor(class_log(0.5)), 
                       factor(testdata$left), 
                       positive=as.character(1))
) # Accuracy : 0.76 
(
x_svm <- confusionMatrix(factor(class_svm(0.5)), 
                       factor(testdata$left), 
                       positive=as.character(1))
)  # Accuracy : 0.7355  
(
x_dt <- confusionMatrix(factor(class_dt(0.5)), 
                         factor(testdata$left), 
                         positive=as.character(1))
) #Accuracy : 0.881  
(
  x_knn <- confusionMatrix(factor(class_knn(0.5)), 
                          factor(testdata$left), 
                          positive=as.character(1))
) #Accuracy : 0.9118 

# 1. Graph the results ROC
library(ROCR) # ROCR::prediction, ROCR::performance
predict_log <- prediction(Yhat_log, left) # prob predict, actual
predict_svm <- prediction(Yhat_svm, left)
predict_dt  <- prediction(Yhat_dt, left)
predict_knn <- prediction(Yhat_knn, left)

performance_log <- performance(predict_log, "tpr", "fpr")
performance_svm <- performance(predict_svm, "tpr", "fpr")
performance_dt  <- performance(predict_dt, "tpr", "fpr")
performance_knn <- performance(predict_knn, "tpr", "fpr")
# attributes(performance_log)

plot.new() # plot new figure, ROC curve
plot(performance_log, col= "deeppink")
plot(performance_svm, add = TRUE, col= "cyan3")
plot(performance_dt, add = TRUE, col= "blueviolet")
plot(performance_knn, add = TRUE, col= "orange")

abline(0,1, col = "red")
title("ROC curves")
legend(0.7, 0.5 ,c("Logistic", "SVM", "TREE","KNN"), 
       lty = c(1,1,1), # line type
       lwd = c(0.5,0.5,0.5), # line width
       col = c("deeppink", "cyan3", "blueviolet", "orange"),
       ncol=1, cex=0.7, y.intersp=0.5)


# 2. Accuracy
accuracy_log   <-  performance(predict_log, "acc") # performance metrix choose acc
accuracy_svm   <-  performance(predict_svm, "acc")
accuracy_tree  <-  performance(predict_dt, "acc")
accuracy_tree  <-  performance(predict_knn, "acc")

# Accuracy Curves
plot(accuracy_log,main="Logistic Regression")
plot(accuracy_svm,main="Support Vector Machine")
plot(accuracy_tree,main="Decision Tree")
plot(accuracy_tree,main="KNN")
# choose a cutoff point where acc is max

# 3. AUC
auc_log    <- performance(predict_log,"auc")
attributes(auc_log)
auc_log.y  <- unlist(slot(auc_log,"y.values"))
auc_log.d  <- round(auc_log.y,4) # round to 4 decimal places

auc_svm    <- performance(predict_svm,"auc")
auc_svm.y  <- unlist(slot(auc_svm,"y.values"))
auc_svm.d  <- round(auc_svm.y,4)

auc_dt   <- performance(predict_dt,"auc")
auc_dt.y <- unlist(slot(auc_dt,"y.values"))
auc_dt.d <- round(auc_dt.y,4)

auc_knn   <- performance(predict_knn,"auc")
auc_knn.y <- unlist(slot(auc_knn,"y.values"))
auc_knn.d <- round(auc_dt.y,4)

auc <- rbind(auc_log.d,auc_svm.d,auc_dt.d, auc_knn.d)
colnames(auc) <- c("AUC")
rownames(auc)<-c("Logistic","SVM","Tree", "Knn")
auc






# 3. CRC
crc_log <- performance(predict_log, measure="tpr", x.measure="rpp")
attributes(crc_log)
plot(crc_log, col="deeppink")

crc_svm <- performance(predict_svm, measure="tpr", x.measure="rpp")
plot(crc_svm, col="cyan3")

crc_dt <- performance(predict_dt, measure="tpr", x.measure="rpp")
plot(crc_dt, col="blueviolet")

crc_knn <- performance(predict_knn, measure="tpr", x.measure="rpp")
plot(crc_dt, col="orange")

plot.new()
plot(crc_log,  col="deeppink")
plot(crc_svm, add = T, col="cyan3")
plot(crc_dt, add = T, col="blueviolet")
plot(crc_knn, add = T,col="orange")
abline(0,1, col = "red")
title("CRC curves")
legend(0.7, 0.5 ,c("Logistic", "SVM", "TREE", "KNN"), 
       lty = c(1,1,1), # line type
       lwd = c(1,1,1), # line width
       col = c("deeppink", "cyan3", "blueviolet", "orange"),
       ncol=1, cex=0.7, y.intersp=0.5)

# 4. lift curve
lift.y.log <- unlist(slot(crc_log,'y.values'))
lift.x.log <- unlist(slot(crc_log,'x.values'))
lift.log <- lift.y.log/lift.x.log

lift.y.svm <- unlist(slot(crc_svm,'y.values'))
lift.x.svm <- unlist(slot(crc_svm,'x.values'))
lift.svm <- lift.y.svm/lift.x.svm

lift.y.dt <- unlist(slot(crc_dt,'y.values'))
lift.x.dt <- unlist(slot(crc_dt,'x.values'))
lift.dt <- lift.y.dt/lift.x.dt

lift.y.knn <- unlist(slot(crc_knn,'y.values'))
lift.x.knn <- unlist(slot(crc_knn,'x.values'))
lift.knn <- lift.y.knn/lift.x.knn

plot.new()
plot(x=lift.x.log, #rpp
     y=lift.log, # tpr/rpp
     type="l", col="deeppink")
plot(x=lift.x.svm, y=lift.svm, type="l", col="cyan3", add = T)
plot(x=lift.x.dt, y=lift.dt, type="l", col="blueviolet", add = T)
plot(x=lift.x.knn, y=lift.knn, type="l", col="blueviolet", add = T)
abline(1,0, col = "red")
