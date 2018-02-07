rm(list=ls())

#Read the file
library(readxl)
Case <- read_excel("~/Priyanka/IDS 572 Data Mining/Assignments/Homework_cae/A7.xlsx")
View(Case)

#Customer Age and Churn Boxplot
boxplot(Case$`Customer Age (in months)`~Case$`Churn (1 = Yes, 0 = No)`, main = "Box Plot of Age vs Churn", xlab = "Churn", ylab = "Age in months", col=c("steelblue","red"))

#Removing the ID column and integrating C function using RCPP package
install.packages("Rcpp")
library(Rcpp)
d<- Case[,-1]

#Change variable names without spaces for ease of working
names(d)[names(d)== "Customer Age (in months)"]<- "CustomerAge"
names(d)[names(d)== "CHI Score Month 0"]<- "CHIScoreMonth0"
names(d)[names(d)== "CHI Score 0-1"]<- "CHIScore01"
names(d)[names(d)== "Support Cases Month 0"]<- "SupportCasesMonth0"
names(d)[names(d)== "Support Cases 0-1"]<- "SupportCases01"
names(d)[names(d)== "SP Month 0"]<- "SPMonth0"
names(d)[names(d)== "SP 0-1"]<- "SP01"
names(d)[names(d)== "Logins 0-1"]<- "Logins01"
names(d)[names(d)== "Blog Articles 0-1"]<- "BlogArticles01"
names(d)[names(d)== "Views 0-1"]<- "Views01"
names(d)[names(d)== " Days Since Last Login 0-1"]<- "DaysSinceLastLogin01"
names(d)[names(d)== "Churn (1 = Yes, 0 = No)"]<- "Churn"

#Performing Logistic regression in the unbalanced data first

#Intall required packages and Split train, test data
install.packages("aod")
install.packages("ggplot2")
install.packages("Rcpp")
library(aod)
library(ggplot2)
library(Rcpp)
set.seed(1)
d$Churn<- as.factor(d$Churn)
index= sample(2,nrow(d), replace= TRUE, prob = c(0.7,0.3))
unbalanced_trainData =d[index==1,]
unbalanced_testData= d[index==2, ]

#Logistic Regression
mylogit_unbalanced = glm(Churn~., data = unbalanced_trainData, family = "binomial")
summary(mylogit_unbalanced)

#Predicting on test data using the logistic model
d1logpred_unbalaned<- predict(mylogit_unbalanced, newdata= unbalanced_testData, type= "response")
predictedchurn_unbalanced<- ifelse(d1logpred_unbalaned> 0.5, 1, 0)

unbalancedtestDataforlogistic<- unbalanced_testData
#Bind the Predicted and Actual test Data
output<- cbind(unbalancedtestDataforlogistic, predictedchurn_unbalanced)
table(output$Churn, output$predictedchurn_unbalanced) 

table(unbalanced_trainData$Churn)
#   0    1 
# 4178  223
#Data is highly unbalanced
#In the case of unbalanced data, the accuracy is coming to be very high i;e 95% but  
#Recall is 0. Hence the model is not a good model and therefore we need balancing for the data

#Oversampling
install.packages("ROSE")
library(ROSE)
index= sample(2,nrow(d), replace= TRUE, prob = c(0.7,0.3))
trainData =d[index==1,]
testData= d[index==2, ]
d_oversampling<- ovun.sample(Churn~., data= trainData, method= "over", N= 8496)$data
table(d_oversampling$Churn)

#Logistic Regression
mylogit_oversampled = glm(Churn~., data = d_oversampling, family = "binomial")
summary(mylogit_oversampled)
d1logpred_oversampled<- predict(mylogit_oversampled, newdata= testData, type= "response")
predictedchurn_oversampled<- ifelse(d1logpred_oversampled> 0.5, 1, 0)
balancedtestDataforlogistic_over<- testData
output1<- cbind(balancedtestDataforlogistic_over, predictedchurn_oversampled)
table(output1$Churn, output1$predictedchurn_oversampled)

#0    1
#0 1120  690
#1   40   51
#Recall: 61.7 percent

#Randon Forest
install.packages("randomForest")
library(randomForest)
rf_oversampled = randomForest(Churn~. , data = d_oversampling, ntree = 500, mtry = 4, proximity = TRUE, replace = TRUE, sampsize = ceiling(0.65*nrow(d_oversampling)), importance = TRUE )
print(rf_oversampled) #Print the error rates

#Confusion matrix:
#0    1 class.error
#0 4100  114  0.02705268
#1    0 4282  0.00000000

#Get accuracy of prediction in test data
rfpred_oversampled = predict(rf_oversampled, newdata = testData)
table(rfpred_oversampled, testData$Churn)
#rfpred_oversampled    0    1
#0 1770   73
#1   40   18
#Recall: 19.1 percent

#Decision Tree for Over sampled
library(rpart)
rpart_oversampled= rpart(Churn~., data = d_oversampling, parms= list(split= "gini"), control = rpart.control(minsplit = 10, cp= 0.001))
rpart_oversampled_pred= predict(rpart_oversampled, type= "class",  newdata= testData)
table(rpart_oversampled_pred, testData$Churn)
#rpart_oversampled_pred    0    1
#0                       1512   58
#1                        298   33
#Recall 31 percent

#Undersampling:
d_undersampling<- ovun.sample(Churn~., data= trainData, method= "under", N= 468, seed= 1)$data
table(d_undersampling$Churn)
#0   1 
#236 232
#Above is balanced dataset

#Random Forest for undersampling
rf_undersampled = randomForest(Churn~. , data = d_undersampling, ntree = 500, mtry = 4, proximity = TRUE, replace = TRUE, sampsize = ceiling(0.65*nrow(d_undersampling)), importance = TRUE )

#Get accuracy of prediction in test data
rfpred_undersampled = predict(rf_undersampled, newdata = testData)
table(rfpred_undersampled, testData$Churn)
#rfpred_undersampled    0    1
#0                    1219   30
#1                     591   61
#Recall=67.4%

#Decision Tree using R part
rpart_undersampled= rpart(Churn~., data = d_undersampling, parms= list(split= "gini"), control = rpart.control(minsplit = 10, cp= 0.001))
rpart_undersampled_pred= predict(rpart_undersampled, type= "class",  newdata= testData)
table(rpart_undersampled_pred, testData$Churn)


rpart_undersampled_pred   
#      0     1
# 0  1134   37
# 1   676   54
#Recall 64%


#Logistic Regression for undersampling
mylogit_undersampled = glm(Churn~., data = d_undersampling, family = "binomial")
d1logpred_undersampled<- predict(mylogit_undersampled, newdata= testData, type= "response")

predictedchurn_undersampled<- ifelse(d1logpred_undersampled> 0.5, 1, 0)

balancedtestDataforlogistic_under<- testData
output4<- cbind(balancedtestDataforlogistic_under, predictedchurn_undersampled)
table(output4$Churn, output4$predictedchurn_undersampled)
#Recall: 62.9 %

#Both Sampling
d_bothsampling<- ovun.sample(Churn~., data= trainData, method= "both", N= 4482, p= 0.5)$data
table(d_bothsampling$Churn)

#Random Forest
rf_bothsampled = randomForest(Churn~. , data = d_bothsampling, ntree = 500, mtry = 4, proximity = TRUE, replace = TRUE, sampsize = ceiling(0.65*nrow(d_bothsampling)), importance = TRUE )

#Get accuracy of prediction in test data
rfpred_bothsampled = predict(rf_bothsampled, newdata = testData)
table(rfpred_bothsampled, testData$Churn)
#Recall: 31.4 percent

##Logistic Regression both sampling
mylogit_bothsampled = glm(Churn~., data = d_bothsampling, family = "binomial")
d1logpred_bothsampled<- predict(mylogit_bothsampled, newdata= testData, type= "response")
predictedchurn_bothsampled<- ifelse(d1logpred_bothsampled> 0.5, 1, 0)
balancedtestDataforlogistic_both<- testData
output2<- cbind(balancedtestDataforlogistic_both, predictedchurn_bothsampled)
table(output2$Churn, output2$predictedchurn_bothsampled)
#Recall: 64 percent

#Decision trees both sampling
rpart_bothsampled= rpart(Churn~., data = d_bothsampling, parms= list(split= "gini"), control = rpart.control(minsplit = 10, cp= 0.001))
rpart_bothsampled_pred= predict(rpart_bothsampled, type= "class",  newdata= testData)
table(rpart_bothsampled_pred, testData$Churn)
#Recall: 32.5 percent

#Model	                  Oversampling	Undersampling	Both
#Logistic Regression	        61.7	      62.9	       64
#Random Forest	              19.1	      67.4	      31.4
#Decision trees	              31	        64	         32.5


#Explanation: From the 1st logistic regression model, by using the unbalanced data we got 
#the accuracy as 95% but the recall was 0% which is not desired because the aim is to predict 
#the people who are actually going to leave or churn. Therefore, Recall is a better parameter 
#to measure the model performance. We tried different samplings like Over sampling, 
#Undersampling and both to see the performance of the model. We considered Logistic regression model, 
#Random Forests and the Decision trees to see the performance under different sampling. 
#Ultimately, we found that the Recall value is highest under Random Forest with Undersampling 
#as shown in the table above. Also, we can see that the Undersampling is the best sampling as the 
#recall values are higher under all the models.  We can also see that the Logistic 
#regression perform same regardless of the sampling used.

#To find the importance of variables we use the varImpPlot() function
varImpPlot(rf_undersampled)

#Code to find the top 100 customers.
prob<- predict(rf_undersampled, newdata= d, 'prob')
nr<- (1:nrow(d))[order(prob[,2], decreasing= T)[1:100]]
nr

           






