############Logistic Regression Case Study - HR Attrition###############
# Set working Directory 
setwd("E:/Laxmi/RCA/Ravi sir stat/Logistic Reg Data/HR Attrition/HRA_Laxmi_FINAL")

# Import Data 
hr <- read.csv("HR Attrition.csv")

#Check the dimensions(no of rows and no of columns)
dim(hr)

#Check names of dataset hr
names(hr)

#Check top(first) rows of dataset hr 
head(hr)

#Check structure of dataset(data structure of each variable)
str(hr)

#Check summary of dataset 
summary(hr)

# Variable Identification 
# Dependent variable is - Attrition 
str(hr$Attrition)
#If Attrition No = 0 
#If Attrition Yes = 1  
hr$Attrition <- ifelse(hr$Attrition=="Yes",1,0)
hr$Attrition <- as.factor(hr$Attrition)
str(hr$Attrition)
x <- table(hr$Attrition)
x
prop.table(x)# data is skewed 0=0.16%,1=0.83%

# Data type conversion 
# Convert below all variables into factors 
# Education,EnvironmentSatisfaction,JobInvolvement,JobSatisfaction
# PerformanceRating,RelationshipSatisfaction,WorkLifeBalance

# Education
str(hr$Education)
hr$Education <- as.factor(hr$Education)

# EnvironmentSatisfaction
hr$EnvironmentSatisfaction <- as.factor(hr$EnvironmentSatisfaction)

# JobInvolvement
hr$JobInvolvement <- as.factor(hr$JobInvolvement)

# JobSatisfaction
hr$JobSatisfaction <- as.factor(hr$JobSatisfaction)

# PerformanceRating
hr$PerformanceRating <- as.factor(hr$PerformanceRating)

# RelationshipSatisfaction
hr$RelationshipSatisfaction <- as.factor(hr$RelationshipSatisfaction)

# WorkLifeBalance
hr$WorkLifeBalance <- as.factor(hr$WorkLifeBalance)

# Finally check with str(hr)
str(hr)

# Missing Value Imputation
sum(is.na(hr))
sapply(hr,function(x)sum(is.na(x)))

# Duplicate values - No duplicates value 
# Use duplicate()/unique() function to findout duplicate values

# Univariate Analysis 
# Age 
mean(hr$Age)
median(hr$Age)
sd(hr$Age)
summary(hr$Age)

# DailyRate
summary(hr$DailyRate)

# DistanceFromhome
min(hr$DistanceFromHome)
max(hr$DistanceFromHome)

# HourlyRate
summary(hr$HourlyRate)

# MonthlyIncome
mean(hr$MonthlyIncome)
max(hr$MonthlyIncome)
min(hr$MonthlyIncome)
sd(hr$MonthlyIncome)
summary(hr$MonthlyIncome)

# MonthlyRate
summary(hr$MonthlyRate)

# Yearssincelastpromotion
summary(hr$YearsSinceLastPromotion)

# YearswithCurrManager
summary(hr$YearsWithCurrManager)

# Bivariate Analysis
# Make Seperate categorical and numerical variables dataframe 
library(dplyr)
hr_numerical <-select_if(hr,is.numeric)
names(hr_numerical)

hr_categorical <- select_if(hr,is.factor)
names(hr_categorical)

# To make bivariate anlysis 
# For Numerical Variables perform independent t test 
# For categorical variables perform Chi sequare test 

# Numeric Variables 
# Age
t.test(hr$Age~hr$Attrition)
# var.test(hr$Age~hr$Attrition)
# t.test(hr$Age~hr$Attrition,var.equal=T)
# Ho -mean of age group 0 is equal to mean of age group 1
# H1 -mean of age group 0 is not equal to mean of age group 1 (33.60 != 37.56)
# p-value = 1.38e-08 ,p<0.05
# Reject Ho,Age is significant
library(ggplot2)
boxplot(hr$Age~hr$Attrition,main="Boxplot",xlab="Attrition",ylab="Age",col=c("Orange","Blue"))

# DailyRate
t.test(hr$DailyRate~hr$Attrition)
t.test(hr$DailyRate ~hr$Attrition)
# Ho -mean of group 0 is equal to mean of group 1
# H1 -mean of group 0 is not equal to mean of group 1 (750.36 != 812.50)
# p-value = 0.03004 ,p<0.05
# Reject Ho,DailyRate is significant 
boxplot(hr$DailyRate~hr$Attrition,main="Boxplot",xlab="Attrition",ylab="DailyRate",col=c("Orange","Blue"))
# DistanceFromHome
t.test(hr$DistanceFromHome~hr$Attrition)
# Ho -mean of  group 0 is equal to mean of  group 1
# H1 -mean of group 0 is not equal to mean of group 1 (10.63!=8.91)
# p-value = 0.004137,p<0.05
# Reject Ho, DistanceFromHome is significant
boxplot(hr$DistanceFromHome~hr$Attrition,main="Boxplot",xlab="Attrition",ylab="DistanceFromHome",col=c("Orange","Blue"))
boxplot(hr$Age ~hr$Attrition,main="boxplot",xlab="age",ylab="att",col=c("red","blue"))
# EmployeeCount
t.test(hr$EmployeeCount~hr$Attrition)
# Data is constant
# EmployeeCount is not significant
boxplot(hr$EmployeeCount~hr$Attrition,main="Boxplot",xlab="Attrition",ylab="EmployeeCount",col=c("Orange","Blue"))

# EmployeeNumber
t.test(hr$EmployeeNumber~hr$Attrition)
# Ho -mean of  group 0 is equal to mean of  group 1
# H1 -mean of group 0 is not equal to mean of group 1(1010.346!=1027.656) 
# p-value = 0.6768,p>0.05
# Accept H0.EmployeeNumber is not significant since p>0.05
# this variable is like a sl.no /id no which cnnot be taken in the model 
boxplot(hr$EmployeeNumber~hr$Attrition,main="Boxplot",xlab="Attrition",ylab="EmployeeNumber",col=c("Orange","Blue"))

# HourlyRate
t.test(hr$HourlyRate~hr$Attrition)
t.test(hr$HourlyRate~hr$Attrition,var.equal=T)
var.test(hr$HourlyRate~hr$Attrition)
# Ho -mean of  group 0 is equal to mean of  group 1 (65.57=65.95)
# H1 -mean of group 0 is not equal to mean of group 1
# p-value = 0.7914,p>0.05
# Accept H0.HourlyRate is not significant
boxplot(hr$HourlyRate~hr$Attrition,main="Boxplot",xlab="Attrition",ylab="HourlyRate",col=c("Orange","Blue"))


# JobLevel
t.test(hr$JobLevel~hr$Attrition)
# Ho -mean of  group 0 is equal to mean of  group 1
# H1 -mean of group 0 is not equal to mean of group 1 (1.63!=2.14)
# p-value = 9.845e-13 ,p<0.05
# Reject H0.JobLevel is significant
boxplot(hr$JobLevel~hr$Attrition,main="Boxplot",xlab="Attrition",ylab="JobLevel",col=c("Orange","Blue"))

# MonthlyIncome
t.test(hr$MonthlyIncome~hr$Attrition)
# Ho -mean of  group 0 is equal to mean of  group 1
# H1 -mean of group 0 is not equal to mean of group 1 (4787.093!=6832.740)
# p-value = 4.434e-13 ,p<0.05
# Reject H0.MonthlyIncome is significant
boxplot(hr$MonthlyIncome~hr$Attrition,main="Boxplot",xlab="Attrition",ylab="MonthlyIncome",col=c("Orange","Blue"))

# MonthlyRate
t.test(hr$MonthlyRate~hr$Attrition)
# H0-mean of group 0 is equal to mean of group 1
# H1-mean of group 0 is not equal to mean of group 1(14559.31!=14265.78)
# p-value = 0.5653,p>0.05
# Accept H0.MonthlyRate is not significant
boxplot(hr$MonthlyRate~hr$Attrition,main="Boxplot",xlab="Attrition",ylab="MonthlyRate",col=c("Orange","Blue"))

# NumCompaniesWorked
t.test(hr$NumCompaniesWorked~hr$Attrition)
# H0-mean of group 0 is equal to mean of group 1
# H1-mean of group 0 is not equal to mean of group 1(2.94!=2.64)but
# p-value = 0.1163,p>0.05
# Accept H0.NumCompaniesWorked is not significant
boxplot(hr$NumCompaniesWorked~hr$Attrition,main="Boxplot",xlab="Attrition",ylab="NumCompaniesWorked",col=c("Orange","Blue"))


# PercentSalaryHike
t.test(hr$PercentSalaryHike~hr$Attrition)
# H0-mean of group 0 is equal to mean of group 1(15.09=15.23)
# H1-mean of group 0 is not equal to mean of group 1
# p-value = 0.6144,p>0.05
# Accept H0.PercentSalaryHike is not significant
boxplot(hr$PercentSalaryHike~hr$Attrition,main="Boxplot",xlab="Attrition",ylab="PercentSalaryHike",col=c("Orange","Blue"))

# StandardHours
t.test(hr$StandardHours~hr$Attrition)
# data are essentially constant
# Accept H0.StandardHours is not significant
boxplot(hr$StandardHours~hr$Attrition,main="Boxplot",xlab="Attrition",ylab="StandardHours",col=c("Orange","Blue"))


# StockoptionLevel
t.test(hr$StockOptionLevel~hr$Attrition)
# H0-mean of group 0 is equal to mean of group 1
# H1-mean of group 0 is not equal to mean of group 1(0.52!=0.84)
# p-value = 2.812e-07,p<0.05 
# Reject H0.StockOptionLevel is significant
boxplot(hr$StockOptionLevel~hr$Attrition,main="Boxplot",xlab="Attrition",ylab="StockOptionLevel",col=c("Orange","Blue"))

# TotalWorkingYears
t.test(hr$TotalWorkingYears~hr$Attrition)
# H0-mean of group 0 is equal to mean of group 1
# H1-mean of group 0 is not equal to mean of group 1(8.24!=11.86)
# p-value = 1.16e-11,p<0.05
#Reject H0.TotalWorkingYears is significant
boxplot(hr$TotalWorkingYears~hr$Attrition,main="Boxplot",xlab="Atrrition",ylab="TotalWorkingYears",col=c("Orange","Blue"))

# TrainingTimesLastYear
t.test(hr$TrainingTimesLastYear~hr$Attrition)
# H0-mean of group 0 is equal to mean of group 1
# H1-mean of group 0 is not equal to mean of group 1(2.62!=2.83)
# p-value = 0.020,p<0.05
# Reject H0.TrainingTimesLastYear is significant
boxplot(hr$TrainingTimesLastYear~hr$Attrition,main="Boxplot",xlab="Atrrition",ylab="TrainingTimesLastYear",col=c("Orange","Blue"))
boxplot(hr$TrainingTimesLastYear)

# YearsAtCompany 
t.test(hr$YearsAtCompany~hr$Attrition)
# H0-mean of group 0 is equal to mean of group 1
# H1-mean of group 0 is not equal to mean of group 1(8.24!=11.86)
# p-value = 2.286e-07,p<0.05
# Reject H0.YearsAtCompany is significant
boxplot(hr$YearsAtCompany~hr$Attrition,main="Boxplot",xlab="Atrrition",ylab="YearsAtCompany",col=c("Orange","Blue"))

# YearsInCurrentRole
t.test(hr$YearsInCurrentRole~hr$Attrition)
# H0-mean of group 0 is equal to mean of group 1
# H1-mean of group 0 is not equal to mean of group 1(2.90!=4.48)
# p-value = 3.187e-11,p<0.05
# Reject H0.YearsInCurrentRole is significant
boxplot(hr$YearsInCurrentRole~hr$Attrition,main="Boxplot",xlab="Atrrition",ylab="YearsInCurrRole",col=c("Orange","Blue"))

# YearsSinceLastPromotion 
t.test(hr$YearsSinceLastPromotion~hr$Attrition)
# H0-mean of group 0 is equal to mean of group 1
# H1-mean of group 0 is not equal to mean of group 1(1.94!=2.23)
# p-value = 0.198,p>0.05
# Accept H0.YearsSinceLastPromotion is not significant
boxplot(hr$YearsSinceLastPromotion~hr$Attrition,main="Boxplot",xlab="Atrrition",ylab="YearsSinceLastPromotion",col=c("Orange","Blue"))

# YearsWithCurrManager
t.test(hr$YearsWithCurrManager~hr$Attrition)
# H0-mean of group 0 is equal to mean of group 1
# H1-mean of group 0 is not equal to mean of group 1(2.85!=4.36)
# p-value = 1.185e-10,p<0.05
# Reject H0.YearsWithCurrManager is significant
boxplot(hr$YearsWithCurrManager~hr$Attrition,main="Boxplot",xlab="Atrrition",ylab="YearsWithCurrManager",col=c("Orange","Blue"))

# Bivariatenalysis of categorical variables -------------------------------
# BusinessTravel
x1 <- table(hr$Attrition,hr$BusinessTravel)
x1
prop.table(x1,2)
chisq.test(x1)
# Reject H0. Business Travel is significant(p<0.05)
# Employees not leaving the company and they are non traveller is -92%,Trave_frequently=75% ,
# Travel_Rarely-85%
# Employees leaving the company and they are non traveller is -8%,Trave_frequently=24% ,
# Travel_Rarely-14%

y1 <- table(hr$Attrition,hr$BusinessTravel)
y1

prop.table(y1)
chisq.test(y1)
# Department
x2 <- table(hr$Attrition,hr$Department)
prop.table(x2,2)
chisq.test(x2)
# Reject H0. Department is significant
# Employees leaving the company working in ,HR -80%,R&D-86%,Sales -79%
# Employees not leaving the company working in,HR-19%,R&D is-13%,In Sales -20%

# EducationField
x3 <- table(hr$Attrition,hr$EducationField)
prop.table(x3,2)
chisq.test(x3)
# Reject H0. EducationField is significant
# Employees who are  leaving the company whose education field is HR is 74.07%,Lifescience is 0.85%,
# marketing is 77%,medical is 86%,others is 86%
# Employees not leaving the company and whose education field is HR is 25%,Lifescience is 22%,marketing is 22%,
# medical -13.57%,others -13.41%

# Gender 
x4 <- table(hr$Attrition,hr$Gender)
prop.table(x4,2)
chisq.test(x4)
# Accept H0. Gender is not significant
# Female and male  Employees not leaving the company 85% and 82% respectively
# Female and male  Employees leaving the company 14% and 17% respectively

# JobRole 
x5 <- table(hr$Attrition,hr$JobRole)
prop.table(x5,2)
chisq.test(x5)
#Reject H0. JobRole is significant
# Employees not leaving the company and there  job roles <-  HealthcareRepresentative-93%%,HR-76%
# Laboratory Technician-76%
# Employees leaving the company and there  job roles <-  HealthcareRepresentative-6%,HR-23%
# Laboratory Technician-23%


# MaritalStatus
x6 <- table(hr$Attrition,hr$MaritalStatus)
prop.table(x6,2)
chisq.test(x6)
# Reject H0. MaritalStatus is significant
# Employees leaving the company and they are -Divorced -10%,Married-12%,Single-25%
# Employees not leaving the company and they are -Divorced -89,Married-87%,Single-74%

# Over18 
x7 <- table(hr$Attrition,hr$Over18)
prop.table(x7,2)
chisq.test(x7)
# Accept H0. Over18 is not significant
# Employees not leaving the company and they are also over18 -83%
# Employees leaving the company and they are over18 16%

# OverTime
x8 <- table(hr$Attrition,hr$OverTime)
prop.table(x8,2)
chisq.test(x8)
# Reject H0. OverTime is significant
# # Employees leaving the company and they are actually working overtime and predicted also as 
# notworking overtime is 89%
# Employees not leaving the company and they are actually working overtime and predicted as not working 
# overtime is 69%,
# Employees leaving the company and they are actually working overtime and predicted as not working 
# overtime is 10%,
# Employees leaving the company and they are actually working overtime and predicted also as working 
# overtime is 30%,

# Education
x9 <- table(hr$Attrition,hr$Education)
prop.table(x9,2)
chisq.test(x9)
#Accept H0. Education is not significant
# Employees not leaving the company and there education levels are 1,2,3,4,5 and percentage of leaving the 
# company are -18%,15%,17%,14%,10%
# Employees leaving the company and there education levels are 1,2,3,4,5 and percentage of not 
# leaving the company are -81%,84%,82%,85%,89% respectively.

# EnvironmentSatisfaction
x10 <- table(hr$Attrition,hr$EnvironmentSatisfaction)
prop.table(x10,2)
chisq.test(x10)
#Reject H0. EnvironmentSatisfaction is significant
# Employees not leaving the company and there EnvironmentSatisfaction are 1,2,3,4 and percentage of
# not leaving the compay are-74%,85%,86%,86% respectively.
# Employees leaving the company and there EnvironmentSatisfaction are 1,2,3,4 and percentage of
# leaving the compay are-25%,14%,13%,13%

# JobInvolvement
x11 <- table(hr$Attrition,hr$JobInvolvement)
prop.table(x11,2)
chisq.test(x11)
# Reject H0. JobInvolvement is significant
# Employees not leaving the company and there jobinvolvement levels are 1,2,3,4 and percentage of
# not leaving the compay are-66%,81%,85%,90%respectively.
# Employees leaving the company and there jobinvolvement levels are 1,2,3,4 and percentage of
# leaving the compay are-33%,18%,14%,9%


# JobSatisfaction 
x12 <- table(hr$Attrition,hr$JobSatisfaction)
prop.table(x12,2)
chisq.test(x12)
#Reject H0. JobSatisfaction is significant
# Employees not leaving the company and there JobSatisfaction  levels are 1,2,3,4 and percentage of
# not leaving the compay are-77%,83%,83%,88% respectively.
# Employees leaving the company and there JobSatisfaction  levels are 1,2,3,4 and percentage of
# leaving the compay are-22%,16%,16%,11% respectively.

# PerformanceRating
x13 <- table(hr$Attrition,hr$PerformanceRating)
prop.table(x13,2)
chisq.test(x13)
# Accept H0. PerformanceRating is not significant
# Employees not leaving the company and performance rating is 3 -83%
# Employees not leaving the company and performance rating is 4 -83%
# Employees leaving the company and performance rating is 3 -16%
# Employees leaving the company and performance rating is 4 -16%

# RelationshipSatisfaction
x14 <- table(hr$Attrition,hr$RelationshipSatisfaction)
prop.table(x14,2)
chisq.test(x14)
# Accept H0. RelationshipSatisfaction is not significant
# Employees not leaving the company and there RelationshipSatisfaction levels are 1,2,3,4 and percentage of
# not leaving the compay are- 79%,85%,84%,85% respectively.
# Employees leaving the company and there RelationshipSatisfaction levels are 1,2,3,4 and percentage of
# leaving the compay are-20%,14%,15%,14%respectively

# WorkLifeBalance
x15 <- table(hr$Attrition,hr$WorkLifeBalance)
prop.table(x15,2)
chisq.test(x15)
#Reject H0. WorkLifeBalance is significant

##Split data to train and test - 70:30
##Simple Random sampling without replacement 
library(caTools)
set.seed(1000)
train_rows <- sample.int(n=nrow(hr),0.7*nrow(hr))
head(train_rows)
train <- hr[train_rows, ]
test <- hr [-train_rows, ]
dim(train)
# library(caret)
# sampleseed <-createDataPartition(hr$Attrition,p=0.8,list=F)
# dim(test)
# train <- hr[sampleseed,]
# test <- hr[-sampleseed,]
##Proportion of DV
prop.table(table(hr$Attrition))
prop.table(table(train$Attrition))
prop.table(table(test$Attrition))

##Build logistic regression model
##Drop non significant Numeric variables 
# EmployeeCount 
# EmployeeNumber 
# HourlyRate
# MonthlyRate
# NumCompaniesWorked 
# Over18
# PercentSalaryHike
# StandardHours
# YearsSinceLastPromotion 
##Drop non significant Ctegorical variables 
# Gender 
# Education 
# PerformanceRating 
# RelationshipSatisfaction
mod <- glm(Attrition~Age+BusinessTravel+DailyRate+Department+DistanceFromHome+EducationField+EnvironmentSatisfaction+
             JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+OverTime+
             StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+
             YearsWithCurrManager,data =train,family = binomial)
summary(mod)

##Drop Age EducationField DailyRate Department JobLevel JobRole(except Laboratory Technician)
##JobSatisfaction(except 4) MaritalStatusMarried MonthlyIncome StockOptionLevel TotalWorkingYears WorkLifeBalance2
##YearsAtCompany YearsWithCurrManager TrainingTimesLastYear

mod1 <- glm(Attrition~BusinessTravel+DistanceFromHome+EnvironmentSatisfaction+JobInvolvement+OverTime+YearsInCurrentRole+
              JobSatisfaction+MaritalStatus+WorkLifeBalance+JobRole,data = train,family = binomial)
summary(mod1)

##Use step function to get final model
mod2 <- step(mod1)
summary(mod2)

varImp(mod2)

#Multicollinarity check
library(car)
vif(mod2)#vif values are less than 5 no multicolinearity  

##Validation
train$hr_Prob <- predict(mod2,train,type = "response")
head(train)

train$hr_NP <- ifelse(train$hr_Prob>=0.5,"Yes","No")

head(train)
X1 <- table(train$Attrition,train$hr_NP)
X1
prop.table(X1,1)

#Interpretation of confusion matrix
# Attrition Yes = 1 # Employees leaving the company #If Attrition Yes #237
# Attrition No = 0  # Employees not leaving the company #If Attrition No #1233
# Employees actually leaving the company(Attrition-yes) predicted  as employees not going 
# to leave the company -107
# Employees actually leaving the company(Attrition-yes) predicted also as employees are going to 
# to leave the company -54
# Employees actually not leaving the company(Attrition-yes) predicted also as employees are not going 
# to leave the company -824
# Employees actually not leaving the company(Attrition-yes) predicted as employees are  going 
# to leave the company -23

# In Train Data set 
# ##Specificity-0.96%
823/(823+29)
961/(961+26)

#Sensitivity0.40%
71/(71+106)
81/(81+109)

# Accuracy - 0.86%
(71+823)/(823+71+29+106)
(81+961)/(81+961+26+109)

##Test
test$hr_Prob <- predict(mod2,test,type = "response")
test$hr_NP <- ifelse(test$hr_Prob>=0.5,"Yes","No") 
head(test)
X2 <- table(test$Attrition,test$hr_NP)
X2
prop.table(X2,1)

#In test data set 
##Specificity
367/(367+14)#0.96%
##Sensitivity
26/(26+34)#0.43%
##Accuracy =0.89%
(26+367)/(26+34+367+14)

17/(17+30)
233/(233+13)
(17+233)/(17+233+13+30)

##Concordance 
library(InformationValue)
library(InformationValue)
Concord <- Concordance(train$Attrition,train$hr_Prob)
Concordance(train$Attrition,train$hr_Prob)#C-0.84 D-0.15
Concordance(test$Attrition,test$hr_Prob)#C-0.81 D-0.18
##Probability of employee leaving the company is greater than probability of employees not leaving the company
##Higher the concurdance better will be the model

##Somers'D
##% of concurdance - % of disconcurdance 
somersD(train$Attrition,train$hr_Prob)##0.69
somersD(test$Attrition,test$hr_Prob)##0.63

somersD(train$Attrition,train$hr_Prob)

##ROC Curve
plotROC(train$Attrition,train$hr_Prob)#0.85
plotROC(test$Attrition,test$hr_Prob)#0.81


##Rank ordering and KS stat
Decile <- cut(train$hr_Prob,
              breaks = quantile(train$hr_Prob,
                                probs = seq(0,1,by=0.1)),
              include.lowest = T)
table(Decile,train$Attrition)

Decile_test <- cut(test$hr_Prob,
              breaks = quantile(test$hr_Prob,
                                probs=seq(0,1,by=0.1)), include.lowest = T)

table(Decile_test,test$Attrition)



# Check with ks cutt off value ``
# ##Test
# test$hr_Prob <- predict(mod2,test,type = "response")
# test$hr_NP <- ifelse(test$hr_Prob>=0.25,"Yes","No") 
# View(test)
# X2 <- table(test$Attrition,test$hr_NP)
# X2
# prop.table(X2,1)
# # sensitivity
# 42/(42+18)
# # specificity
# 320/(320+61)
# # Accuracy
# (42+320)/(42+320+18+61)
