############ CHAID MODELS ON - HR Attrition###############
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
prop.table(x)

# Data type conversion 
# Convert below all variables into factors 
# Education,EnvironmentSatisfaction,JobInvolvement,JobSatisfaction
# PerformanceRating,RelationshipSatisfaction,WorkLifeBalance

# Education
hr$Education <- as.factor(hr$Education)

# EnvironmentSatisfaction
hr$EnvironmentSatisfaction <- as.factor(hr$EnvironmentSatisfaction)
str(hr$EnvironmentSatisfaction)

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

# Make Seperate categorical and numerical variables dataframe 
library(dplyr)
hr_numerical <-select_if(hr,is.numeric)
names(hr_numerical)

hr_categorical <- select_if(hr,is.factor)
names(hr_categorical)

# Convert all numeric variables in to categorical
# Age
quantile(hr$Age)
hr$Age_new <- ifelse(hr$Age>=46,"Highage39",
                     ifelse(hr$Age>=32,"Mediumage26_39","Lowage26"))
60-18
(60-18)/3#14
18+14#14+0%
32+14
46+14

str(hr$Age_new)
hr$Age_new <- as.factor(hr$Age_new)
str(hr$Age_new)

# "DailyRate"  
quantile(hr$DailyRate)
1499-102
1397/3
102+465
567+465
1032+465
hr$DailyRate_new <- ifelse(hr$DailyRate>=1000,"HighRate_1000",
                           ifelse(hr$DailyRate>=500,"MediumRate_500_1000","LowRate<500"))
str(hr$DailyRate_new)
hr$DailyRate_new <- as.factor(hr$DailyRate_new)
str(hr$DailyRate_new)

# "DistanceFromHome"
quantile(hr$DistanceFromHome)
29-1
28/3
1+10
11+10
21+10

hr$DFH_New <- ifelse(hr$DistanceFromHome>=20,"HighDFH_20",
                     ifelse(hr$DistanceFromHome>=10,"MediumDFH_10_20","LowDFH<10"))

str(hr$DFH_New)
hr$DFH_New <- as.factor(hr$DFH_New)
str(hr$DFH_New)

# "HourlyRate"
quantile(hr$HourlyRate)
100-30
70/3
30+24
54+24
78+24
hr$HourlyRate_New <- ifelse(hr$HourlyRate>=78,"HighHR",
                            ifelse(hr$HourlyRate>=54,"MediumHR","LowHR"))
str(hr$HourlyRate_New)
hr$HourlyRate_New <- as.factor(hr$HourlyRate_New)
str(hr$HourlyRate_New)

# "JobLevel" 
quantile(hr$JobLevel)
5-1
4/3
1+1
2+2
4+2

hr$JobLevel_New <- ifelse(hr$JobLevel>=2,"Highlevel","Lowlevel")
str(hr$JobLevel_New)
hr$JobLevel_New <- as.factor(hr$JobLevel_New)
str(hr$JobLevel_New)

# "MonthlyIncome" 
quantile(hr$MonthlyIncome)
19999-1009
18990/3
1009+6330
7339+6330
13669+6330
hr$MonthlyIncome_New <- ifelse(hr$MonthlyIncome>=13669,"HighIncome",
                               ifelse(hr$MonthlyIncome>=7339,"MediumIncome","LowIncome"))

str(hr$MonthlyIncome_New)
hr$MonthlyIncome_New <- as.factor(hr$MonthlyIncome_New)
str(hr$MonthlyIncome_New)

# "MonthlyRate" 
quantile(hr$MonthlyRate)
26999-2094
24905/3
2094+8302
10395+8302
18697+8302
hr$MonthlyRate_New <- ifelse(hr$MonthlyRate>=18697,"HighMRate",
                             ifelse(hr$MonthlyRate>=10395,"MediumMRate","LowMRate"))
str(hr$MonthlyRate_New)
hr$MonthlyRate_New <- as.factor(hr$MonthlyRate_New)
str(hr$MonthlyRate_New)

# "NumCompaniesWorked" 
quantile(hr$NumCompaniesWorked)
hr$NumCompaniesWorked_New <- ifelse(hr$NumCompaniesWorked>=6,"HighNCW",
                                    ifelse(hr$NumCompaniesWorked>=3,"MediumNCW","LowNCW"))

str(hr$NumCompaniesWorked_New)
hr$NumCompaniesWorked_New <- as.factor(hr$NumCompaniesWorked_New)
str(hr$NumCompaniesWorked_New)

# "PercentSalaryHike" 
quantile(hr$PercentSalaryHike)
25-11
14/3
11+5
16+5
21+5
hr$PercentSalaryHike_New <- ifelse(hr$PercentSalaryHike>=21,"HighPSH",
                                   ifelse(hr$PercentSalaryHike>=16,"MediumPSH","LowPSH"))
str(hr$PercentSalaryHike_New)
hr$PercentSalaryHike_New <- as.factor(hr$PercentSalaryHike_New)
str(hr$PercentSalaryHike_New)

# StandardHours
# quantile(hr$StandardHours)
# hr$StandardHours_New <- ifelse(hr$StandardHours>=20,"High",
#                                    ifelse(hr$StandardHours>=15,"Medium","Low"))

# "StockOptionLevel"
quantile(hr$StockOptionLevel)
hr$stock_option_New <- ifelse(hr$StockOptionLevel>=3,"HighSOL",
                              ifelse(hr$StockOptionLevel>=2,"MediumSO","LowSOL"))
str(hr$stock_option_New)
hr$stock_option_New <- as.factor(hr$stock_option_New)
str(hr$stock_option_New)

# "TotalWorkingYears" 
quantile(hr$TotalWorkingYears)
40/3
14
14+14
28+14
42+14
hr$TotalWorkingYears_New <- if_else(hr$TotalWorkingYears>=28,"HighExperience",
                                    ifelse(hr$TotalWorkingYears>=14,"MediumExperience","LessExperience"))
table(hr$TotalWorkingYears_New)
str(hr$TotalWorkingYears_New)
hr$TotalWorkingYears_New <- as.factor(hr$TotalWorkingYears_New)
str(hr$TotalWorkingYears_New)

# "TrainingTimesLastYear" 
quantile(hr$TrainingTimesLastYear)
6/3
2
2+2
4+2
hr$TrainingTimesLastYea_New<- ifelse(hr$TrainingTimesLastYear>=4,"MoreTraining",
                                     ifelse(hr$TrainingTimesLastYear>=2,"MediumTraining","LessTraining"))
table(hr$TrainingTimesLastYea_New)
str(hr$TrainingTimesLastYea_New)
hr$TrainingTimesLastYea_New <- as.factor(hr$TrainingTimesLastYea_New)
str(hr$TrainingTimesLastYea_New)

# "YearsAtCompany"  
quantile(hr$YearsAtCompany)
40-0
40/3
14+0
14+14
28+14
hr$YearsAtCompany_New <- ifelse(hr$YearsAtCompany>=28,"MoreYears",
                                ifelse(hr$YearsAtCompany>=14,"MediumYears","LessYears"))
table(hr$YearsAtCompany_New)
str(hr$YearsAtCompany_New)
hr$YearsAtCompany_New <- as.factor(hr$YearsAtCompany_New)
str(hr$YearsAtCompany_New)

# "YearsInCurrentRole"
quantile(hr$YearsInCurrentRole)
18-0
18/3
6+0
6+6
12+6
hr$YearsInCurrentRole_new<- ifelse(hr$YearsInCurrentRole>=12,"MoreYears",
                                   ifelse(hr$YearsInCurrentRole>=6,"MediumYears","LessYears"))
table(hr$YearsInCurrentRole_new)
str(hr$YearsInCurrentRole_new)
hr$YearsInCurrentRole_new <- as.factor(hr$YearsInCurrentRole_new)
str(hr$YearsInCurrentRole_new)

# "YearsSinceLastPromotion"
quantile(hr$YearsSinceLastPromotion)
15-0
15/3
5
5+5
10+5
hr$YearsSinceLastPromotion_new <- ifelse(hr$YearsSinceLastPromotion>=10,"High",
                                         ifelse(hr$YearsSinceLastPromotion>=5,"Medium","Low"))
str(hr$YearsSinceLastPromotion_new)
hr$YearsSinceLastPromotion_new <- as.factor(hr$YearsSinceLastPromotion_new)
str(hr$YearsSinceLastPromotion_new)

# "YearsWithCurrManager" 
quantile(hr$YearsWithCurrManager)
17/3
6+0
6+6
6+12
hr$YearsWithCurrManager_new <- ifelse(hr$YearsWithCurrManager>=12,"More",
                                      ifelse(hr$YearsWithCurrManager>=6,"Medium","Less"))

table(hr$YearsWithCurrManager_new)
str(hr$YearsWithCurrManager_new)
hr$YearsWithCurrManager_new <- as.factor(hr$YearsWithCurrManager_new)
str(hr$YearsWithCurrManager_new)

# library(caTools)
# set.seed(1000)
# train_rows <- sample.int(n=nrow(hr),0.7*nrow(hr))
# head(train_rows)
# train <- hr[train_rows, ]
# test <- hr [-train_rows, ]
# dim(train)
# dim(test)

# CHAID
library(CHAID)
m2 <- chaid(Attrition~BusinessTravel+Department+Education+EducationField+EnvironmentSatisfaction+Gender
            +JobInvolvement+JobRole+JobSatisfaction+MaritalStatus+Over18+OverTime+PerformanceRating+
              RelationshipSatisfaction+WorkLifeBalance+Age_new+DailyRate_new+DFH_New+HourlyRate_New+
              JobLevel_New+MonthlyIncome_New+MonthlyRate_New+NumCompaniesWorked_New+PercentSalaryHike_New+
              stock_option_New+TotalWorkingYears_New+TrainingTimesLastYea_New+YearsAtCompany_New+
              YearsInCurrentRole_new+YearsSinceLastPromotion_new+YearsWithCurrManager_new,data = hr)
plot(m2,cex=0.1,extra=0.01)
# digress for plotting
plot(m2,type = "simple")
plot(m2,main = "Testing Graphical Options",gp=gpar(fontsize=6),type = "simple")
plot(m2,main="Testing more graphical options",gp=gpar(col="blue",lty="solid",lwd=3,fontsize=6))
m2
# library(rpart)  			        # Popular decision tree algorithm
# library(rattle)					# Fancy tree plot
# library(rpart.plot)				# Enhanced tree plots
# library(RColorBrewer)				# Color selection for fancy tree plot
# library(party)					# Alternative decision tree algorithm
# library(partykit)				# Convert rpart object to BinaryTree
# library(caret)					# Just a data source for this script


####Accuracy of model#####################
pred <- as.data.frame(predict(m2,hr,type="response"))
hr <- cbind(hr,pred)
x <- table(hr$Attrition,hr$`predict(m2, hr, type = "response")`)
x

# sensitivity-82/(82+155)-0.34%
82/(82+155)
# specificity -0.96%
1194/(1194+39)
# accuracy -0.86%
(82+1194)/(82+1194+39+155)
##Overall Acccuracy
sum(diag(x))/sum(x)

# https://www.r-bloggers.com/chaid-and-r-when-you-need-explanation-may-15-2018/

