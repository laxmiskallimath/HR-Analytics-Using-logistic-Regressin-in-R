setwd("E:/Laxmi/RCA/Ravi sir stat/Logistic Reg Data/HR Attrition/HRA_Laxmi_FINAL")

# Import Data 
hr <- read.csv("HR Attrition.csv")
#######CART###############
library(rpart)  			        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script
########Exploratory analysis#########
str(hr)

table(hr$Attrition)
prop.table(table(hr$Attrition))
mean(hr$Age)
aggregate(hr$Age,by=list(hr$Attrition),mean)
attach(hr)
m1<-rpart(Attrition~., data=hr)
fancyRpartPlot(m1,cex=0.5)
rpart.plot(m1,cex=0.8)
m1

####Accuracy of model######################
hr$pred<-predict(m1,hr,type='class')
View(hr)
fix(hr)
table(hr$Attrition,hr$pred)
prop.table(table(hr$Attrition,hr$pred),2)
help("type")

# Sensitivity -0.42%
100/(100+137)

# Specificity -0.97%
1201/(1201+32)

# Accuracy -0.88%
(1201+100)/(1201+32+137+100)

24+28+56+2+10+4+2
6+1+1+2+1+1+3+14
(966+81)/1470
1470/0.916
966/1470
966+81
1047*0

