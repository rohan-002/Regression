# Objective : In data we have total 7 fish species in a fish market and observation of 159 fishes.
# Our objective is estimate the weight of fish with help of other  given variables.
##### Dataset Contains : 
## 1.Species : species name of fish
## 2.Weight(y) : weight of fish in gram(gm)
## 3.Lenght1(x1) : vertical length in cm
## 4.Lenght2(x2) : diagonl length in cm
## 5.Lenght3(x3) : cross length in cm
## 6.Height(x4) : Height in cm
## 7. Width(x5) : diagonal width in cm

#import data sets
data <- read.csv(file = "Fish1.csv")

#Checking the column names  
names(data)

#Removing species column as we are doing regression
data <-data[-1] 

#structure of dataset
str(data)

#Checking missing values and outliers
sapply(data,function(x) sum(is.na(x)))

#Checking correlation matrix
cor(data)

#attach data and checking summary
attach(data)
summary(data)

#ploting boxplot to check outliers as linear reg is affected by outliers
boxplot(data)

#outliers treatment for length1
boxplot(data$Length1)
summary(data$Length1)
upper <- 32.70 + 1.5*IQR(data$Length1);upper
data$Length1[data$Length1 > upper] <- upper 
summary(data$Length1)
boxplot(data$Length1)


#outliers treatment for length2
boxplot(data$Length2)
summary(data$Length2)
upper <- 35.50 + 1.5*IQR(data$Length2);upper
data$Length2[data$Length2 > upper] <- upper 
summary(data$Length2)
boxplot(data$Length2)

#outliers treatment for length3
boxplot(data$Length3)
summary(data$Length3)
upper <- 39.65 + 1.5*IQR(data$Length3);upper
data$Length3[data$Length3 > upper] <- upper 
summary(data$Length3)
boxplot(data$Length3)


#data partition
set.seed(8000)
library(caret)
part <- createDataPartition(data$Weight,p = 0.70,list = FALSE)
train <- data[part,]
test <- data[-part,]


#model building
model <- lm(Weight ~ .,data = train)
summary(model)
library(car)
vif(model)

#variable significance
model1 <- step(lm(Weight ~.,data = train),direction = "both")
summary(model1)
vif(model1)

#removing variable which has highest vif values
model1 <- lm(Weight ~ Length2+Width,data = train)
summary(model1)
vif(model1)

#assumption of model

#plotting the 4 plots to check the assumption
par(mfrow = c(2,2))
plot(model1)

#checking the assumption of autocorrelation 
library(lmtest)
dwtest(model1)

#checking assumption of heteroscedasticity
library(gvlma)
gvlma(model1)

#Since our model fails the assumption of heteroscedasticity we will use some transformation 
#here we will use boxcox tranformation which is a type of power tranformation
## and create a dummy variable and again we'll split data and build the model over it


library(caret)
bcweight <- BoxCoxTrans(data$Weight) 
bcweight
data <- cbind(data,weight_bc = predict(bcweight,data$Weight))
head(data)

#data partition
set.seed(8000)
library(caret)
part <- createDataPartition(data$weight_bc,p = 0.70,list = FALSE)
train <- data[part,]
test <- data[-part,]

#
#model building
model <- lm(weight_bc ~ .,data = train)
summary(model)
library(car)
vif(model)

#variable significance
model1 <- step(lm(weight_bc ~.,data = train),direction = "both")
summary(model1)
vif(model1)

#assumption of model
#plotting the 4 plots to check the assumption
par(mfrow = c(2,2))
plot(model1)

#checking the assumption of autocorrelation 
library(lmtest)
dwtest(model1)

#checking assumption of heteroscedasticity
library(gvlma)
gvlma(model1)


#prediction on test data
test$predict <- predict(model1,test)

#converting predicted values to actual weights
test$originl <- ((test$predict*0.3)+1)^(10/3)


