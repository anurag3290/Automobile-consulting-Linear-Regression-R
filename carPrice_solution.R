# including libraries
library(tidyr)
library(MASS)
library("car")

carDf <- read.csv("CarPrice_Assignment.csv")
View(carDf)
str(carDf)

# Data Cleaning
# Splitting carName column
carDf$CarName <- as.character(carDf$CarName)
carDf <- separate(carDf,CarName, into=c('brand','model'),sep=' ')
carDf <- carDf[,-which(names(carDf) == "model")]
carDf <- carDf[,-which(names(carDf) == "car_ID")]

# making symboling column in the range of 0-5
carDf$symboling <- carDf$symboling + 2

#DUMMY VARIABLE CREATION.
# converting fueltype variable to numeric by replacing the levels:
# 0-diesel(No-gas) 1-gas
levels(carDf$fueltype)<-c(0,1)
carDf$fueltype<- as.numeric(levels(carDf$fueltype))[carDf$fueltype]

# converting aspiration variable to numeric by replacing the levels:
# 0-std(No-turbo) 1-turbo
levels(carDf$aspiration)<-c(0,1)
carDf$aspiration<- as.numeric(levels(carDf$aspiration))[carDf$aspiration]

# converting doornumber variable to numeric by replacing the levels:
# 0-four(No-two) 1-two
levels(carDf$doornumber)<-c(0,1)
carDf$doornumber<- as.numeric(levels(carDf$doornumber))[carDf$doornumber]

# converting enginelocation variable to numeric by replacing the levels:
# 0-rear(No-front) 1-front
levels(carDf$enginelocation)<-c(1,0)
carDf$enginelocation<- as.numeric(levels(carDf$enginelocation))[carDf$enginelocation]

#Converting "carbody" into dummies(Dimensionality reduction)
dummy_1 <- data.frame(model.matrix( ~carbody, data = carDf))
# Combine the dummy variables to the main data set, after removing the original categorical column
dummy_1 <- dummy_1[,-1]
carDf <- cbind(carDf[,-which(names(carDf) == "carbody")], dummy_1)

#Converting "drivewheel" into dummies(Dimensionality reduction)
dummy_1 <- data.frame(model.matrix( ~drivewheel, data = carDf))
# Combine the dummy variables to the main data set, after removing the original categorical column
dummy_1 <- dummy_1[,-1]
carDf <- cbind(carDf[,-which(names(carDf) == "drivewheel")], dummy_1)

#Converting "enginetype" into dummies(Dimensionality reduction)
dummy_1 <- data.frame(model.matrix( ~enginetype, data = carDf))
# Combine the dummy variables to the main data set, after removing the original categorical column
dummy_1 <- dummy_1[,-1]
carDf <- cbind(carDf[,-which(names(carDf) == "enginetype")], dummy_1)

#Converting "cylindernumber" into dummies(Dimensionality reduction)
dummy_1 <- data.frame(model.matrix( ~cylindernumber, data = carDf))
# Combine the dummy variables to the main data set, after removing the original categorical column
dummy_1 <- dummy_1[,-1]
carDf <- cbind(carDf[,-which(names(carDf) == "cylindernumber")], dummy_1)

#Converting "fuelsystem" into dummies(Dimensionality reduction)
dummy_1 <- data.frame(model.matrix( ~fuelsystem, data = carDf))
# Combine the dummy variables to the main data set, after removing the original categorical column
dummy_1 <- dummy_1[,-1]
carDf <- cbind(carDf[,-which(names(carDf) == "fuelsystem")], dummy_1)


#Converting "brand" into dummies(Dimensionality reduction)
dummy_1 <- data.frame(model.matrix( ~brand, data = carDf))
# Combine the dummy variables to the main data set, after removing the original categorical column
dummy_1 <- dummy_1[,-1]
carDf <- cbind(carDf[,-which(names(carDf) == "brand")], dummy_1)

## DATA CLEANING 
## There are multiple occurrence of different names of the same brand
## Merging them into 1 column(eg. brandtoyota & brandtoyouta)
carDf$brandtoyota <- carDf$brandtoyota + carDf$brandtoyouta
carDf <- carDf[,-which(names(carDf) == "brandtoyouta")]

## Similarly, brandvolkswagen, brandvokswagen and brandvw
carDf$brandvolkswagen <- carDf$brandvolkswagen + carDf$brandvokswagen + carDf$brandvw
carDf <- carDf[,-which(names(carDf) == "brandvokswagen")]
carDf <- carDf[,-which(names(carDf) == "brandvw")]

## Similarly, brandporsche & brandporcshce
carDf$brandporsche <- carDf$brandporsche + carDf$brandporcshce
carDf <- carDf[,-which(names(carDf) == "brandporcshce")]

## Similarly, brandnissan and brandNissan
carDf$brandnissan <- carDf$brandnissan + carDf$brandNissan
carDf <- carDf[,-which(names(carDf) == "brandNissan")]

## Similarly, brandmazda and brandmaxda
carDf$brandmazda <- carDf$brandmazda + carDf$brandmaxda
carDf <- carDf[,-which(names(carDf) == "brandmaxda")]

set.seed(100)
trainindices= sample(1:nrow(carDf), 0.7*nrow(carDf))
train = carDf[trainindices,]
test = carDf[-trainindices,]
model_1 <-lm(price~.,data=train)
summary(model_1)


# VARIABLE SELECTION
# Using steepAIC method to shortlist some variables which are significant
step <- stepAIC(model_1, direction="both")
step


# We noticed that in the last iteration, the following 20 variables have been eliminated in the stepAIC method
#highwaympg,brandisuzu,citympg,cylindernumberfour,cylindernumbersix,carlength,brandmercury,enginetypeohcv,fuelsystemmfi,fuelsystemspdi,brandaudi,brandvolvo,fueltype,fuelsystemidi,wheelbase,compressionratio,drivewheelfwd,symboling,doornumber,carheight;

# BACKWARD MODEL
# Creating model_2 after eliminating the above variables
model_2 <- lm(formula = price~ horsepower + brandsaab + boreratio + fuelsystemmpfi + enginetypeohc + cylindernumberfive + stroke + carbodyhardtop + brandhonda + fuelsystem2bbl + cylindernumberthree + brandjaguar + carbodysedan + curbweight + carbodyhatchback + carbodywagon + enginetypedohcv + peakrpm + enginetypel + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_2 in vif
vif(model_2)
summary(model_2)

# With high VIF value and highly insignificant factor, removing horsepower
# Creating model_3 after eliminating the above variables
model_3 <- lm(formula = price~ brandsaab + boreratio + fuelsystemmpfi + enginetypeohc + cylindernumberfive + stroke + carbodyhardtop + brandhonda + fuelsystem2bbl + cylindernumberthree + brandjaguar + carbodysedan + curbweight + carbodyhatchback + carbodywagon + enginetypedohcv + peakrpm + enginetypel + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_3 in vif
vif(model_3)
summary(model_3)

## enginesize and curbweight have the highest VIF but they quite significant
## looking for the variables which are highly insignificant and highest VIF among them

## Therefore, Creating model_4 after eliminating the enginetypeohc
model_4 <- lm(formula = price~ brandsaab + boreratio + fuelsystemmpfi + cylindernumberfive + stroke + carbodyhardtop + brandhonda + fuelsystem2bbl + cylindernumberthree + brandjaguar + carbodysedan + curbweight + carbodyhatchback + carbodywagon + enginetypedohcv + peakrpm + enginetypel + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_4 in vif
vif(model_4)
summary(model_4)

## Therefore, Creating model_5 after eliminating the fuelsystemmpfi
model_5 <- lm(formula = price~ brandsaab + boreratio + cylindernumberfive + stroke + carbodyhardtop + brandhonda + fuelsystem2bbl + cylindernumberthree + brandjaguar + carbodysedan + curbweight + carbodyhatchback + carbodywagon + enginetypedohcv + peakrpm + enginetypel + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_5 in vif
vif(model_5)
summary(model_5)

## Therefore, Creating model_6 after eliminating the fuelsystem2bbl
model_6 <- lm(formula = price~ brandsaab + boreratio + cylindernumberfive + stroke + carbodyhardtop + brandhonda + cylindernumberthree + brandjaguar + carbodysedan + curbweight + carbodyhatchback + carbodywagon + enginetypedohcv + peakrpm + enginetypel + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_6 in vif
vif(model_6)
summary(model_6)

## Therefore, Creating model_7 after eliminating the boreratio
model_7 <- lm(formula = price~ brandsaab + cylindernumberfive + stroke + carbodyhardtop + brandhonda + cylindernumberthree + brandjaguar + carbodysedan + curbweight + carbodyhatchback + carbodywagon + enginetypedohcv + peakrpm + enginetypel + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_7 in vif
vif(model_7)
summary(model_7)

## Therefore, Creating model_8 after eliminating the cylindernumberfive
model_8 <- lm(formula = price~ brandsaab + stroke + carbodyhardtop + brandhonda + cylindernumberthree + brandjaguar + carbodysedan + curbweight + carbodyhatchback + carbodywagon + enginetypedohcv + peakrpm + enginetypel + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_8 in vif
vif(model_8)
summary(model_8)

## Therefore, Creating model_9 after eliminating the brandsaab
model_9 <- lm(formula = price~ stroke + carbodyhardtop + brandhonda + cylindernumberthree + brandjaguar + carbodysedan + curbweight + carbodyhatchback + carbodywagon + enginetypedohcv + peakrpm + enginetypel + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_9 in vif
vif(model_9)
summary(model_9)

## With highest VIF and quite insignificant factor, removing curbweight
## Therefore, Creating model_10 after eliminating the curbweight
model_10 <- lm(formula = price~ stroke + carbodyhardtop + brandhonda + cylindernumberthree + brandjaguar + carbodysedan + carbodyhatchback + carbodywagon + enginetypedohcv + peakrpm + enginetypel + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_10 in vif
vif(model_10)
summary(model_10)

## looking for the variables which are highly insignificant and highest VIF among them
## Creating model_11 after eliminating the enginetypel
model_11 <- lm(formula = price~ stroke + carbodyhardtop + brandhonda + cylindernumberthree + brandjaguar + carbodysedan + carbodyhatchback + carbodywagon + enginetypedohcv + peakrpm + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_11 in vif
vif(model_11)
summary(model_11)

# Similarly, Creating model_12 after eliminating the cylindernumberthree
model_12 <- lm(formula = price~ stroke + carbodyhardtop + brandhonda + brandjaguar + carbodysedan + carbodyhatchback + carbodywagon + enginetypedohcv + peakrpm + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_12 in vif
vif(model_12)
summary(model_12)

# Similarly, Creating model_13 after eliminating the carbodywagon
model_13 <- lm(formula = price~ stroke + carbodyhardtop + brandhonda + brandjaguar + carbodysedan + carbodyhatchback + enginetypedohcv + peakrpm + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_13 in vif
vif(model_13)
summary(model_13)

# Similarly, Creating model_14 after eliminating the carbodysedan
model_14 <- lm(formula = price~ stroke + carbodyhardtop + brandhonda + brandjaguar + carbodyhatchback + enginetypedohcv + peakrpm + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_14 in vif
vif(model_14)
summary(model_14)

# Similarly, Creating model_15 after eliminating the carbodyhatchback
model_15 <- lm(formula = price~ stroke + carbodyhardtop + brandhonda + brandjaguar + enginetypedohcv + peakrpm + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_15 in vif
vif(model_15)
summary(model_15)

# Similarly, Creating model_16 after eliminating the carbodyhardtop
model_16 <- lm(formula = price~ stroke + brandhonda + brandjaguar + enginetypedohcv + peakrpm + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_16 in vif
vif(model_16)
summary(model_16)

# Similarly, Creating model_17 after eliminating the carbodyhardtop
model_17 <- lm(formula = price~ stroke + brandhonda + brandjaguar + enginetypedohcv + peakrpm + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + enginesize + brandbmw + enginelocation, data = train)
# Passing the model_17 in vif
vif(model_17)
summary(model_17)

## model_17 contain all the variables having p-vaues < 0.05
## Although hghly significaant, enginesize have the highest VIF
## Its is highly correlated to carwidth, thus can be removed
cor(carDf$enginesize,carDf$carwidth)

# Creating model_18 after eliminating the enginesize
model_18 <- lm(formula = price~ stroke + brandhonda + brandjaguar + enginetypedohcv + peakrpm + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + brandbmw + enginelocation, data = train)
# Passing the model_18 in vif
vif(model_18)
summary(model_18)

## looking for the variables which are highly insignificant and highest VIF among them
# Creating model_19 after eliminating the stroke
model_19 <- lm(formula = price~ brandhonda + brandjaguar + enginetypedohcv + peakrpm + drivewheelrwd + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + brandbmw + enginelocation, data = train)
# Passing the model_19 in vif
vif(model_19)
summary(model_19)

# Similarly, Creating model_20 after eliminating the drivewheelrwd
model_20 <- lm(formula = price~ brandhonda + brandjaguar + enginetypedohcv + peakrpm + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + enginetyperotor + brandmitsubishi + brandbmw + enginelocation, data = train)
# Passing the model_20 in vif
vif(model_20)
summary(model_20)

# Similarly, Creating model_21 after eliminating the enginetyperotor
model_21 <- lm(formula = price~ brandhonda + brandjaguar + enginetypedohcv + peakrpm + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + aspiration + brandmitsubishi + brandbmw + enginelocation, data = train)
# Passing the model_21 in vif
vif(model_21)
summary(model_21)

# Similarly, Creating model_22 after eliminating the aspiration
model_22 <- lm(formula = price~ brandhonda + brandjaguar + enginetypedohcv + peakrpm + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + branddodge + brandmitsubishi + brandbmw + enginelocation, data = train)
# Passing the model_22 in vif
vif(model_22)
summary(model_22)

# Similarly, Creating model_23 after eliminating the branddodge
model_23 <- lm(formula = price~ brandhonda + brandjaguar + enginetypedohcv + peakrpm + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandnissan + brandplymouth + brandtoyota + carwidth + brandbuick + brandmitsubishi + brandbmw + enginelocation, data = train)
# Passing the model_23 in vif
vif(model_23)
summary(model_23)

# Similarly, Creating model_24 after eliminating the brandnissan
model_24 <- lm(formula = price~ brandhonda + brandjaguar + enginetypedohcv + peakrpm + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandplymouth + brandtoyota + carwidth + brandbuick + brandmitsubishi + brandbmw + enginelocation, data = train)
# Passing the model_24 in vif
vif(model_24)
summary(model_24)

# Similarly, Creating model_25 after eliminating the brandtoyota(highly insignificant)
model_25 <- lm(formula = price~ brandhonda + brandjaguar + enginetypedohcv + peakrpm + brandrenault + enginetypeohcf + brandvolkswagen + brandmazda + brandplymouth + carwidth + brandbuick + brandmitsubishi + brandbmw + enginelocation, data = train)
# Passing the model_25 in vif
vif(model_25)
summary(model_25)

# Similarly, Creating model_26 after eliminating the enginetypeohcf
model_26 <- lm(formula = price~ brandhonda + brandjaguar + enginetypedohcv + peakrpm + brandrenault + brandvolkswagen + brandmazda + brandplymouth + carwidth + brandbuick + brandmitsubishi + brandbmw + enginelocation, data = train)
# Passing the model_26 in vif
vif(model_26)
summary(model_26)

# Similarly, Creating model_27 after eliminating the brandplymouth
model_27 <- lm(formula = price~ brandhonda + brandjaguar + enginetypedohcv + peakrpm + brandrenault + brandvolkswagen + brandmazda + carwidth + brandbuick + brandmitsubishi + brandbmw + enginelocation, data = train)
# Passing the model_27 in vif
vif(model_27)
summary(model_27)

# Similarly, Creating model_28 after eliminating the enginetypedohcv
model_28 <- lm(formula = price~ brandhonda + brandjaguar + peakrpm + brandrenault + brandvolkswagen + brandmazda + carwidth + brandbuick + brandmitsubishi + brandbmw + enginelocation, data = train)
# Passing the model_28 in vif
vif(model_28)
summary(model_28)

# Similarly, Creating model_29 after eliminating the brandvolkswagen
model_29 <- lm(formula = price~ brandhonda + brandjaguar + peakrpm + brandrenault + brandmazda + carwidth + brandbuick + brandmitsubishi + brandbmw + enginelocation, data = train)
# Passing the model_29 in vif
vif(model_29)
summary(model_29)

# Similarly, Creating model_30 after eliminating the brandhonda
model_30 <- lm(formula = price~ brandjaguar + peakrpm + brandrenault + brandmazda + carwidth + brandbuick + brandmitsubishi + brandbmw + enginelocation, data = train)
# Passing the model_30 in vif
vif(model_30)
summary(model_30)

# Refining further
## removing brandrenault, brandmazda and brandmitsubishi which have the lowest significant 
model_31 <- lm(formula = price~ brandjaguar + peakrpm + carwidth + brandbuick + brandbmw + enginelocation, data = train)
# Passing the model_31 in vif
vif(model_31)
summary(model_31)

# predicting the results in test dataset
Predict_1 <- predict(model_31,test[,-1])
test$test_price <- Predict_1
# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

# It can be seen that it is th lowest difference between rsquared and r-square of model_31 since model_1
# model_31 is the best model so far.
##########################################################
# variables which are significant in predicting the price of a car:
# 1. Brand : Jaguar, Buick and BMW are highly efficient driving variables for predicting car price
#    Having coefficients 1.444e+04, 1.145e+04 and 1.115e+04 respectively, one can easily say that these brands are driving factors for car price in US
# 2. Peakrpm : having coefficient 1.365e+00, peak rpm is also one of the main driving facors for the car price
# 3. carwidth : having coefficient 2.236e+03 , car width is also one of the main driving facors for the car price
# 4. enginelocation : having negative coefficient -2.378e+04, engnelocation shows that rear engines are high priced