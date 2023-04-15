###UPLOAD THE DATASET


data_airline=airline_passenger_satisfaction
data_airline$satisfaction

###UPLOAD ALL THE LIBRARY THAT WE ARE GOING TO NEED

library(tidyverse)
library(dplyr)

library(RColorBrewer)
library(forcats)
library(readxl)
library(haven)
library(broom)
library(psych)
library(modelr)
library(ggplot2)
library(lubridate)
install.packages("mltools")
library(mltools)
install.packages("data.table")
library(data.table)
library(tidyr)
library(dplyr)
library(gplots)
library(plyr)
library(MASS)
library(heplots)


data_airline %>% 
  DataExplorer::create_report()


###### DATA PREPARATION 
data_airline
library(dplyr)
glimpse(data_airline)
summary(data_airline)

n_row=nrow(data_airline)
n_row
#129880

n_col=ncol(data_airline)
n_col
#24

anyNA(data_airline)


head(data_airline)
missing(data_airline)

##first row is the id number
##we want to delete the id from our dataset

data_airline=data_airline[,-1]
head(data_airline)

data_airline$departure_delay_in_minutes

complete.cases(data_airline$departure_delay_in_minutes)


missing(data_airline)
data_airline=data_airline[complete.cases(data_airline),]
nrow(data_airline)
#129487
#were deleted 
129880-129487
##393 were the uncomplete cases


range(data_airline$departure_delay_in_minutes)

range(data_airline$departure_arrival_time_convenient)

range(data_airline$arrival_delay_in_minutes)

data_airline$type_of_travel=as.factor(data_airline$type_of_travel)
data_airline$type_of_travel


data_airline$customer_class=as.factor(data_airline$customer_class)
data_airline$customer_class


data_airline$customer_type=as.factor(data_airline$customer_type)
data_airline$customer_type

data_airline$satisfaction=as.factor(data_airline$satisfaction)
data_airline$satisfaction

data_airline$Gender=as.factor(data_airline$Gender)
data_airline$Gender

###problem:d
levels(data_airline$satisfaction)
data_airline$satisfaction <- ifelse(data_airline$satisfaction=="satisfied",1,0)
data_airline$satisfaction=as.factor(data_airline$satisfaction)
head(data_airline$satisfaction)
data_airline$satisfaction

attach(data_airline)
satisfaction





par(mfrow=c(1,1))
boxplot(data_airline$seat_comfort)


hist(data_airline$ease_of_online_booking, col="green")

hist(data_airline$seat_comfort,col="blue")

plot(data_airline$type_of_travel,col="orange")

#####unit of measure is the nautical mile


###we have a majority of short flights



##Response variable distribution
library(tidyverse)

response_plot=ggplot(data_airline, aes(x = satisfaction)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
response_plot

plot_by_gender <- ggplot(data_airline, aes(x = Gender)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
plot_by_gender


plot_by_class <- ggplot(data_airline, aes(x = customer_class)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
plot_by_class

plot_by_type=ggplot(data_airline, aes(x = customer_type)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
plot_by_type
data_airline$departure_delay_in_minutes


##Distribution of age for satisfied and not satisfied

plot_age<- ggplot(data = data_airline, aes(age, color = satisfaction))+
  geom_freqpoly(binwidth = 5, size = 1)
plot_age

##flight distance
plot_flight_dis<- ggplot(data = data_airline, aes(flight_distance, color = satisfaction))+
  geom_freqpoly(binwidth = 5, size = 1)
plot_flight_dis

##time delay plot
##flight distance
plot_flight_delay_arr<- ggplot(data = data_airline, aes(arrival_delay_in_minutes, color = satisfaction))+
  geom_freqpoly(binwidth = 5, size = 1)
plot_flight_delay_arr

plot_flight_delay_dep<- ggplot(data = data_airline, aes(departure_delay_in_minutes, color = satisfaction))+
  geom_freqpoly(binwidth = 5, size = 1)
plot_flight_delay_dep



data_airline

#Plot online booking
plot_online_book <- ggplot(data_airline, aes(x = ease_of_online_booking)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

plot_online_book


##plot for leg room
plot_leg_room<- ggplot(data_airline, aes(x = leg_room_service)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
plot_leg_room
##plot arrival time convinient
plot_time_convenient<- ggplot(data_airline, aes(x = departure_arrival_time_convenient)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
plot_time_convenient



##Does satisfied or not differ significantly for
#some particular factor?


range(data_airline$age)
is.factor(data_airline$satisfaction)

boxplot(age~satisfaction, data=data_airline)

plotmeans(age~satisfaction, data=data_airline)

head(data_airline)

plotmeans(seat_comfort~satisfaction, data=data_airline)
plotmeans(cleanliness~satisfaction,data=data_airline)
plotmeans(departure_arrival_time_convenient~satisfaction,data=data_airline)

plotmeans(flight_distance~satisfaction,data=data_airline)

##analyze the same for departure delay in minutes:
boxplot(data_airline$departure_delay_in_minutes)
hist(data_airline$departure_delay_in_minutes)



##Chek for flight distance and make the log of it
hist((data_airline$flight_distance))
boxplot((data_airline$flight_distance))

hist(log(data_airline$flight_distance))
boxplot(log(data_airline$flight_distance))
##I will introduce the variable with ist log value into the model

data_airline$flight_distance=log(data_airline$flight_distance)

##Delete the outliers from flight ditance:
outli=boxplot(data_airline$flight_distance)$out
max(outli)##I use this value to delete the outliers
hist(data_airline$flight_distance) 
summary(boxplot(data_airline$flight_distance))

data_airline=data_airline[which(data_airline$flight_distance>4),]

boxplot(data_airline$flight_distance)
##we can see from the boxplot that there are no outliers anymore.

boxplot(data_airline$age)
hist(data_airline$age)

####TRASFORM THE TWO DELAY VARIABLES INTO DUMMY
##DELAY=1,NO DELAY=0
##first check if the variables are numeric
is.numeric(data_airline$arrival_delay_in_minutes)
is.numeric(data_airline$departure_delay_in_minutes)

data_airline$arrival_delay_in_minutes
data_airline$departure_delay_in_minutes

data_airline$arrival_delay_in_minutes=ifelse(data_airline$arrival_delay_in_minutes>0,1,0)
data_airline$arrival_delay_in_minutes=as.factor(data_airline$arrival_delay_in_minutes)

data_airline$arrival_delay_in_minutes

data_airline$departure_delay_in_minutes=ifelse(data_airline$departure_delay_in_minutes>0,1,0)
data_airline$departure_delay_in_minutes=as.factor(data_airline$departure_delay_in_minutes)
data_airline$departure_delay_in_minutes

plot_by_dep_del=ggplot(data_airline, aes(x =departure_delay_in_minutes )) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
plot_by_dep_del

data_airline$arrival_delay_in_minutes
plot_by_arr_del=ggplot(data_airline, aes(x =arrival_delay_in_minutes )) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
plot_by_arr_del

data_airline %>% 
  DataExplorer::create_report()



###OUTLIERS
complete_model=glm(satisfaction~.,family=binomial,data=data_airline)
summary(complete_model)
##All the variables seems to be pretty significant.
# AIC 86141


library(car)
outlierTest(complete_model)

##we reject the null hypothesis and this means that the observation 60674
#is actually an outlier


data_airline[60674,]
##we remove this only outlier from the dataste
data_airline[-60674,]

###Check the multicollinearity
library(car)
vif(complete_model)
sqrt(vif(complete_model))>2

###We don't have any collinearity problem

###In order to improve the interpretability and the prediction of our model
##we want to do model selection

##Model selection
##-Best subset regression
library(leaps)
?regsubsets
fitregfull=regsubsets(satisfaction~.,data=data_airline,nvmax=23)
summfirreg=summary(fitregfull)
names(summfirreg)
plot(summfirreg$cp)
which.min(summfirreg$cp)
#23
points(23,summfirreg$cp[23],pch=20,col="red")


##we obtain that the number of variables hat should be included in the model
#are 23 (all the varibles)
##in order to understand better the process thanks to which we choose that we had to keep all the variables

plot(fitregfull,scale="Cp")
coef(fitregfull,23)



##Forward selection
forwardfit=regsubsets(satisfaction~.,data=data_airline,nvmax=23, method="forward")
summary(forwardfit)
plot(forwardfit,scale="Cp")
## Also in this case we take all the variables

step(complete_model)

##



###Dividing the dataset into training and test set
set.seed(123)
index=sample(2,nrow(data_airline),replace=T,prob=c(0.8,0.2))
train_sample=data_airline[index==1,]
test=data_airline[index==2,]
index
train_sample

test



##LOGISTIC REGRESSION
model=glm(satisfaction~., family=binomial,data=train_sample)
summary(model)
sqrt(vif(model)^2)>2
#AIC 68606


##Confusion matrix
prob1 <- predict(model, test, type="response")
pred1 <- ifelse(prob1 > 0.5,1,0)
table(Predicted = pred1, Actual = test$satisfaction)

#Actual
#Predicted       not satisfied satisfied
#Not satisfied          13187      1895
#satisfied              1456     9337



##Accuracy
pr2 <- predict(model, train_sample, type="response")
pred2 <- ifelse(pr2 > 0.5,1,0)
tab1 <- table(Predicted = pred2, Actual = train_sample$satisfaction)
tab2 <- table(Predicted = pred1, Actual = test$satisfaction)

##Confusion matrix train
library(caret)
confusionMatrix(
  as.factor(pred2),
  as.factor(train_sample$satisfaction),
  positive = "1" 
)
##Accuracy : 0.8755

##Test (doesn't work anymore)
# Test
test$satisfaction
pred1
confusionMatrix(
  as.factor(pred1),
  as.factor(test$satisfaction),
  positive = "1" 
)

##Accuracy 0.8705

library(pROC)
lprob2 <- predict(model, test, type="response")
lprob2
test_roc = roc(test$satisfaction ~ lprob2, plot = TRUE, print.auc = TRUE)
as.numeric(test_roc$auc)
##0.924923
##which is high (pretti high)


#############
###LINEAR DISCIMINANT ANALYSIS
##it works just with quatitatives variables as independent variable 
##
head(train_sample)
train_sample[,c(1,2,4,5,21,22)]

train_sample_lda=(train_sample[,-c(1,2,4,5,21,22)])
ncol(train_sample_lda)
head(train_sample_lda)

data_sample_lda=cbind(scale(train_sample_lda[,-17]),train_sample_lda$satisfaction)
test_lda=test[,-c(1,2,4,5,21,22)]
test_lda=cbind(scale(test_lda[,-17]),test_lda$satisfaction)

library(MASS)

LDA=lda(satisfaction~.,data = train_sample_lda)
LDA
plot(LDA)


##from the plot we can say that the two groups doesn't seem
#to be quite distant and so the discriminant analysis does not work well
#with this kind of dataset.

##Accuracy training data:
p2=predict(LDA,train_sample_lda)$class
tab2=table(p2,train_sample_lda$satisfaction)
sum(diag(tab2))/sum(tab2)
#0.8224341

LDApred=predict(LDA,test,type="response")
LDApred_class=predict(LDA,test,typr="response")$class
tab1=table(LDApred_class,test$satisfaction)
tab1
##Accuracy
sum(diag(tab1))/sum(tab1)
##0.8160773


#LDApred         not satisfied satisfied
#not satisfied         12463      2579
#satisfied              2180      8653






###RANDOM FOREST
##Since we have many features we wanted to try to do random forest
library(randomForest)
set.seed(123)
##we set the seed in order to make it repetable
randf=randomForest(satisfaction~.,data=train_sample)
print(randf)
summary(randf)
install.packages("caret")
library(caret)
install.packages("party")
library(party)
train_sample$satisfaction
p1_rf=predict(randf,train_sample)
head(p1_rf)

confusionMatrix(p1_rf, train_sample$satisfaction,positive="1")
#Accuracy 0.9999 

##Prediction with the test data:
p2_rf=predict(randf,test)
confusionMatrix(p2_rf,test$satisfaction,positive="1")
##Accuracy 0.9632

##ROC Curve
randf
lprob3_num <- predict(randf, test)
lprob3_num
test$satisfaction

test_roc_rf = roc(test$satisfaction ~ lprob3_num, plot = TRUE, print.auc = TRUE)
as.numeric(test_roc_rf$auc)


##Error rate
plot(randf)
###we can see that after 300 trees the error doesn't improve anymore
##from the plot we choose the number of tree
##Tune random forest model
train_sample[,c(1:23)]
tuneRF(train_sample[,-23],train_sample[,23],stepFactor=0.5,plot=TRUE,ntreeTry=300, improve=0.05,trace=TRUE)


##Number of models for the trees
hist(treesize(randf),main="Number of nodes for the trees",col="green")
##Variable importance

varImpPlot(randf)


