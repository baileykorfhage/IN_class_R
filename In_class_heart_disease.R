library(tidyverse)
dataset=read.csv("heart.data.csv")

#Explore the data

view(dataset)
glimpse(dataset)
head(dataset)
length(dataset)
names(dataset)
summary(dataset)

#Missing values for biking

colSums(is.na(dataset))

ggplot(data=dataset,aes(biking))+geom_density()

biking_median=median(dataset$biking, na.rm = TRUE)

dataset$biking=ifelse(is.na(dataset$biking),biking_median,dataset$biking)

colSums(is.na(dataset))

#Missing values for smoking

ggplot(data=dataset,aes(smoking))+geom_density()

smoking_median=median(dataset$smoking, na.rm = TRUE)

dataset$smoking=ifelse(is.na(dataset$smoking),smoking_median,dataset$smoking)

colSums(is.na(dataset))

#Missing value for heart disease

ggplot(data=dataset,aes(heart.disease))+geom_density()

disease_median=median(dataset$heart.disease, na.rm = TRUE)

dataset$heart.disease=ifelse(is.na(dataset$heart.disease),disease_median,dataset$heart.disease)

colSums(is.na(dataset))

#Splitting the data into two sets

library('caTools')
split=sample.split(dataset$heart.disease,SplitRatio = .8)
train=subset(dataset,split=TRUE)
test=subset(dataset,split=FALSE)

#Multiple linear regression training

names(dataset)
mlr=lm(formula=heart.disease~.,data=train)
summary(mlr)

# formula for mlr is  14.959 -.200*biking +.1795*smoking

#both biking and smoking are significant because the p value is less than .05

#Mean square error
summ=summary(mlr)
mse=(mean(summ$residuals^2))
paste("The mean square error:",mse)

#R-square
summary(mlr)

#This model has an Rsquared value of 97.65%, which means that when predicting
#if someone will have heart disease, smoking and biking will correctly predict
#it 98% of the time

#testing set prediction
y_pred=predict(mlr,newdata = test)
data=data.frame(test$heart.disease,y_pred)
head(data)

#The values are very close to the prediction model

#use validation

new=read.csv('Heart_validation.csv')
new_x=new[c(1,2)]
new_x

data.frame(new[c(3)],predict(mlr,newdata=new_x))












