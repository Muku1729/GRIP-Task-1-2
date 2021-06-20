#The Sparks Foundation 
#Predicting Score of Student Using Supervised ML.
#TASK - 1 (GRIP)
#Name : Mukesh Dhariwal
#Importing Library to be Used--->
library(ggplot2)
#Importing the data--->

setwd("C:\\Users\\KING\\Documents")
task1data=read.csv("Task1.csv")
attach(task1data)
#Structure of the data
str(task1data)
#Checking for for outliers----
boxplot(task1data$Scores)
#No outliers is found in the data

#Correlation between Hours of Study and Score

correlation=lm(Scores~Hours,data=task1data)
print(correlation)
summary(correlation)

#Scores and No. of hours spent studying shows a strong positive linear relationship

#Plot the data----
base<- ggplot(task1data, aes(x=Hours, y=Scores, col = "Blue"))
base+geom_point()+labs(xlab ="Hours",ylab ="Scores",title = "Relation b/w Study_Hours and Scores",col="blue")+
  geom_smooth(method ="lm" )  


#Predicting the score for a student who studies for 9.25 hrs/ day?--->
p = data.frame("Hours" = 9.25)
predicted_score<-predict(correlation,p)
print(predicted_score)

#Predicted Score is 92.9

