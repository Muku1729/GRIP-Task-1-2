#UnSupervised ML.
#TASK -2  (GRIP)
#Name: Mukesh Dhariwal

#Loading the required packages --->
library(ggplot2)
library(cluster)
#Importing the data --->
setwd("F:\\Stats")
datairis=read.csv("Iris.csv")
attach(datairis)
#Structure and Summary of the Dataset --->
str(datairis)
summary(datairis)
#Preparing 
iris.s<-datairis
iris.s$Species<-NULL
#Predicting the optimum number of clusters by Elbow Method
set.seed(1234)#for constant reproducibility 
wcss<-vector()
kmax<-10
for (i in 1:kmax) wcss[i] <- sum(kmeans(iris.s,i)$withinss)
plot(1:kmax,wcss,type='b',main=paste('Elbow Method'),xlab='Number of Clusters',ylab='WCSS')

#The variability in WCSS significantly decreases at 3 and after that there is no significant difference in WCSS
#Hence K(centers)=3

#Visualizing the data

set.seed(1221)
kmean <- kmeans(datairis[,1:4], centers = 3, iter.max = 300)
kmean
table(kmean$cluster, datairis$Species)
clusplot(datairis[,c(1,2)],
         kmean$cluster,
         shade = T,
         color = T,
         lines = 0,
         span = T,
         main = "Cluster Iris",
         xlab = "Sepal Length",
         ylab = "Sepal Width",
         plotchar = T)

#Boxplot

ggplot(datairis)+geom_boxplot(aes(x=Species,y=SepalLengthCm,fill=Species))+theme_bw()
ggplot(datairis)+geom_boxplot(aes(x=Species,y=SepalWidthCm,fill=Species))+theme_grey()
ggplot(datairis)+geom_boxplot(aes(x=Species,y=PetalLengthCm,fill=Species))+theme_bw()
ggplot(datairis)+geom_boxplot(aes(x=Species,y=PetalWidthCm,fill=Species))+theme_bw()


#Sepal-Length vs Sepal-Width
ggplot(datairis)+geom_point(aes(x=SepalLengthCm,y=SepalWidthCm),stroke=0.5)+facet_wrap(~Species)+
  labs(x='Sepal Length',y='Sepal Width')+theme_minimal()
#Petal-Length vs Petal-Width
ggplot(datairis)+geom_point(aes(x=PetalLengthCm,y=PetalWidthCm),stroke=1)+facet_wrap(~Species)+
  labs(x='Petal Length',y='Petal Width')+theme_minimal()
#Sepal-Length vs Petal-Length
ggplot(datairis)+geom_point(aes(x=SepalLengthCm,y=PetalLengthCm),stroke=0.1)+facet_wrap(~Species)+
  labs(x='Sepal Length',y='Petal Length')+theme_minimal()
#Sepal-Width vs Petal-Width
ggplot(datairis)+geom_point(aes(x=SepalWidthCm,y=PetalWidthCm),stroke=0.1)+facet_wrap(~Species)+
  labs(x='Sepal Width',y='Petal Width')+theme_minimal()
