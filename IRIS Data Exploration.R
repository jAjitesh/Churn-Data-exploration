#EDA Homework name:
#TASK 1
iris
str(iris)
summary(iris)
#library(corrplot)
#cor(iris$Sepal.Length, iris$Sepal.Width)
iris
my_data1<- iris[,c(1,2,3,4)] #only numeric variables has to be considered
rem<-cor(my_data1)
rem
round(rem, 2) #to round off to 2 digits


iris
setosa.cor<- iris[iris$Species == "setosa",c(1,2,3,4)] #only numeric variables has to be considered
rel<-cor(setosa.cor)
round(rel, 2) #to round off to 2 digits

versicolor.cor<- iris[iris$Species == "versicolor",c(1,2,3,4)] #only numeric variables has to be considered
rel<-cor(versicolor.cor)
round(rel, 2) #to round off to 2 digits

virginica.cor<- iris[iris$Species == "virginica",c(1,2,3,4)] #only numeric variables has to be considered
rel<-cor(virginica.cor)
round(rel, 2) #to round off to 2 digits


#1.
#Recall the example that focused on the 'iris' dataset. 
#Using this dataset, calculate the three separate correlation 
#matrices with the four variables, which correspond to the 
#three levels in the Species factor.
#use cor example from Step 1 and be sure to review help for cor and cov
#
#2. Is the previous assertion that Petal.Length
#is highly positively correlated with Petal.Width justi???ed?
#
#3. Is the previous assertion that Petal.Length
#is highly positively correlated with Sepal.Length justi???ed?
#
#4. Is the previous assertion that Petal.Length
#and Petal.Width are negatively correlated with Sepal.Width justi???ed?
#
#5. Is the previous assertion that Sepal.Length and Sepal.Width
#are uncorrelated (but negatively correlated, if at all) justi???ed?
#
#6. What conclusions can you draw from the results of the correlation?
#
#TASK 2
#1. Complete EDA on the CO2 file that comes with R
#start by looking up CO2 in R help
#then examine CO2
library(effects)
library(mosaic)
CO2
write.csv(CO2, file ="/Users/madhavsarma/Desktop/CO2.csv", row.names=FALSE)
#and create data frame CO2df
CO2df <- CO2
CO2df

#first plan your approach to EDA
#then using examples from class, including Step 1 and 2 EDA,
#explore the data and prepare a report on your findings
#remember to standardize and/or bin numeric variables
#if necessary
#we are looking for the affect on CO2 update of chilling as
#compared to nonchilling
#turn in your report as a PDF
#and your R code file

CO2df
head(CO2)
tail(CO2)
str(CO2)
summary(CO2)
library(ggplot2)
pop_sd_uptake <- sd(CO2$uptake)*sqrt((length(CO2$uptake)-1)/(length(CO2$uptake)))
pop_mean_uptake <- mean(CO2$uptake)
pop_sd_uptake
pop_mean_ptake
zscore.uptake <- (CO2$uptake - pop_mean_uptake) / pop_sd_uptake
zscore.uptake

ggplot(data=CO2, aes(x=conc, y=uptake, fill=CO2$Type)) + geom_bar( position="dodge",stat = "identity")
#Quebec uptake is always greater than mississippi plants uptake at any given concentration
#and they both increase linearly with inc in concentration
ggplot(data=CO2, aes(x=conc, y=uptake, fill=CO2$Treatment)) + geom_bar( position="dodge",stat = "identity")
#The non chilled treatment's uptake is always greater than the chilled treatment's uptake at any given conc
#Both uptakes increases linearly with inc in conc
ggplot(data=CO2, aes(x=conc, y=uptake, fill=CO2$Plant)) + geom_bar( position="dodge",stat = "identity")
#mc(1,2,3) plant's uptake is the least compared to other plants at any conc
ggplot(data=CO2, aes(x=Plant, y=uptake, fill=CO2$Treatment)) + geom_bar( position="dodge",stat = "identity")
#uptake increases linearly with inc in conc
#n type of plants has higher uptake rate than c type plants.
#Qn(1,2,3) has highest uptake with each > 40
library(descr)
crosstab(CO2,row.vars= "uptake" , col.vars ="Plant" , type = "r")



abc<- xtabs(~Plant + uptake,data=CO2)
colSums(abc)
abc
par(mfrow = c(2,1))
abc<- seq(7, 46,by= 1)
hist(CO2$conc, breaks=abc,xlim=c(5,50), main="conc",
     xlab="co2 conc",ylab="Counts")
hist(CO2$conc, breaks=40,xlim=c(0,1000), main="conc",
     xlab="co2 conc",ylab="Counts")

