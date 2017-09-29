oil<- read.csv("/Users/madhavsarma/Desktop/database.csv")
head(oil)
names(oil)
str(oil)
# getting an idea about the dataset we have
library(ggplot2)
qplot(x=Accident.Year, data=oil) 
#max accidents occured in 2015. In each year from 2014, there were more than 400 accidents

summary(oil$PipelineFacility.Name)
oil_pipeline.Facility.Name<-subset(oil,!is.na(PipelineFacility.Name))
FacilityMoreThan10 <- subset(oil, summary(PipelineFacility.Name) > 10)
summary(FacilityMoreThan10$PipelineFacility.Name)
#Wyoming pipeline company has maximum breakages with 121 and rest below 20. So it has to be extra careful with its maintainace
#HOW TO GET THE PIPELINE COMPANY NAMES WHICH HAVE MORE THAN 10 ACCIDENTS.

qplot(x=Pipeline.Location, data=oil)
summary(oil$Pipeline.Location) #Most accidents were on shore
summary(oil$Pipeline.Type) # max<- Aboveground with 1475 and least with transition area 16
summary(oil$Liquid.Subtype) #Anhydrous ammonia <- max type (1446)
oil1<-subset(oil,!is.na(Liquid.Name))
head(oil1)
summary(oil1$Liquid.Name)  #ethane with 23 max..... 2573 empty

summary(oil$Accident.State) #Maximum accidents in texas with more than 10 times the next one
summary(oil$Accident.County) #Maximum in harris and jefferson
summary(oil$Cause.Category) #MATERIAL/WELD/EQUIP FAILURE maximum with 1435, trice more than the next
summary(oil$Net.Loss..Barrels.) #Seems to have many outliers (we'll explore this later)
summary(oil$All.Costs)
qplot(x=Accident.State, y=All.Costs, data=oil,ylim=c(0,3000000))
#Most of the accidents have led to costs < 1000000 although there are considerable no 
#of accidents leading to costs >1000000
max(by(oil$All.Costs,oil$Accident.State,sum))
#With one major accident, MI state has the Maximum value of the total costs.
min(by(oil$All.Costs,oil$Accident.State,sum))
# ME state has the least
# outlier value of All costs in MI state with All costs=840500000

#by(oil$All.Costs/length(oil$Accident.State),oil$Accident.State,sum)
#length(oil$Accident.State)



#Considering injuries caused in different states
by(oil$All.Injuries,oil$Accident.State,sum)
oil2<-subset(oil,!is.na(All.Injuries))
oil2$All.Injuries
max(by(oil2$All.Injuries,oil$Accident.State,sum))
qplot(x=Accident.State, y=All.Injuries, data=oil)

#most accidents havent caused any injuries to the public and among those caused Texas has most
ggplot(aes(x=Accident.State, y=Net.Loss..Barrels.), data=oil)+geom_point()
by(oil$Net.Loss..Barrels.,oil$Accident.State,sum)
#Texas has huge loss of oil barrels with frequent accidents although West virigina had an accident with the highest loss in barrels

qplot(x=Cause.Category,y=Net.Loss..Barrels.,data=oil)+
  facet_wrap(~Accident.State)
# Here in each state we can see net barrel loss for each category

(by(oil$All.Costs,oil$Accident.State,sum))
ggplot(aes(x=Accident.State, y=All.Costs), data=oil)+geom_point()+
  scale_y_sqrt()


