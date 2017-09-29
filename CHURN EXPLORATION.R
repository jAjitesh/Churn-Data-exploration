churn <- read.csv("/Users/madhavsarma/Desktop/KDD/churn1.csv")
churn$Churn
library(ggplot2)
qplot(data = churn, x=Churn)
count(churn$Churn)
prop.table(table(churn$Churn))*100

#WORKING OUT WITH CATEGORICAL VALUES
qplot(data = churn, x= Intl.Plan)
by(churn$Churn,churn$Intl.Plan,count)
# There are very few number of people churned without Intl plan
churn_Intl.Plan <- subset(churn, Intl.Plan == "yes")
churn_Intl.Plan
count(churn_Intl.Plan$Churn)
prop.table(table(churn_Intl.Plan$Churn))*100
#We can see that 42% of the people with intl plan have churned. Therefore the company has to 
#look into its intl plan scheme.
#CHECK CROSS TAB 

qplot(data = churn, x= VMail.Plan)
by(churn$VMail.Plan,churn$Churn,count)
churn_VMail.Plan <- subset(churn, VMail.Plan == "yes")
churn_VMail.Plan
count(churn_VMail.Plan$Churn)
prop.table(table(churn_VMail.Plan$Churn))*100
#Not a significant difference can be seen here as only 8% churned and it depends on the company
#to what extend it want to take this seriously

qplot(data = churn, x= VMail.Plan)
by(churn$VMail.Plan,churn$Churn,count)
churn_VMail.Plan <- subset(churn, VMail.Plan == "no")
churn_VMail.Plan
count(churn_VMail.Plan$Churn)
prop.table(table(churn_VMail.Plan$Churn))*100
#Without VMail plan around 17% of the people churned which is more than people having VMail plan

VMail_Intl.Plan<-subset(churn, VMail.Plan=="yes" & Intl.Plan=="yes")
count(VMail_Intl.Plan$Churn)
prop.table(table(VMail_Intl.Plan$Churn))*100
#Around 40% of the people having both the plans churned which the company has to note seriously
#make plans to combine or satisfy both

VMail_Intl.Plan<-subset(churn, VMail.Plan=="yes" & Intl.Plan=="no")
count(VMail_Intl.Plan$Churn)
prop.table(table(VMail_Intl.Plan$Churn))*100
#Only 5% churned so can be ignored

VMail_Intl.Plan<-subset(churn, VMail.Plan=="no" & Intl.Plan=="yes")
count(VMail_Intl.Plan$Churn)
prop.table(table(VMail_Intl.Plan$Churn))*100
#Around 43% of the people churned without a voice mail plan and with a Intl plan, which is huge. 
#Company has to make sure that their voice mail is being taken by the customers along with improving Intl plan

str(churn)
by(churn$State,churn$Churn,count)
qplot(data = churn, x = Churn)+
  facet_wrap(~State)
#Cannot observe any abnormalitites

#EXPLORING NUMERIC VALUES

qplot(data = churn, x = Account.Length)
#Data looks normally distributed and symmetric

qplot(data = churn, x = Area.Code)
count(churn$Area.Code)
count(churn,vars = c("State","Area.Code"))
#Area code is only 408,415 and 510. That is the might be anomalous

qplot(data = churn, x = VMail.Message)
summary(churn$VMail.Message)
#Half of the data has VMail.messages = 0. 

VMail.Message_zero<- subset(churn, VMail.Message == 0)
VMail.Message_zero
count(VMail.Message_zero$Churn)

VMail.Message_Nozero<- subset(churn, VMail.Message != 0)
VMail.Message_Nozero
count(VMail.Message_Nozero$Churn)
qplot(data = VMail.Message_Nozero, x = VMail.Message)

qplot(data = churn, x = Day.Mins)
boxplot(churn$Day.Mins)
summary(churn$Day.Mins)
Day.Mins_lessThanMean <- subset(churn, Day.Mins < 180)
count(Day.Mins_lessThanMean$Churn)
prop.table(table(Day.Mins_lessThanMean$Churn))*100
Day.Mins_moreThanMean <- subset(churn, Day.Mins >= 180)
count(Day.Mins_moreThanMean$Churn)
prop.table(table(Day.Mins_moreThanMean$Churn))*100
#There's slightly more number of people churned as the number of Day.minutes kept increasing
#ACTUALLY ANALYSIS HAS TO TO BE DONE TO EACH QUARTILE! DO LATER

#SIMILARLY ANALYSE FOR THE REMAINING NUMERIC VARIABLES
custservicecalls<-churn$CustServ.Calls
qplot(data = churn, x = CustServ.Calls)
summary(churn$CustServ.Calls)
pop_sd <- sd(custservicecalls)*sqrt((length(custservicecalls)-1)/(length(custservicecalls)))
pop_mean <- mean(custservicecalls)
pop_sd
pop_mean
zscore.custservicecalls <- (churn$CustServ.Calls - pop_mean) / pop_sd
qplot(data = churn, x = zscore.custservicecalls)
#WHY DO WE HAVE TO FIND Z SCORE HERE WHEN THE DISTRIBUTION BEFORE AND AFTER LOOKS THE SAME
#HOW WILL NORMALIZATION EASE OUR PROCESS?

str(churn)
churn_numericData <- subset( churn, select = -c(Churn, Intl.Plan, VMail.Plan, State, Phone ) )
cor(churn_numericData)
#We can remove one of the two highly correlating variables from our exploration as it wouldnt be helpful


