install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
data <- read.csv("C:/Data Science with R/HospitalCosts.csv")
head(data)

#1 To record the patient statistics, the agency wants to find the age 
# category of people who frequent the hospital and has the maximum expenditure. 

agedistribution <- hist(data$AGE, col = "lightblue", xlab = "Age", 
          main = "Hospital Visits by Age")
table(data$AGE)

# shows the ages of people who frequent the hospital

summarise(group_by(data,AGE), sum(TOTCHG))

maxexpend <- ggplot(data, aes(x=AGE, y=TOTCHG/1000)) + 
  geom_bar(stat="identity") +
  labs(x="Age",y="Total Hospital Costs in Thousands",
       title = "Hospital Maximum Expenditure by age")

plot(maxexpend)

# shows which age group has the maximum expenditure

#2 In order of severity of the diagnosis and treatments and to find 
# out the expensive treatments, the agency wants to find the diagnosis 
# related group that has maximum hospitalization and expenditure. 

drg <- summarise(group_by(data,APRDRG), sum(LOS), sum(TOTCHG))
drg

maxSumLos <- drg[which.max(drg$`sum(LOS)`),]
maxSumTOTCHG <- drg[which.max(drg$`sum(TOTCHG)`),]
maxSumLos 
maxSumTOTCHG

# shows that the group 640 has both the most maximum hospitalization and expenditure

#3 To make sure that there is no malpractice, the agency needs to analyze if the race 
# of the patient is related to the hospitalization costs.

lm.out <- lm(data$TOTCHG~data$RACE)
lm.out

summary(lm.out)

# shows that race is not statistically signficicant, and thus, not related to hospital costs

#4 To properly utilize the costs, the agency has to analyze the severity of the hospital 
# costs by age and gender for proper allocation of resources. 

summarize(group_by(data,AGE,FEMALE), sum(TOTCHG))
summarize(group_by(data,FEMALE), sum(TOTCHG))

# shows basic info, will use a graph to have a better visual understanding

agegendercost <- ggplot(data, aes(x=AGE, fill= factor(FEMALE), y=TOTCHG/1000)) + 
  geom_bar(stat="identity") +
  labs(x="Age",y="Total Hospital Costs in Thousands",
       title = "Severity of Hospital Costs by age and gender") +
  scale_fill_discrete(name= "Gender", labels=c("Male", "Female"))

plot(agegendercost)


# shows the distribution or severity of hospital costs by age and gender

#5 Since the length of stay is the crucial factor for inpatients, the agency 
# wants to find if the length of stay can be predicted from age, gender, and race. 

lengthstay <- lm(data$LOS~data$AGE+data$FEMALE+data$RACE)
lengthstay
summary(lengthstay) 

# shows that with a small R-squared and large p-values length of stay cannot 
# be predicted from age, gender and race

#6 To perform a complete analysis, the agency wants to find the variable that mainly 
# affects the hospital costs.

hospcost <- lm(data$TOTCHG~data$AGE + data$FEMALE + data$LOS + data$RACE + data$APRDRG)
summary(hospcost)

# shows that length of stay has the lowest p-value and highest t value, as well as
# the largest coefficient value, and thus has the largest effect on hospital costs