setwd("C:/Users/sumas/Documents/Data science/R progamming/Project/Projects for Submission/Healthcare/Healthcare")
library(dplyr) #### loading dplyr package ###



Hospital_data <- read.csv("HospitalCosts.csv")
print(Hospital_data)
View(Hospital_data)

### the age category of people who frequently visit the hospital ####
#### method 1 #####
table(Hospital_data$AGE)
#### method 2 #####
Hospital_data%>% group_by(AGE) %>% summarise(n())
#### method 3 #####
hist(Hospital_data$AGE,xlab = "Age",ylab = "no of visits",breaks = 18)
#### method 4 #####
summary(as.factor(Hospital_data$AGE))

### the age category of people who have maximum expenditure ####
#### method 1 #####
aggregate(Hospital_data$TOTCHG, by=list(Hospital_data$AGE), FUN=sum)
#### method 2 #####
Hospital_data%>% group_by(AGE)%>% summarise(sum(TOTCHG))
#### method 3######
sort(tapply(Hospital_data$TOTCHG,Hospital_data$AGE,FUN=sum),decreasing = TRUE)
#### method 4 ######
barplot(tapply(Hospital_data$TOTCHG,Hospital_data$AGE,FUN=sum))




### diagnosis related group that has maximum expenditure.####
#### method 1 #####
Expenditure_diagnosis <- aggregate(Hospital_data$TOTCHG, by=list(Hospital_data$APRDRG), FUN=sum)
Expenditure_diagnosis
Expenditure_diagnosis %>% arrange(Expenditure_diagnosis$x,desc(x))
##### method 2 #####
sort(tapply(Hospital_data$TOTCHG,Hospital_data$APRDRG,FUN=sum),decreasing = TRUE)

#### diagnosis related group that has maximum hospitalization #####
#### method 1 #####
LOS_diagnosis <- aggregate(Hospital_data$LOS, by=list(Hospital_data$APRDRG), FUN=sum)
LOS_diagnosis
LOS_diagnosis %>% arrange(LOS_diagnosis$x,desc(x))
##### method 2 #####
sort(tapply(Hospital_data$LOS,Hospital_data$APRDRG,FUN=sum),decreasing = TRUE)
###### method 3 ####
table(Hospital_data$APRDRG)



####Race and expenditure###

summary(as.factor(Hospital_data$RACE)) ### indicates presence of a missing value
Hospital_data[is.na(Hospital_data$RACE),"RACE"] <- median(Hospital_data$RACE,na.rm = TRUE) ### adding the missing value to the highest cost group ###
table(Hospital_data$RACE)
### Ho= Race is not influencing hospital costs #### NULL hypothesis
### Ha= Race is influencing hospital costs #### Alternative hypothesis
model <- aov(TOTCHG~RACE,Hospital_data)
summary(model)### reject null hypothesis ####

######Gender,age and hospital cost#####

model1 <- lm(TOTCHG~AGE+FEMALE,Hospital_data)
model1
summary(model1) #### depends on age and gender####


##### to find if the length of stay can be predicted from age, gender, and race####

model2 <- lm(LOS~AGE+FEMALE+RACE,Hospital_data)
model2
summary(model2)


##### to find the variable that mainly affects the hospital costs ####

model3 <- lm(TOTCHG~AGE+FEMALE+RACE+LOS+APRDRG,data = Hospital_data)
model3
summary(model3)

####As it is apparent from the coefficient values, Age, Length of stay (LOS) and patient refined diagnosis related groups(APRDRG) have three stars (***) next to it. So they are the ones with statistical significance
### Gender and RACE are not significant. build a model after removing RACE and gender
Hospital_data$AGE <- as.factor(Hospital_data$AGE)
model4 <- lm(TOTCHG~AGE+LOS+APRDRG,data = Hospital_data)
model4
summary(model4)


###  APRDRG has -ve t-value, dropping it ####

model5 <- lm(TOTCHG~AGE+LOS,data = Hospital_data)
model5
summary(model5)

#### Removing Race and gender doesn't change the R2 value. It doesn't impact cost
#### Removing APRDRG in model5 increases the standard error. Hence model model4 seems to be better.