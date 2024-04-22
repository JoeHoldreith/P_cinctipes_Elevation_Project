library(stringr)
library(tidyverse)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(olsrr)
library(readxl)
library(geomtextpath)

data_final = read.csv("C:/Users/joemh/Desktop/final data analysis/all_treatments_yolk_consumption_rates.csv")

#read in embryo area and yolk ratio values
early_data = read.csv("C:/Users/joemh/Desktop/final data analysis/early_embryo_yolkratio_values.csv")


test_data = left_join(data_final, early_data, join_by(mom, embryo, Treatment), relationship = "many-to-many")

#anova test for developomental stage and yolk consumption rate
p = aov(Estimate~yolk_embryo_ratio, data = test_data)

summary(p)

f = aov(Estimate~embryo_area_mm, data = test_data)

summary(f)



#make plot of yolk consumption rates on y axis and developmental stage on x axis
ggplot(test_data, aes(x = yolk_embryo_ratio, y = Estimate, color = Elevation.x)) + geom_point() + scale_x_reverse()+geom_smooth(method = "lm") + labs(color = "Elevation") + scale_color_manual(values=c("red", "blue"))+xlab("Yolk/Embryo Ratio at Week 1") +  ylab("Yolk Consumption Rate (%yolk/day)") + ggtitle("Developmental Stage and\nYolk Consumption Rates")+ theme(plot.title = element_text(face="bold"))


#ANOVAs for elevation and yolk consumption rate
X = aov(Estimate~Elevation_numeric * coll_date_numeric + Treatment_numeric + Mom_numeric/embryo, data = data_final)

summary(X)

Y = aov(Estimate~Elevation_numeric + coll_date_numeric  + Treatment_numeric + Mom_numeric/embryo, data = data_final)

summary(Y)

comparison = AIC(X,Y)
comparison

Z = aov(Estimate~Elevation_numeric * coll_date_numeric  + Treatment_numeric, data = data_final)

summary(Z)

comparison = AIC(X,Y,Z)

comparison


## Calculate mean and SD for data for each mom
means<-ddply(data_final,.(mom,Treatment), function(d) mean(d$Estimate))
head(means)
names(means)=c("ID","Treatment","YolkConsumptionRate")

SD<-ddply(data_final,.(mom,Treatment), function(d) sd(d$Estimate))
head(SD)
names(SD)=c("ID","Treatment","YolkConsumptionRate_SD")

AvgData=merge(means,SD)
head(AvgData)

## now let's make a plot of the data using means+ error bars
AvgData$Season=c(rep("Winter",16),rep("Summer",8),rep("Winter",14),rep("Summer",8))
AvgData$Elevation=c(rep("Higher",24),rep("Lower",22))


q = ggplot(data=AvgData, aes(x=Elevation,y=YolkConsumptionRate, color = Season)) + scale_color_manual(values=c("darkorange", "blue"))
q+geom_boxplot(aes(fill = Treatment)) + scale_fill_manual(values=c("lightblue", "red")) + ylab("Mean Yolk Consumption Rate (%yolk/day)") + ggtitle("Mean Brood Yolk Consumption Rates\nAcross Elevations and Seasons") + theme(plot.title = element_text(face="bold"))

