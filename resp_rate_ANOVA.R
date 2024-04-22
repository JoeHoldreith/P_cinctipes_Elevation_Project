library(stringr)
library(tidyverse)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(olsrr)
library(readxl)

data_final = read.csv("C:/Users/joemh/Desktop/final data analysis/QCd_resp_rates.csv")

#read in embryo area and yolk ratio values
early_data = read.csv("C:/Users/joemh/Desktop/final data analysis/early_embryo_yolkratio_values.csv")


test_data = left_join(data_final, early_data, join_by(Mom, Well, Treatment), relationship = "many-to-many")


X = aov(standardized_resp_rate~Elevation_numeric * Coll_date_numeric + Treatment + Mom_numeric/Well, data = data_final)

summary(X)

Y = aov(standardized_resp_rate~Elevation_numeric + Coll_date_numeric  + Treatment + Mom_numeric/Well, data = data_final)

summary(Y)


Z = aov(standardized_resp_rate~Elevation_numeric * Coll_date_numeric  + Treatment, data = data_final)

summary(Z)

comparison = AIC(X,Y,Z)

comparison


p = aov(respiration_rate_percent_02perhour~yolk_embryo_ratio, data = test_data)

summary(p)

u = aov(respiration_rate_percent_02perhour~embryo_area_mm, data = test_data)

summary(u)





AA = aov(respiration_rate_percent_02perhour~Elevation_numeric * Coll_date_numeric + Treatment + Mom_numeric/Well, data = data_final)

summary(AA)

BB = aov(respiration_rate_percent_02perhour~Elevation_numeric * Coll_date_numeric + Treatment * Mom_numeric/Well, data = data_final)
summary(BB)

CC = aov(respiration_rate_percent_02perhour~Elevation_numeric * Coll_date_numeric + Treatment * Mom_numeric, data = data_final)
summary(CC)

comparion_2 = AIC(AA, BB, CC)



## Calculate mean and SD for data for each mom
means<-ddply(data_final,.(Mom), function(d) mean(d$standardized_resp_rate))
head(means)
names(means)=c("ID","RespirationRate")

SD<-ddply(data_final,.(Mom), function(d) sd(d$standardized_resp_rate))
head(SD)
names(SD)=c("ID","RespirationRate_SD")

AvgData=merge(means,SD)
head(AvgData)

## now let's make a plot of the data using means+ error bars
AvgData$Season=c(rep("Summer",4),rep("Winter",8),rep("Summer",4),rep("Winter",8))
AvgData$Elevation=c(rep("Higher",12),rep("Lower",12))

q = ggplot(data=AvgData, aes(x=Elevation,y=RespirationRate,color=Season)) + scale_color_manual(values=c("red", "blue"))
q+geom_boxplot() + geom_smooth(aes(group = Season), method = "lm") + ylab("Mean Normalized Respiration Rate (%02/hour)") + ggtitle("Mean Brood Normalized Respiration Rates\nAcross Elevations and Seasons") + theme(plot.title = element_text(face="bold"))

qq = q+geom_boxplot() + geom_smooth(aes(group = Season), method = "lm") + ylab("Normalized Respiration Rate (%02/hour)") + ggtitle("Normalized Respiration Rates Across\nElevations and Seasons") + theme(plot.title = element_text(face="bold"))

ggplot_build(qq)

