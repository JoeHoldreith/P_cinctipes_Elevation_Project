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

data_final = read.csv("C:/Users/joemh/Desktop/final data analysis/all_treatments_hatching_success.csv")

data_final$Mom = str_replace_all(data_final$Mom, fixed(" "), "")


#read in embryo area and yolk ratio values
average_sizes = read.csv("C:/Users/joemh/Desktop/final data analysis/avg_embryoarea_yolkratio_per_brood_treatment.csv")
average_sizes$ID = str_replace_all(average_sizes$ID, fixed(" "), "")

#add treatment_numeric column, r wont match control column for winter broods with "Control" string for some reason
average_sizes = arrange(average_sizes, Treatment)
average_sizes$Treatment_numeric = NA
average_sizes$Treatment_numeric[1:24] = 0
average_sizes$Treatment_numeric[25:48] = 1

#make data to test interaction between developmental stage and hatching success
test_data = left_join(data_final, average_sizes, join_by(Mom == ID, Treatment_numeric))

#anovas for developmental stage and embryo area and hatching success
c = aov(Decimal_success~YolkEmbryoRatio, data = test_data)

summary(c)

d = aov(Decimal_success~EmbryoAreamm, data = test_data)

summary(d)


b = aov(Decimal_success~YolkEmbryoRatio + Elevation_numeric + Coll_date_numeric, data = test_data)

summary(b)

#plot hatching success over developmental stage 
ggplot(test_data, aes(x = YolkEmbryoRatio, y = Decimal_success, color = Elevation)) + scale_color_manual(values=c("red", "blue")) + geom_point() + scale_x_reverse() + geom_smooth(method = "lm")+ scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + ylab("% Hatching Success") + xlab("Mean Yolk/Embryo Ratio at Week 1") + ggtitle("Developmental Stage and Hatching Success")+ theme(plot.title = element_text(face="bold"))

#plot developmental stage across broods regardless of treatment
#read in data
avg_brood_sizes = read.csv("C:/Users/joemh/Desktop/final data analysis/avg_embryoarea_yolkratio_per_brood.csv")

#add elevation column to avg_brood_size
avg_brood_sizes$Elevation = NA
avg_brood_sizes$Elevation[1:12] = "Higher"
avg_brood_sizes$Elevation[13:24] = "Lower"


ggplot(avg_brood_sizes, aes(x = Mom_numeric, y = YolkEmbryoRatio, fill = Elevation)) + geom_col() + scale_fill_manual(values=c("red", "blue"))+ xlab("Brood") + ylab("Mean Yolk/Embryo Area Ratio at Week 1") + ggtitle("Developmental Stages at Week 1") + theme(plot.title = element_text(face="bold"))

#anovas for hatching success and elevation
Y = aov(Decimal_success~Elevation_numeric + Coll_date_numeric  + Treatment_numeric + Mom_numeric, data = data_final)

summary(Y)


Z = aov(Decimal_success~Elevation_numeric * Coll_date_numeric  + Treatment_numeric, data = data_final)

summary(Z)

comparison = AIC(Y,Z)

comparison






ggplot(data_final, aes(x=Elevation,y=Decimal_success, Group = Elevation, color = Coll_date)) + geom_boxplot()+ scale_color_manual(values=c("red", "blue")) + geom_smooth(aes(group = Coll_date, color = Coll_date), method = "lm")  + scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + ylab("% Hatching Success") + labs(color = "Season") + ggtitle("Hatching Success Across\nElevations and Seasons")+ theme(plot.title = element_text(face="bold"))

plot = ggplot(data_final, aes(x=Elevation,y=Decimal_success, Group = Elevation, color = Coll_date)) + geom_boxplot()+ scale_color_manual(values=c("red", "blue")) + geom_smooth(aes(group = Coll_date, color = Coll_date), method = "lm")  + scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + ylab("% Hatching Success") + labs(color = "Season") + ggtitle("Hatching Success Across\nElevations and Seasons")+ theme(plot.title = element_text(face="bold"))



ggplot_build(plot)

