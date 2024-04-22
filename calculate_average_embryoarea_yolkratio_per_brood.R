library(stringr)
library(tidyverse)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(olsrr)
library(readxl)

all_treatments_yolk_data = read.csv("C:/Users/joemh/Desktop/final data analysis/all_treatments_yolk_data_QC_complete.csv")

#convert pixels to mm, 1mm = 2275.0220 pixels. square 2275.0220 to convert area in pixels to area in mm
all_treatments_yolk_data$embryo_area_mm = all_treatments_yolk_data$embryo_area/(2275.0220 * 2275.0220)

#identify earliest day photos were taken for each mom
ggplot(all_treatments_yolk_data, aes(x = Day_of_Exp, y = embryo_area_mm)) + geom_point() + facet_wrap(~mom)

#choose only earliest photos for each mom
early_data = filter(all_treatments_yolk_data, Day_of_Exp <= 7)

#check to make sure only first photos are included for each mom
ggplot(early_data, aes(x = Day_of_Exp, y = embryo_area_mm)) + geom_point() + facet_wrap(~mom)

write.csv(early_data, "C:/Users/joemh/Desktop/final data analysis/early_embryo_yolkratio_values.csv")

## Calculate mean embryo are for each mom, for each treatment at start of exp
size_means<-ddply(early_data,.(mom, Treatment), function(d) mean(d$embryo_area_mm))
head(size_means)
names(size_means)=c("ID","Treatment", "EmbryoAreamm")

#calculate mean embryo/yolk ratio for each mom at start of exp
ratio_means<-ddply(early_data,.(mom, Treatment), function(d) mean(d$yolk_embryo_ratio))
head(ratio_means)
names(ratio_means)=c("ID", "Treatment", "YolkEmbryoRatio")

AvgData = merge(size_means, ratio_means)

write.csv(AvgData, "C:/Users/joemh/Desktop/final data analysis/avg_embryoarea_yolkratio_per_brood_treatment.csv")

## Calculate mean embryo are for each mom at start of exp
size_means2<-ddply(early_data,.(mom), function(d) mean(d$embryo_area_mm))
head(size_means2)
names(size_means2)=c("ID", "EmbryoAreamm")

#calculate mean embryo/yolk ratio for each mom at start of exp
ratio_means2<-ddply(early_data,.(mom), function(d) mean(d$yolk_embryo_ratio))
head(ratio_means2)
names(ratio_means2)=c("ID", "YolkEmbryoRatio")

AvgData2 = merge(size_means2, ratio_means2)

write.csv(AvgData2, "C:/Users/joemh/Desktop/final data analysis/avg_embryoarea_yolkratio_per_brood.csv")

