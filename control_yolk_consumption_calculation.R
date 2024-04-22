library(stringr)
library(tidyverse)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(olsrr)
library(readxl)


#function that compiles all yolk data within a 96 well plate subfolder into one data frame
read_yolk_data = function(master){
  

  

    
    moms = list.files(master)
    HZ11 = read.csv(file.path(master, moms[1], "yolk_consumption_data.csv"))




    HZ12 = read.csv(file.path(master, moms[2], "yolk_consumption_data.csv"))



    HZ13 = read.csv(file.path(master, moms[3], "yolk_consumption_data.csv"))
    
    
    
    HZ9 = read.csv(file.path(master, moms[4], "yolk_consumption_data.csv"))
    
    
    
    LZ2 = read.csv(file.path(master, moms[5], "yolk_consumption_data.csv"))
    
    
    
    LZ3 = read.csv(file.path(master, moms[6], "yolk_consumption_data.csv"))
    
    
    
    LZ5 = read.csv(file.path(master, moms[7], "yolk_consumption_data.csv"))
    
    
    
    LZ7 = read.csv(file.path(master, moms[8], "yolk_consumption_data.csv"))
    
    data = rbind(HZ11, HZ12, HZ13, HZ9, LZ2, LZ3, LZ5, LZ7)
    return(data)
    
  
  
}

setwd("C:/Users/joemh/Desktop/all_tifs/HS_tifs")

#makes a list of each dated folder within a master folder
dated_folders = list.files(getwd())

#runs the function for each date and makes a separate data frame for each date
First_Data = read_yolk_data(file.path(getwd(),dated_folders[1]))
Second_Data = read_yolk_data(file.path(getwd(),dated_folders[2]))  
Third_Data = read_yolk_data(file.path(getwd(),dated_folders[3])) 
Fourth_Data = read_yolk_data(file.path(getwd(),dated_folders[4])) 
Fifth_Data = read_yolk_data(file.path(getwd(),dated_folders[5]))  

#binds data from each date into one data frame
Master_data = rbind(First_Data, Second_Data, Third_Data, Fourth_Data, Fifth_Data)
write.csv(Master_data, "Master_HS_yolk_data.csv")
write.csv(First_Data, "First_data.csv")
write.csv(Second_Data, "Second_data.csv")
write.csv(Third_Data, "Third_data.csv")
write.csv(Fourth_Data, "Fourth_data.csv")
write.csv(Fifth_Data, "Fifth_data.csv")




data = read.csv("Master_HS_yolk_data.csv")


#remove dead embryos from data
data = data |> filter(yolk_embryo_ratio < 1)
data = data |> filter(embryo_area != 7935464)


#arrange data by mom
data = data |> arrange(mom)

ggplot(data, aes(x=day_of_exp, y=embryo_area, color = mom)) + geom_point() + facet_wrap(~treatment)
ggplot(data, aes(x=day_of_exp, y=yolk_area, color = mom)) + geom_point() + facet_wrap(~treatment)

#make dataframes for each mom and filter out dead embryos that got photographed 
HZ11_data = data |> filter(mom %in% c("HZ11"))
HZ12_data = data |> filter(mom %in% c("HZ12"))
HZ13_data = data |> filter(mom %in% c("HZ13"))
HZ13_data = HZ13_data |> filter(X.1 != 225)
HZ9_data = data |> filter(mom %in% c("HZ9"))
LZ2_data = data |> filter(mom %in% c("LZ2"))
LZ2_data = LZ2_data |> filter(embryo != 2)
LZ3_data = data |> filter(mom %in% c("LZ3"))
LZ3_data = LZ3_data |> filter(X.1 != 255)
LZ5_data = data |> filter(mom %in% c("LZ5"))
LZ5_data = LZ5_data |> filter(X.1 != 468)
LZ7_data = data |> filter(mom %in% c("LZ7"))
LZ7_data = LZ7_data |> filter(embryo != 3)

#make plots of embryo size, yolk size
ggplot(HZ11_data, aes(x=day_of_exp, y=embryo_area, color = embryo)) + geom_point() + facet_grid(rows = 1) + geom_smooth(method ="lm") + xlab("Day of Experiment") + ylab("Embryo Area (In pixels)") + ggtitle("Embryo Growth During Experiment")
ggplot(HZ11_data, aes(x=day_of_exp, y=yolk_area, color = embryo)) + geom_point() + facet_grid(rows = 1) + geom_smooth(method = "lm") + xlab("Day of Experiment") + ylab("Yolk Area (In pixels)") + ggtitle("Yolk Area Reduction During Experiment")


#make plots of yolk to embryo ratio
ggplot(HZ13_data, aes(x=day_of_exp, y=yolk_embryo_ratio))+geom_point()+facet_grid(rows = vars(embryo)) + geom_smooth(method = "lm")


#current version of slope extraction
yolk_consumption_slopes = function(mom_data){
  slopes = dlply(mom_data, .(mom, embryo, treatment, elevation), function(d) summary(lm(yolk_embryo_ratio~day_of_exp, data = d)))
  coefficients = ldply(slopes, function(d) coef(d))
  x=length(coefficients)
  coefficients = filter(coefficients, Estimate < 0)
  coefficients = filter(coefficients, coefficients[,6] <= 0.1)
  coefficients$Estimate = coefficients$Estimate * -1
  return(coefficients)
}
HZ11_slopes = yolk_consumption_slopes(HZ11_data)
HZ12_slopes = yolk_consumption_slopes(HZ12_data)
HZ13_slopes = yolk_consumption_slopes(HZ13_data)
HZ9_slopes = yolk_consumption_slopes(HZ9_data)
LZ2_slopes = yolk_consumption_slopes(LZ2_data)
LZ3_slopes = yolk_consumption_slopes(LZ3_data)
LZ5_slopes = yolk_consumption_slopes(LZ5_data)
LZ7_slopes = yolk_consumption_slopes(LZ7_data)




high_elevation_yolk_consumption_rates = rbind(HZ11_slopes, HZ12_slopes, HZ13_slopes, HZ9_slopes)
low_elevation_yolk_consumption_rates = rbind(LZ2_slopes, LZ3_slopes, LZ5_slopes, LZ7_slopes)
all_yolk_consumption_rates = rbind(high_elevation_yolk_consumption_rates, low_elevation_yolk_consumption_rates)
ggplot(all_yolk_consumption_rates, aes(x=elevation, y = Estimate)) + geom_point() + facet_wrap(~treatment)

#write csv of yolk consumption rates, can be for control or HS
write.csv(all_yolk_consumption_rates, "control_yolk_consumption_rates.csv")

#make boxplot of yolk consumption rates
ggplot(all_yolk_consumption_rates, aes(x = treatment, y = yolk_consumption_rates, group = elevation, color = elevation)) + geom_boxplot() + ggtitle("Yolk Consumption Rates of Higher and Lower Elevation Groups Across Treatments") + ylab("%Yolk Consumed per Day")

setwd("C:/Users/joemh/Desktop/all_tifs/")
all_treatments_data = rbind(read.csv("control_tifs/control_yolk_consumption_rates.csv"),read.csv("HS_tifs/HS_yolk_consumption_rates.csv"))

write.csv(all_treatments_data,"C:/Users/joemh/Desktop/all_tifs/6_12Exp_yolk_consumption_rates.csv" )

all_treatments_plot <- ggplot(all_treatments_data, aes(x = elevation, y = yolk_consumption_rates, group = elevation, color = elevation)) + geom_boxplot() + facet_wrap(~treatment) + xlab("Elevation") + ylab("Yolk Consumption Rate (Percent yolk consumed per day") + ggtitle("Yolk Consumption Rate Across Elevations and Treatments")
ggplot(all_treatments_data, aes(x = elevation, y = yolk_consumption_rates, group = elevation, color = elevation)) + geom_boxplot() + facet_wrap(~treatment) + xlab("Elevation") + ylab("Yolk Consumption Rate (Percent yolk consumed per day)") + ggtitle("Yolk Consumption Rate Across Elevations and Treatments")
layer_data(all_treatments_plot)
