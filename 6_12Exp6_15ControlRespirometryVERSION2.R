library(stringr)
library(tidyverse)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(olsrr)
library(readxl)
#Change directory
setwd("C:/Users/joemh/Desktop/Tecan CSV files/")
data=read.csv("6_12CONTROL6_15RESPIROMETRY.csv")
data=data[1:61,]
## clean up data to remove units from time and Temperature and to make all data types numeric.
data$Time = as.numeric(substr(data$Time,1,nchar(data$Time)-1))#Can only run once, otherwise removes too many chars
#Convert seconds to hours
data$Time = data$Time/3600

##temp as numeric NEEDS TO BE SAME NAME AS COLUMN OR IT WILL MAKE A NEW ONE!!!
data$Temp = as.numeric(substr(data$Temp,1,nchar(data$Temp)-3))

##subset data to only above a certain time value, use subset command, make new data frame that subsets original data frame but time is greater than certain value, 5000 in this case 
#data <- subset(data, Time >= 5000,
              # select=c(Time,Temp,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,G1,G2,G3,G4,G5,G6,G7,G8,G9,G10,G11,G12,H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12))

#Converts characters to integers in overflow wells

##wide to tall
data_tall = melt(data,id.vars=c("Time","Temp"), variable.name="Well")


##take data tall, give me all the rows of data tall where the column called well has a value in blank wells

blanks= c("A1","B1","C1","D1")

data_tall_blanks = data_tall[data_tall$Well %in% blanks,]

##zeros 0% ox

zeros = c("E1","F1","G1","H1")

data_tall_zeros = data_tall[data_tall$Well %in% zeros,]

samples = c("A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","B2", "B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12")

data_tall_samples = data_tall[data_tall$Well %in% samples,]

##for each of those three data frames, add a column that describes the data, goal of adding that word, and then put it all together. using cbind or rbind (look it up :)) one final data thing stiched together with everything, make a plot specify the color based on the column (name) 

##new column, first have to add the value of the column, in this case, zero

zero <- c('zero')

data_tall_zeros$type = zero

blank = c('blank')

data_tall_blanks$type = blank

sample = c('sample')

data_tall_samples$type = sample


##now use either r bind or c bind

data_final = rbind(data_tall_blanks,data_tall_zeros,data_tall_samples)

data_final$well_type= paste(data_final$Well,data_final$type)








#Make example fluoresence plots for control wells
ggplot(data_tall_blanks, aes(x=Time, y=value))+geom_point()+facet_wrap(~Well)+geom_smooth(method="lm") + xlab("Hours") + ylab("Fluoresence") + ggtitle("Oxygenated Controls")
ggplot(data_tall_zeros, aes(x=Time, y=value))+geom_point()+facet_wrap(~Well)+geom_smooth(method="lm") + xlab("Hours") + ylab("Fluoresence") +ggtitle("Deoxygenated Controls")

ggplot(data_final, aes(x=Time, y=value))+geom_point()+facet_wrap(~Well, scales = "free", nrow = 8)+geom_smooth(method="lm") + xlab("Hours") + ylab("Fluoresence")


#ggplot(data_final, aes(x=Time, y=value, color=well_type)) + geom_point() +geom_smooth(method="lm") + xlab("Hours") + ylab("Fluoresence")

# extract slope data and R-squared for each well
#slopes = dlply(data_final, .(Well), function(d) summary(lm(value~Time, data=d)))
#coefficients = ldply(slopes, function(d) coef(d))
#slopes2=coefficients[c(seq(2,192,2)),]
#slopes2
#R2 = ldply(slopes, function(d) d$r.squared)

#Respiration_Rates = merge(slopes2,R2)

##average slope of 0s, average slope of 100s. subtract the background respiration from each embryo. but you need to add back the changing 0s. 

#averagezeros= mean(c(Respiration_Rates[49,2],Respiration_Rates[61,2],Respiration_Rates[73,2],Respiration_Rates[85,2]))
#averagezeros

#averageblanks= mean(c(Respiration_Rates[1,2],Respiration_Rates[13,2],Respiration_Rates[25,2],Respiration_Rates[37,2]))
#averageblanks

##new column in the data frame that corrects the rates for the zeroes and the blanks

#Respiration_Rates$Corrected = Respiration_Rates[,2]-averageblanks-averagezeros

#ggplot(Respiration_Rates, aes(x = Well, y = Corrected)) + geom_point() 

# ##change in fluorescence, need to convert these to oxygen values rather than fluorescence values 
# ## first calculate starting span and ending span (range between 0% O2 and 100% O2)
# startingzero = subset(data_tall_zeros, Time<min(Time+1))
# startingzeroFluoresence = mean(startingzero$value)
# 
# startingblank = subset(data_tall_blanks, Time<min(Time+1))
# startingblankFluoresence = mean(startingblank$value)
# 
# endingzero = subset(data_tall_zeros, Time>max(Time-1))
# endingzeroFluoresence = mean(endingzero$value)
# 
# endingblank = subset(data_tall_blanks, Time>max(Time-1))
# endingblankFluoresence = mean(endingblank$value)
# 
# starting_span = startingzeroFluoresence-startingblankFluoresence
# starting_span # 24755: the difference between 100% O2 and 0% O2 at start
# 
# ending_span = endingzeroFluoresence-endingblankFluoresence
# ending_span # 21494: the difference between 100% O2 and 0% O2 at end
# 
# pct_change = 1-ending_span/starting_span
# pct_change
# 
# # this results in a quantitative measure of the fluoresence span at the start and end of the "linear portion" of the data. In this case was 13%.  That means that the slopes should be corrected by that amount.  Since the span got smaller, the slopes are artifically higher by 13%


## Instead of correcting the slopes, I thought it would be better to convert everything into %O2 from start (span 0-100 O2) and then do a normalization to scale the data at each timepoint between the means of the 0 and 100% O2 values at that timepoint.

## first calculate row means for zeros and blanks
data_zeros = data[,c(51,63,75,87)]
data_zeros$mean = rowMeans(data_zeros[,1:4])

data_blanks = data[,c(3,15,27,39)]
data_blanks$mean = rowMeans(data_blanks[,1:4])

# next use Min-Max normalization (X – min(X)) / (max(X) – min(X)) (from: https://www.statology.org/how-to-normalize-data-in-r/#:~:text=By%20normalizing%20the%20variables%2C%20we,(X%20–%20μ)%20%2F%20σ)

data_overall_corrected = 100*(data[,3:98]-data_zeros$mean)/(data_blanks$mean-data_zeros$mean) ## normalization based on the mean value for zeros and mean value for blanks at each timepoint, multiplied by 100 so spans 0 to 100. (Min-Max function scales to 0-1) and inverted so Zeros are 0 and blanks are 100).

# this makes a data frame where the blanks (100% O2 are scaled to 100%) and the zeros are closer to 0%  This is done separateley for each timepoint.

# put the first few columns back in
data_corr = cbind(data[,1:2],data_overall_corrected)
data_corr_tall = melt(data_corr,id.vars=c("Time","Temp"), variable.name="Well")

#trim time period where fluorescence was abnormal
data_corr_tall = filter(data_corr_tall, Time > 1)




#ggplot(data_corr_tall, aes(x=Time, y=value))+geom_point()+facet_wrap(~Well,nrow=8)+geom_smooth(method="lm") + xlab("Hours") + ylab("%O2 Saturation")
ggplot(data_corr_tall, aes(x=Time, y=value))+geom_point()+facet_wrap(~Well,scales="free",nrow=8)+geom_smooth(method="lm") + xlab("Hours") + ylab("%O2 Saturation")
slopes = dlply(data_corr_tall, .(Well), function(d) summary(lm(value~Time, data=d)))
coefficients = ldply(slopes, function(d) coef(d))
slopes2=coefficients[c(seq(2,192,2)),]  # note the 2,192,2 is only needed for a full 96-well plate. That's why the output dataframe has NAs
slopes2
R2 = ldply(slopes, function(d) d$r.squared)

Summarized_data = merge(slopes2,R2)

#trim time specific anomalies in sample wells


Summarized_data$respiration_rate_percent_02perhour = Summarized_data$Estimate*-1


#trim all wells with negative respiration rate
Summarized_data = filter(Summarized_data, respiration_rate_percent_02perhour > 0)

#Trim control wells
Summarized_data = filter(Summarized_data, Well != "A1")
Summarized_data = filter(Summarized_data, Well != "B1")
Summarized_data = filter(Summarized_data, Well != "C1")
Summarized_data = filter(Summarized_data, Well != "D1")
Summarized_data = filter(Summarized_data, Well != "E1")
Summarized_data = filter(Summarized_data, Well != "F1")
Summarized_data = filter(Summarized_data, Well != "G1")
Summarized_data = filter(Summarized_data, Well != "H1")

#Remove wells with abnormal fluorescence curves
Summarized_data = filter(Summarized_data, Well != "A4")
Summarized_data = filter(Summarized_data, Well != "A7")
Summarized_data = filter(Summarized_data, Well != "E10")
Summarized_data = filter(Summarized_data, Well != "G10")
#Trim wells with dead embryos

#populate summarized data with new columns 
Summarized_data$Mom = "NA"
Summarized_data$Mom_numeric = "NA"
Summarized_data$Elevation = "NA"
Summarized_data$Mom[1:8] = "Higher Zone Summer 9"
Summarized_data$Mom[9:18] = "Higher Zone Summer 11"
Summarized_data$Mom[19:27] = "Higher Zone Summer 12"
Summarized_data$Mom[28:38] = "Higher Zone Summer 13"
Summarized_data$Mom[39:47] = "Lower Zone Summer 2"
Summarized_data$Mom[48:57] = "Lower Zone Summer 3"
Summarized_data$Mom[58:66] = "Lower Zone Summer 5"
Summarized_data$Mom[67:74] = "Lower Zone Summer 7"
Summarized_data$Mom_numeric[1:8] = 1
Summarized_data$Mom_numeric[9:18] = 2
Summarized_data$Mom_numeric[19:27] = 3
Summarized_data$Mom_numeric[28:38] = 4
Summarized_data$Mom_numeric[39:47] = 13
Summarized_data$Mom_numeric[48:57] = 14
Summarized_data$Mom_numeric[58:66] = 15
Summarized_data$Mom_numeric[67:74] = 16
Summarized_data$Elevation[1:38] = "Higher"
Summarized_data$Elevation[39:74] = "Lower"
Summarized_data$Treatment[1:74] = "Control"
Summarized_data$Day_of_Exp[1:74] = "4"
Summarized_data$Julien_Date[1:74] = "166"
Summarized_data$Coll_date[1:74] = "Summer"

write.csv(Summarized_data,"C:/Users/joemh/Desktop/Tecan CSV files/QCd Resp Rates/6_12CONTROL6_15respiration_rates_QC_COMPLETE.CSV")
## OK - I think this works - please check it!

#Plot of temperature over time for each well
#ggplot(data_corr_tall, aes(x=Time, y=Temp))+geom_point()+facet_wrap(~Well,nrow=8)+geom_smooth(method="lm") + xlab("Hours") + ylab("°C")

#Plot of temp over time for one well
#ggplot(data_corr_tall[data_corr_tall$Well=="A1",], aes(x=Time, y=Temp))+geom_point()+geom_smooth(method="lm")+ xlab("Hours") + ylab("°C")

#Make a plot of all summarized data
#ggplot(Summarized_data, aes(x=Day_of_Exp,y=respiration_rate_percent_02perhour,group=Mom,color=Treatment,fill=Elevation))+geom_boxplot()+facet_wrap(~Mom,scale="free") + xlab("Day of Experiment") + ylab("Respiration Rate %O2 per Hour")
#Removed fill. Use above line for fill. 
ggplot(Summarized_data, aes(x=Day_of_Exp,y=respiration_rate_percent_02perhour,group=Mom,color=Elevation))+geom_boxplot()+facet_wrap(~Mom,scale="free") + xlab("Day of Experiment") + ylab("Respiration Rate %O2 per Hour")


