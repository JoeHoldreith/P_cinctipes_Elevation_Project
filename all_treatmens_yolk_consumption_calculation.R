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

#current version of slope extraction, taken from summer yolk consumption calculation script
yolk_consumption_slopes = function(mom_data){
  slopes = dlply(mom_data, .(mom, embryo, Treatment, Elevation), function(d) summary(lm(yolk_embryo_ratio~Day_of_Exp, data = d)))
  coefficients = ldply(slopes, function(d) coef(d))
  x=length(coefficients)
  coefficients = filter(coefficients, Estimate < 0)
  coefficients$Estimate = coefficients$Estimate * -1
  return(coefficients)
}

yolk_slopes = yolk_consumption_slopes(all_treatments_yolk_data)


#check for duplicate rows
test_data = yolk_slopes[1:510,1:3]
test_data2 = test_data %>% distinct()

ggplot(yolk_slopes,  aes( x = Treatment, y = Estimate, factor = Treatment)) + geom_point() + facet_wrap(~mom)

#add a column for collection date
yolk_slopes = arrange(yolk_slopes, mom)
yolk_slopes$coll_date = NA
yolk_slopes$coll_date[1:182] = "Winter"
yolk_slopes$coll_date[183:275] = "Summer"
yolk_slopes$coll_date[276:419] = "Winter"
yolk_slopes$coll_date[420:510] = "Summer"

#add a numeric version of collection date
yolk_slopes = arrange(yolk_slopes, coll_date)
yolk_slopes$coll_date_numeric = NA
yolk_slopes$coll_date_numeric[1:184] = 0
yolk_slopes$coll_date_numeric[185:510] = 1

#add a numeric version of treatment
yolk_slopes = arrange(yolk_slopes, Treatment)
yolk_slopes$Treatment_numeric = NA
yolk_slopes$Treatment_numeric[1:260] = 0
yolk_slopes$Treatment_numeric[261:510] = 1

#add numeric version of elevation
yolk_slopes = arrange(yolk_slopes, Elevation)
yolk_slopes$Elevation_numeric = NA
yolk_slopes$Elevation_numeric[1:275] = 1
yolk_slopes$Elevation_numeric[276:510] = 0

#add numeric version of mom
yolk_slopes = arrange(yolk_slopes, mom)
yolk_slopes$Mom_numeric = NA
yolk_slopes$Mom_numeric[1:20] = 5
yolk_slopes$Mom_numeric[21:40] = 6
yolk_slopes$Mom_numeric[41:64] = 7
yolk_slopes$Mom_numeric[65:87] = 8
yolk_slopes$Mom_numeric[88:111] = 9
yolk_slopes$Mom_numeric[112:135] = 10
yolk_slopes$Mom_numeric[136:159] = 11
yolk_slopes$Mom_numeric[160:182] = 12
yolk_slopes$Mom_numeric[183:205] = 2
yolk_slopes$Mom_numeric[206:229] = 3
yolk_slopes$Mom_numeric[230:252] = 4
yolk_slopes$Mom_numeric[253:275] = 1
yolk_slopes$Mom_numeric[276:298] = 17
yolk_slopes$Mom_numeric[299:321] = 19
yolk_slopes$Mom_numeric[322:345] = 20
yolk_slopes$Mom_numeric[346:369] = 21
yolk_slopes$Mom_numeric[370:393] = 22
yolk_slopes$Mom_numeric[394:417] = 23
yolk_slopes$Mom_numeric[418:419] = 24
yolk_slopes$Mom_numeric[420:441] = 13
yolk_slopes$Mom_numeric[442:464] = 14
yolk_slopes$Mom_numeric[465:488] = 15
yolk_slopes$Mom_numeric[489:510] = 16






write.csv(yolk_slopes, "C:/Users/joemh/Desktop/final data analysis/all_treatments_yolk_consumption_rates.csv")


#current version of slope extraction, taken from summer yolk consumption calculation script
growth_slopes = function(mom_data){
  slopes = dlply(mom_data, .(mom, embryo, Treatment, Elevation), function(d) summary(lm(embryo_area~Day_of_Exp, data = d)))
  coefficients = ldply(slopes, function(d) coef(d))
  x=length(coefficients)
  coefficients = filter(coefficients,  Estimate < 100000)
  return(coefficients)
}



growth_rates = growth_slopes(all_treatments_yolk_data)

#check for duplicate rows
test_data = growth_rates[1:511,1:3]
test_data2 = test_data %>% distinct()

#add a column for collection date
growth_rates = arrange(growth_rates, mom)
growth_rates$coll_date = NA
growth_rates$coll_date[1:183] = "Winter"
growth_rates$coll_date[184:276] = "Summer"
growth_rates$coll_date[277:420] = "Winter"
growth_rates$coll_date[421:511] = "Summer"

#add a numeric version of collection date
growth_rates = arrange(growth_rates, coll_date)
growth_rates$coll_date_numeric = NA
growth_rates$coll_date_numeric[1:184] = 0
growth_rates$coll_date_numeric[185:511] = 1

#add a numeric version of treatment
growth_rates = arrange(growth_rates, Treatment)
growth_rates$Treatment_numeric = NA
growth_rates$Treatment_numeric[1:260] = 0
growth_rates$Treatment_numeric[261:511] = 1

#add numeric version of elevation
growth_rates = arrange(growth_rates, Elevation)
growth_rates$Elevation_numeric = NA
growth_rates$Elevation_numeric[1:276] = 1
growth_rates$Elevation_numeric[277:511] = 0

#add numeric version of mom
growth_rates = arrange(growth_rates, mom)
growth_rates$Mom_numeric = NA
growth_rates$Mom_numeric[1:21] = 5
growth_rates$Mom_numeric[22:41] = 6
growth_rates$Mom_numeric[42:65] = 7
growth_rates$Mom_numeric[66:88] = 8
growth_rates$Mom_numeric[89:112] = 9
growth_rates$Mom_numeric[113:136] = 10
growth_rates$Mom_numeric[137:160] = 11
growth_rates$Mom_numeric[161:183] = 12
growth_rates$Mom_numeric[184:206] = 2
growth_rates$Mom_numeric[207:230] = 3
growth_rates$Mom_numeric[231:253] = 4
growth_rates$Mom_numeric[254:276] = 1
growth_rates$Mom_numeric[277:299] = 17
growth_rates$Mom_numeric[300:322] = 19
growth_rates$Mom_numeric[323:346] = 20
growth_rates$Mom_numeric[347:370] = 21
growth_rates$Mom_numeric[371:394] = 22
growth_rates$Mom_numeric[395:418] = 23
growth_rates$Mom_numeric[419:420] = 24
growth_rates$Mom_numeric[421:442] = 13
growth_rates$Mom_numeric[443:465] = 14
growth_rates$Mom_numeric[466:489] = 15
growth_rates$Mom_numeric[490:511] = 16


ggplot(growth_rates, aes( x = Treatment, y = Estimate, factor = Treatment)) + geom_point() + facet_wrap(~mom)

#growth rate calculation using mm squared instead of pixels as unit
growth_slopes_mm = function(mom_data){
  slopes = dlply(mom_data, .(mom, embryo, Treatment, Elevation), function(d) summary(lm(embryo_area_mm~Day_of_Exp, data = d)))
  coefficients = ldply(slopes, function(d) coef(d))
  x=length(coefficients)
  coefficients = filter(coefficients,  Estimate < 0.1)
  return(coefficients)
}

growth_rates_mm = growth_slopes_mm(all_treatments_yolk_data)

#check for duplicate rows
test_data = growth_rates_mm[1:511,1:3]
test_data2 = test_data %>% distinct()

#add a column for collection date
growth_rates_mm = arrange(growth_rates_mm, mom)
growth_rates_mm$coll_date = NA
growth_rates_mm$coll_date[1:183] = "Winter"
growth_rates_mm$coll_date[184:276] = "Summer"
growth_rates_mm$coll_date[277:420] = "Winter"
growth_rates_mm$coll_date[421:511] = "Summer"

#add a numeric version of collection date
growth_rates_mm = arrange(growth_rates_mm, coll_date)
growth_rates_mm$coll_date_numeric = NA
growth_rates_mm$coll_date_numeric[1:184] = 0
growth_rates_mm$coll_date_numeric[185:511] = 1

#add a numeric version of treatment
growth_rates_mm = arrange(growth_rates_mm, Treatment)
growth_rates_mm$Treatment_numeric = NA
growth_rates_mm$Treatment_numeric[1:260] = 0
growth_rates_mm$Treatment_numeric[261:511] = 1

#add numeric version of elevation
growth_rates_mm = arrange(growth_rates_mm, Elevation)
growth_rates_mm$Elevation_numeric = NA
growth_rates_mm$Elevation_numeric[1:276] = 1
growth_rates_mm$Elevation_numeric[277:511] = 0

#add numeric version of mom
growth_rates_mm = arrange(growth_rates_mm, mom)
growth_rates_mm$Mom_numeric = NA
growth_rates_mm$Mom_numeric[1:21] = 5
growth_rates_mm$Mom_numeric[22:41] = 6
growth_rates_mm$Mom_numeric[42:65] = 7
growth_rates_mm$Mom_numeric[66:88] = 8
growth_rates_mm$Mom_numeric[89:112] = 9
growth_rates_mm$Mom_numeric[113:136] = 10
growth_rates_mm$Mom_numeric[137:160] = 11
growth_rates_mm$Mom_numeric[161:183] = 12
growth_rates_mm$Mom_numeric[184:206] = 2
growth_rates_mm$Mom_numeric[207:230] = 3
growth_rates_mm$Mom_numeric[231:253] = 4
growth_rates_mm$Mom_numeric[254:276] = 1
growth_rates_mm$Mom_numeric[277:299] = 17
growth_rates_mm$Mom_numeric[300:322] = 19
growth_rates_mm$Mom_numeric[323:346] = 20
growth_rates_mm$Mom_numeric[347:370] = 21
growth_rates_mm$Mom_numeric[371:394] = 22
growth_rates_mm$Mom_numeric[395:418] = 23
growth_rates_mm$Mom_numeric[419:420] = 24
growth_rates_mm$Mom_numeric[421:442] = 13
growth_rates_mm$Mom_numeric[443:465] = 14
growth_rates_mm$Mom_numeric[466:489] = 15
growth_rates_mm$Mom_numeric[490:511] = 16


write.csv(growth_rates_mm, "C:/Users/joemh/Desktop/final data analysis/all_treatments_growth_rates_mm.csv")









