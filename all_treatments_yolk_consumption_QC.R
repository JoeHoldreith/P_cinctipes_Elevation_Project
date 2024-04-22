library(stringr)
library(tidyverse)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(olsrr)
library(readxl)


#read in winter data
winter_yolk_raw_data = read.csv("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/master_winter_yolk_raw_data.csv")


#add a column for summer or winter exp
winter_yolk_raw_data$Exp = "Winter"

#remove dead embryos from winter data using embryo area of dummy photo
winter_yolk_raw_data = filter(winter_yolk_raw_data, embryo_area != "4399939")
winter_yolk_raw_data = filter(winter_yolk_raw_data, yolk_embryo_ratio < 1)

#read in summer data
summer_control = read.csv("C:/Users/joemh/Desktop/all_tifs/control_tifs/Master_control_yolk_data.csv")
summer_heatshock = read.csv("C:/Users/joemh/Desktop/all_tifs/HS_tifs/Master_HS_yolk_data.csv")

#remove pictures of dead embryos that got photographed
summer_control = summer_control |> filter(X.1 != 225)
LZ2_data = summer_control |> filter(mom %in% c("LZ2"))
LZ2_data = LZ2_data |> filter(embryo != 2)
summer_control = filter(summer_control, mom != "LZ2")
summer_control = summer_control  |> filter(X.1 != 255)
summer_control = summer_control   |> filter(X.1 != 468)
LZ7_data = summer_control |> filter(mom %in% c("LZ7"))
LZ7_data = LZ7_data |> filter(embryo != 3)
summer_control = filter(summer_control, mom != "LZ7")
summer_control = rbind(summer_control, LZ2_data, LZ7_data)


HZ11_data = summer_heatshock |> filter(mom %in% c("HZ11"))
HZ11_data = HZ11_data |> filter(embryo != 7)
summer_heatshock = filter(summer_heatshock, mom != "HZ11")
summer_heatshock = summer_heatshock  |> filter(X.1 != 293)
summer_heatshock = summer_heatshock  |> filter(X.1 != 411)
LZ2_data = summer_heatshock |> filter(mom %in% c("LZ2"))
LZ2_data = LZ2_data |> filter(embryo != 10)
summer_heatshock = filter(summer_heatshock, mom != "LZ2")
LZ3_data = summer_heatshock |> filter(mom %in% c("LZ3"))
LZ3_data = LZ3_data |> filter(embryo != 7)
summer_heatshock = filter(summer_heatshock, mom != "LZ3")
summer_heatshock = summer_heatshock  |> filter(X.1 != 446)
summer_heatshock = summer_heatshock  |> filter(X.1 != 350)
summer_heatshock = summer_heatshock  |> filter(X.1 != 266)
summer_heatshock = rbind(summer_heatshock, HZ11_data, LZ2_data, LZ3_data)

summer_yolk_raw_data = rbind(summer_control,summer_heatshock)

#remove dead embryos from summer data using embryo area of dummy photo
summer_yolk_raw_data = filter(summer_yolk_raw_data, embryo_area != 7935464)
summer_yolk_raw_data = filter(summer_yolk_raw_data, yolk_embryo_ratio < 1)



write.csv(summer_yolk_raw_data, "C:/Users/joemh/Desktop/final data analysis/summer_data_dummies_removedONLY.csv")
write.csv(winter_yolk_raw_data, "C:/Users/joemh/Desktop/final data analysis/winter_data_dummies_removedONLY.csv")

#read in winter and summer data from dinal data analysis folder
summer_data = read.csv("C:/Users/joemh/Desktop/final data analysis/summer_data_dummies_removedONLY.csv")
winter_data = read.csv("C:/Users/joemh/Desktop/final data analysis/winter_data_dummies_removedONLY.csv")



all_treatments_yolk_data = rbind(summer_data, winter_data)

write.csv(all_treatments_yolk_data, "C:/Users/joemh/Desktop/final data analysis/all_treatments_data_dummies_removedONLY.csv")

#read in all treatments data
all_treatments_yolk_data = read.csv("C:/Users/joemh/Desktop/final data analysis/all_treatments_data_dummies_removedONLY.csv")

#quality control! plot all embryo area values from all moms and identify errors 
ggplot(all_treatments_yolk_data, aes(x = Day_of_Exp, y = embryo_area, color = embryo)) + geom_point() + facet_wrap(~mom)

HZ_winter_2 = filter(all_treatments_yolk_data, mom == "Higher zone WINTER 2")
HZ_winter_2 = filter(HZ_winter_2, Day_of_Exp == 20 )

#HZ_Winter_2 heat shock Embryo 9 had a mismeasurement, manual correction input below
all_treatments_yolk_data$embryo_area[909] = 3919511

HZ_winter_8 = filter(all_treatments_yolk_data, mom == "Higher zone WINTER 8")
HZ_winter_8 = filter(HZ_winter_8, Day_of_Exp == 28)
HZ_winter_8 = arrange(HZ_winter_8, embryo_area)

#HZ_winter_8 heat shock embryo 7 had a mismeasurement, manual correction input below
all_treatments_yolk_data$embryo_area[1486] = 3341910

HZ_SUMMER_11 = filter(all_treatments_yolk_data, mom == "HZ SUMMER11")
HZ_SUMMER_11 = filter(HZ_SUMMER_11, Day_of_Exp < 30)
HZ_SUMMER_11 = filter(HZ_SUMMER_11, Day_of_Exp > 9)
HZ_SUMMER_11 = arrange(HZ_SUMMER_11, embryo_area)

#HZ_SUMMER_11 HS embryo 1 is dead, correction below
all_treatments_yolk_data = filter(all_treatments_yolk_data, X != 613)





HZ_SUMMER_9 =  filter(all_treatments_yolk_data, mom == "HZ SUMMER9")
HZ_SUMMER_9 = filter(HZ_SUMMER_9,  Day_of_Exp  >  15)
HZ_SUMMER_9 =  arrange(HZ_SUMMER_9,  embryo_area)


#HZ_SUMMER_9 Control  embryo 11 is  dead at day 19, remove it
all_treatments_yolk_data = filter(all_treatments_yolk_data, X != 186)

#HZ_SUMMER_9 HS embryo 7 had a mismeasurement at day 31,  manual correction below
all_treatments_yolk_data$embryo_area[543] = 3052370

LZ_WINTER_1 =  filter(all_treatments_yolk_data, mom == "Lower zone WINTER 1")
LZ_WINTER_1 = filter(LZ_WINTER_1, Day_of_Exp > 25)

#LZ_WINTER_1 already hatched at day 28, remove it
all_treatments_yolk_data = filter(all_treatments_yolk_data,  X != 1562)

LZ_WINTER_3 = filter(all_treatments_yolk_data, mom == "Lower zone WINTER 3")
LZ_WINTER_3 = filter(LZ_WINTER_3, Day_of_Exp > 25)

LZ_WINTER_7 = filter(all_treatments_yolk_data, mom == "Lower zone WINTER 7")
LZ_WINTER_7 = filter(LZ_WINTER_7, Day_of_Exp > 10)
LZ_WINTER_7  = arrange(LZ_WINTER_7, embryo_area)

#LZ WINTER 7 control embryo 4 had a mismeasurement at day 13, manual correction below

all_treatments_yolk_data$embryo_area[1926] = 3672471

LZ_SUMMER_2  = filter(all_treatments_yolk_data, mom == "LZ SUMMER2")
LZ_SUMMER_2  = filter(LZ_SUMMER_2, Day_of_Exp > 10)
LZ_SUMMER_2 = arrange(LZ_SUMMER_2, embryo_area)

#LZ SUMMER 2 HS embryo 33 had a mismeasurement at day 19, manual correctionn below
all_treatments_yolk_data$embryo_area[674] = 2249049

LZ_SUMMER_3 = filter(all_treatments_yolk_data, mom == "LZ SUMMER3")
LZ_SUMMER_3 = filter(LZ_SUMMER_3, Day_of_Exp > 10)
LZ_SUMMER_3 = arrange(LZ_SUMMER_3, embryo_area)

#LZ SUMMER  3  HS embryo 1 had a mismeasuremennt at day 25 and 31, manual correction below
all_treatments_yolk_data$embryo_area[735] = 3275415
all_treatments_yolk_data$embryo_area[727] = 3647096


#quality control! plot all yolk area values from all moms and identify errors 
ggplot(all_treatments_yolk_data, aes(x = Day_of_Exp, y = yolk_area, color = embryo)) + geom_point() + facet_wrap(~mom)


HZ_WINTER_1 = filter(all_treatments_yolk_data, mom == "Higher zone WINTER 1")
HZ_WINTER_1 = filter(HZ_WINTER_1, Day_of_Exp  < 10)
HZ_WINTER_1 = arrange(HZ_WINTER_1, yolk_area)

HZ_WINTER_3 = filter(all_treatments_yolk_data, mom == "Higher zone WINTER 3")
HZ_WINTER_3 = filter(HZ_WINTER_3, Day_of_Exp < 10)
HZ_WINTER_3 = arrange(HZ_WINTER_3, yolk_area)

#HZ WINTER 3 HS embryo  2 had a mismeasurement at day 4, manual correction below
all_treatments_yolk_data$yolk_area[950] =  1653168

HZ_WINTER_5 = filter(all_treatments_yolk_data, mom ==  "Higher zone WINTER 5")
HZ_WINTER_5 = filter(HZ_WINTER_5, Day_of_Exp <15)
HZ_WINTER_5 = arrange(HZ_WINTER_5, yolk_area)



HZ_WINTER_6 = filter(all_treatments_yolk_data, mom == "Higher zone WINTER 6")
HZ_WINTER_6 = filter(HZ_WINTER_6, Day_of_Exp > 30)
HZ_WINTER_6 = arrange(HZ_WINTER_6,  yolk_area)

HZ_WINTER_8 = filter(all_treatments_yolk_data,  mom  == "Higher zone WINTER 8")
HZ_WINTER_8 = filter(HZ_WINTER_8, Day_of_Exp > 25)
HZ_WINTER_8 = arrange(HZ_WINTER_8, yolk_area)

LZ_WINTER_3 = filter(all_treatments_yolk_data, mom  == "Lower zone WINTER 3")
LZ_WINTER_3 = filter(LZ_WINTER_3, Day_of_Exp < 10)
LZ_WINTER_3 = arrange(LZ_WINTER_3, yolk_area)

#LZ WINTER 3 hs embryo 1 had a mismeasurement at day 4, manual correction below
all_treatments_yolk_data$yolk_area[1656]  = 1251966

LZ_WINTER_7 = filter(all_treatments_yolk_data, mom == "Lower zone WINTER 7")
LZ_WINTER_7 = arrange(LZ_WINTER_7, yolk_area)
 


LZ_SUMMER_2 = filter(all_treatments_yolk_data, mom ==  "LZ SUMMER2")
LZ_SUMMER_2 = filter(LZ_SUMMER_2, Day_of_Exp < 5)
LZ_SUMMER_2 = arrange(LZ_SUMMER_2, yolk_area)

LZ_SUMMER_5 = filter(all_treatments_yolk_data, mom == "LZ SUMMER5")
LZ_SUMMER_5 = filter(LZ_SUMMER_5, Day_of_Exp == 19)
LZ_SUMMER_5 = arrange(LZ_SUMMER_5,  yolk_area)


write.csv(all_treatments_yolk_data, "C:/Users/joemh/Desktop/final data analysis/all_treatments_data_QC_IN_PROGRESS.csv")

#RE NAME EMBRYOS THAT WERE IMPROPERLY INDEXED DURING TIFF IMAGE PROCESSING! 
all_treatments_yolk_data = arrange(all_treatments_yolk_data, mom)

#WINTER LZ3 control embryo  11 was misnamed as embryo 1 on day 28, rename it below
all_treatments_yolk_data$embryo[1294] =  11

#WINTER HZ2 HS embryos 7, 8, and 10 misnamed as embryos 1, 2, and 3 respectively on day 27, rename below
all_treatments_yolk_data$embryo[165] = 7
all_treatments_yolk_data$embryo[166] = 8
all_treatments_yolk_data$embryo[167] = 10

#WINTER HZ3 HS embryo 8 misnamed as embryo 2 on day 27, rename below
all_treatments_yolk_data$embryo[239] = 8

#WINTER LZ4 HS embryos 2, 3, and 4 misnamed as  1, 2, and  3 respectively on day 27. rename below
all_treatments_yolk_data$embryo[1401] = 2
all_treatments_yolk_data$embryo[1402] = 3
all_treatments_yolk_data$embryo[1403] = 4

#WINTER LZ8 Control embryo 5 misnamed as 1 on day 13, rename below
all_treatments_yolk_data$embryo[1634] = 5

#WINTER HZ5 control embryo 8 misnamed as 1 on day 28, rename below
all_treatments_yolk_data$embryo[386] = 8

#WINTER HZ7 control embryo 9 misnamed as 1 on day 28, rename below
all_treatments_yolk_data$embryo[619] = 9

#WINTER LZ6 control embryo 8 misnamed as  1 on day 28, rename below
all_treatments_yolk_data$embryo[1513] = 8

#WINTER HZ5 HS embryo A4 misnamed as 1 on day 28, rename below
all_treatments_yolk_data$embryo[423] = 4

#WINTER HZ7 HS embryos 5 and 7 misnamed 1 and 2 respectively, rename below
all_treatments_yolk_data$embryo[656] = 5
all_treatments_yolk_data$embryo[657] = 7

write.csv(all_treatments_yolk_data, "C:/Users/joemh/Desktop/final data analysis/all_treatments_data_QC_IN_PROGRESS.csv")



#QC DAY OF EXP AND JULIAN DATE

all_treatments_yolk_data = arrange(all_treatments_yolk_data, Exp)

#summer control plates  were photographed on day of exp 18 and day of exp 26, data frame erroneously lists them as day of exp 19 and day of exp 27 respectively, fix below
all_treatments_yolk_data$Day_of_Exp[25:36] = 18
all_treatments_yolk_data$Day_of_Exp[37:48] = 26

all_treatments_yolk_data$Day_of_Exp[121:131] = 18
all_treatments_yolk_data$Day_of_Exp[132:142] = 26

all_treatments_yolk_data$Day_of_Exp[215:224] = 18
all_treatments_yolk_data$Day_of_Exp[225:233] = 26

all_treatments_yolk_data$Day_of_Exp[301:309] = 18
all_treatments_yolk_data$Day_of_Exp[310:318] = 30
all_treatments_yolk_data$Day_of_Exp[319:327] = 26

write.csv(all_treatments_yolk_data, "C:/Users/joemh/Desktop/final data analysis/all_treatments_yolk_data_QC_COMPLETE.csv")


ggplot(all_treatments_yolk_data, aes(x = Day_of_Exp, y = embryo_area, color = embryo)) + geom_point() + facet_wrap(~mom)

ggplot(all_treatments_yolk_data, aes(x = Day_of_Exp, y = yolk_area, color = embryo)) + geom_point() + facet_wrap(~mom)

ggplot(all_treatments_yolk_data, aes(x = Day_of_Exp, y = yolk_embryo_ratio, color = embryo)) + geom_point() + facet_wrap(~mom)
