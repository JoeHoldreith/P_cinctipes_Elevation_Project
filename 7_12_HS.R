library(stringr)
library(tidyverse)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(olsrr)
library(readxl)



yolk_consumption = function(sub_directory, index){
  
  setwd(file.path(getwd(), sub_directory))
  
  
  
  #read in embryo area and remove unnecessary columns 
  embryo_area_raw_data = read.csv(file.path(getwd(), "embryo_area.csv/"))
  
  
  #read in yolk area and remove unnecessary columns
  yolk_area_raw_data = read.csv(file.path(getwd(), "yolk_area.csv/"))
  
  
  #create dataframe with embryo and yolk areas 
  data_final = yolk_area_raw_data
  data_final$embryo_area = embryo_area_raw_data[,2]
  colnames(data_final)[1] = "embryo"
  colnames(data_final)[2] = "yolk_area"
  
  #create column that divides yolk area by embryo area for each embryo
  data_final$yolk_embryo_ratio = data_final[,2]/data_final[,3]
  
  #creates a column for moms based on index
  if (index == 1){
    data_final$mom = "HZ11"
  } else if ( index == 2){
    data_final$mom = "HZ12"
  } else if ( index == 3){
    data_final$mom = "HZ13"
  } else if ( index == 4){
    data_final$mom = "HZ9"
  } else if ( index == 5){
    data_final$mom = "LZ2"
  } else if ( index == 6){
    data_final$mom = "LZ3"
  } else if ( index == 7){
    data_final$mom = "LZ5"
  } else if ( index == 8){
    data_final$mom = "LZ7"
  }
  
  #creates a column for elevation based on index
  if (index < 5){
    data_final$elevation = "higher"
  } else{
    data_final$elevation = "lower"
  }
  
  #creates a column for treatment
  data_final$treatment = "HS"
  
  #create a column with dates
  data_final$day_of_exp = 31
  data_final$julian_date = 193
  
  #write a csv of data_final
  write.csv(data_final, "yolk_consumption_data.csv")
  
}

setwd("C:/users/joemh/Desktop/HS_tifs/7_12_HS")

sub_directories = list.files(getwd())

print(sub_directories)

for (j in 1:length(sub_directories)){
  
  
  yolk_consumption(sub_directories[j], j)
  setwd("C:/users/joemh/Desktop/HS_tifs/7_12_HS")
  
}
