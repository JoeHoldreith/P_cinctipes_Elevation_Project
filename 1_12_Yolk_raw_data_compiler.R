library(stringr)
library(tidyverse)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(olsrr)
library(readxl)


#function that calculates yolk/embryo ratio, adds a column for mom and a columns for Julian date and day of exp from raw yolk and embryo data for Control 1 plates.
yolk_consumption_control1_plates = function(sub_directory, index, date){
  
  
  
  
  
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
    data_final$mom = "Higher Zone 1"
  } else if ( index == 2){
    data_final$mom = "Higher Zone 2"
  } else if ( index == 3){
    data_final$mom = "Higher Zone 3"
  } else if ( index == 4){
    data_final$mom = "Higher Zone 4"
  } else if ( index == 5){
    data_final$mom = "Lower Zone 1"
  } else if ( index == 6){
    data_final$mom = "Lower Zone 2"
  } else if ( index == 7){
    data_final$mom = "Lower Zone 3"
  } else if ( index == 8){
    data_final$mom = "Lower Zone 4"
  }
  
  #create column for treatment
  data_final$Treatment = "Control"
  
  
  #create columns for date
  if (date == 1){
    data_final$Day_of_Exp = "5"
    data_final$Julian_date = "16"
  } else if (date == 2){
    data_final$Day_of_Exp = "12"
    data_final$Julian_date = "23"
  }else if (date == 3){
    data_final$Day_of_Exp = "21"
    data_final$Julian_date = "32"
  }else if (date == 4){
    data_final$Day_of_Exp = "28"
    data_final$Julian_date = "39"
  }
  

  #write a csv of data_final
  write.csv(data_final, "yolk_consumption_data.csv")
  
}

setwd("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/1 plates/control/")

dated_folders = list.files(getwd())

for (i in 1:length(dated_folders)){
  
  
  sub_directories = list.files(file.path("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/1 plates/control/", dated_folders[i]))
  
  
  
  for (j in 1:length(sub_directories)){
    
    setwd(file.path("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/1 plates/control/", dated_folders[i]))
    setwd(file.path("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/1 plates/control/", dated_folders[i], sub_directories[j]))
    yolk_consumption_control1_plates(sub_directories[j], j, i)
    
    
  }
  
}


yolk_consumption_control2_plates = function(sub_directory, index, date){
  
  
  
  
  
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
    data_final$mom = "Higher Zone 5"
  } else if ( index == 2){
    data_final$mom = "Higher Zone 6"
  } else if ( index == 3){
    data_final$mom = "Higher Zone 7"
  } else if ( index == 4){
    data_final$mom = "Higher Zone 8"
  } else if ( index == 5){
    data_final$mom = "Lower Zone 5"
  } else if ( index == 6){
    data_final$mom = "Lower Zone 6"
  } else if ( index == 7){
    data_final$mom = "Lower Zone 7"
  } else if ( index == 8){
    data_final$mom = "Lower Zone 8"
  }
  
  #creates a column for treatment
  data_final$Treatment = "Control"
  
  #create columns for date, change these values depending on which folders you're processing! 
  #re-run this function if you change the dates!  
  if (date == 1){
    data_final$Day_of_Exp = "6"
    data_final$Julian_date = "17"
  } else if (date == 2){
    data_final$Day_of_Exp = "13"
    data_final$Julian_date = "24"
  }else if (date == 3){
    data_final$Day_of_Exp = "21"
    data_final$Julian_date = "32"
  }else if (date == 4){
    data_final$Day_of_Exp = "28"
    data_final$Julian_date = "39"
  }
  
  #write a csv of data_final
  write.csv(data_final, "yolk_consumption_data.csv")
  
}


setwd("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/2 plates/control/")

dated_folders = list.files(getwd())

for (i in 1:length(dated_folders)){
  
  
  sub_directories = list.files(file.path("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/2 plates/control/", dated_folders[i]))
  
  
  
  for (j in 1:length(sub_directories)){
    
    setwd(file.path("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/2 plates/control/", dated_folders[i]))
    setwd(file.path("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/2 plates/control/", dated_folders[i], sub_directories[j]))
    yolk_consumption_control2_plates(sub_directories[j], j, i)
    
    
  }
  
}

#function that calculates yolk/embryo ratio, adds a column for mom and a columns for Julian date and day of exp from raw yolk and embryo data for Heat Shock 1 plates.
yolk_consumption_heatshock1_plates = function(sub_directory, index, date){
  
  
  
  
  
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
    data_final$mom = "Higher Zone 1"
  } else if ( index == 2){
    data_final$mom = "Higher Zone 2"
  } else if ( index == 3){
    data_final$mom = "Higher Zone 3"
  } else if ( index == 4){
    data_final$mom = "Higher Zone 4"
  } else if ( index == 5){
    data_final$mom = "Lower Zone 1"
  } else if ( index == 6){
    data_final$mom = "Lower Zone 2"
  } else if ( index == 7){
    data_final$mom = "Lower Zone 3"
  } else if ( index == 8){
    data_final$mom = "Lower Zone 4"
  }
  
  #create column for treatment
  data_final$Treatment = "Heat Shock"
  
  
  #create columns for date
  if (date == 1){
    data_final$Day_of_Exp = "4"
    data_final$Julian_date = "15"
  } else if (date == 2){
    data_final$Day_of_Exp = "12"
    data_final$Julian_date = "23"
  }else if (date == 3){
    data_final$Day_of_Exp = "20"
    data_final$Julian_date = "31"
  }else if (date == 4){
    data_final$Day_of_Exp = "27"
    data_final$Julian_date = "38"
  }
  
  
  #write a csv of data_final
  write.csv(data_final, "yolk_consumption_data.csv")
  
}

setwd("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/1 plates/heat shock/")

dated_folders = list.files(getwd())

for (i in 1:length(dated_folders)){
  
  
  sub_directories = list.files(file.path("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/1 plates/heat shock/", dated_folders[i]))
  
  
  
  for (j in 1:length(sub_directories)){
    
    setwd(file.path("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/1 plates/heat shock/", dated_folders[i]))
    setwd(file.path("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/1 plates/heat shock/", dated_folders[i], sub_directories[j]))
    yolk_consumption_heatshock1_plates(sub_directories[j], j, i)
    
    
  }
  
}


#function that calculates yolk/embryo ratio, adds a column for mom and a columns for Julian date and day of exp from raw yolk and embryo data for Heat Shock 2 plates.
yolk_consumption_heatshock2_plates = function(sub_directory, index, date){
  
  
  
  
  
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
    data_final$mom = "Higher Zone 5"
  } else if ( index == 2){
    data_final$mom = "Higher Zone 6"
  } else if ( index == 3){
    data_final$mom = "Higher Zone 7"
  } else if ( index == 4){
    data_final$mom = "Higher Zone 8"
  } else if ( index == 5){
    data_final$mom = "Lower Zone 5"
  } else if ( index == 6){
    data_final$mom = "Lower Zone 6"
  } else if ( index == 7){
    data_final$mom = "Lower Zone 7"
  } else if ( index == 8){
    data_final$mom = "Lower Zone 8"
  }
  
  #create a column for treatment 
  data_final$Treatment = "Heat Shock"
  
  #create columns for date, change these values depending on which folders you're processing! 
  #re-run this function if you change the dates!  
  if (date == 1){
    data_final$Day_of_Exp = "4"
    data_final$Julian_date = "15"
  } else if (date == 2){
    data_final$Day_of_Exp = "12"
    data_final$Julian_date = "23"
  }else if (date == 3){
    data_final$Day_of_Exp = "21"
    data_final$Julian_date = "32"
  }else if (date == 4){
    data_final$Day_of_Exp = "28"
    data_final$Julian_date = "39"
  }

  #write a csv of data_final
  write.csv(data_final, "yolk_consumption_data.csv")
  
}



setwd("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/2 plates/heat shock/")

dated_folders = list.files(getwd())

for (i in 1:length(dated_folders)){
  
  
  sub_directories = list.files(file.path("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/2 plates/heat shock/", dated_folders[i]))
  
  

  for (j in 1:length(sub_directories)){
    
    setwd(file.path("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/2 plates/heat shock/", dated_folders[i]))
    setwd(file.path("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/2 plates/heat shock/", dated_folders[i], sub_directories[j]))
    yolk_consumption_heatshock2_plates(sub_directories[j], j, i)
    
    
  }
  
}


#same function as above, however, rewritten for special mixed plates that included a variety of embryos from a variety of treatments and moms. Chane date values depending on which folders you're processing.
yolk_consumption_MIXED_PLATE_2_14 = function(sub_directory, index){
  
  
  
  
  
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
    data_final$mom = "Higher Zone 1"
  } else if ( index == 2){
    data_final$mom = "Higher Zone 4"
  } else if ( index == 3){
    data_final$mom = "Higher Zone 6"
  } else if ( index == 4){
    data_final$mom = "Higher Zone 8"
  } else if ( index == 5){
    data_final$mom = "Higher Zone 1"
  } else if ( index == 6){
    data_final$mom = "Higher Zone 4"
  } else if ( index == 7){
    data_final$mom = "Higher Zone 6"
  } else if ( index == 8){
    data_final$mom = "Higher Zone 8"
  }
  
  if (index == 1){
    data_final$Treatment = "Control"
  } else if ( index == 2){
    data_final$Treatment = "Control"
  } else if ( index == 3){
    data_final$Treatment = "Control"
  } else if ( index == 4){
    data_final$Treatment= "Control"
  } else if ( index == 5){
    data_final$Treatment= "Heat Shock"
  } else if ( index == 6){
    data_final$Treatment = "Heat Shock"
  } else if ( index == 7){
    data_final$Treatment = "Heat Shock"
  } else if ( index == 8){
    data_final$Treatment = "Heat Shock"
  }
  
  
  #create columns for date, change these values depending on which folders you're processing! 
  #re-run this function if you change the dates!  
  data_final$Day_of_Exp = "34"
  data_final$Julian_date = "45"
  
  
  #write a csv of data_final
  write.csv(data_final, "yolk_consumption_data.csv")
  
}

setwd("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/mixed plate photos/2_14_Mixed Plate/")
sub_directories =  list.files(file.path(getwd()))

for (i in 1:length(sub_directories)){
  setwd(file.path("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/mixed plate photos/2_14_Mixed Plate/", sub_directories[i]))
  yolk_consumption_MIXED_PLATE(sub_directories[i], i)
}

yolk_consumption_MIXED_PLATE_2_22 = function(sub_directory, index){
  
  
  
  
  
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
    data_final$mom = "Higher Zone 1"
  } else if ( index == 2){
    data_final$mom = "Higher Zone 6"
  } else if ( index == 3){
    data_final$mom = "Higher Zone 8"
  } else if ( index == 4){
    data_final$mom = "Higher Zone 6"
  } else if ( index == 5){
    data_final$mom = "Higher Zone 8"
  }
  
  if (index == 1){
    data_final$Treatment = "Control"
  } else if ( index == 2){
    data_final$Treatment = "Control"
  } else if ( index == 3){
    data_final$Treatment = "Control"
  } else if ( index == 4){
    data_final$Treatment= "Heat Shock"
  } else if ( index == 5){
    data_final$Treatment= "Heat Shock"
  }
  
  
  #create columns for date, change these values depending on which folders you're processing! 
  #re-run this function if you change the dates!  
  data_final$Day_of_Exp = "42"
  data_final$Julian_date = "53"
  
  
  #write a csv of data_final
  write.csv(data_final, "yolk_consumption_data.csv")
  
}

setwd("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/mixed plate photos/2_22_mixed_plate/")
sub_directories =  list.files(file.path(getwd()))

for (i in 1:length(sub_directories)){
  setwd(file.path("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/mixed plate photos/2_22_mixed_plate/", sub_directories[i]))
  yolk_consumption_MIXED_PLATE_2_22(sub_directories[i], i)
}

yolk_consumption_MIXED_PLATE_2_29 = function(sub_directory, index){
  
  
  
  
  
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
    data_final$mom = "Higher Zone 6"
  } else if ( index == 2){
    data_final$mom = "Higher Zone 6"
  }
  
  if (index == 1){
    data_final$Treatment = "Control"
  } else if ( index == 2){
    data_final$Treatment= "Heat Shock"
  }
  
  
  #create columns for date, change these values depending on which folders you're processing! 
  #re-run this function if you change the dates!  
  data_final$Day_of_Exp = "49"
  data_final$Julian_date = "60"
  
  
  #write a csv of data_final
  write.csv(data_final, "yolk_consumption_data.csv")
  
}

setwd("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/mixed plate photos/2_29_MIXED_PLATE/")
sub_directories =  list.files(file.path(getwd()))

for (i in 1:length(sub_directories)){
  setwd(file.path("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/mixed plate photos/2_29_MIXED_PLATE/", sub_directories[i]))
  yolk_consumption_MIXED_PLATE_2_29(sub_directories[i], i)
}

#function that binds all yolk consumption csvs from one day into master data file
read_yolk_data = function(master){
  moms = list.files(file.path(master))
  data_final = read.csv(file.path(master, moms[1], "yolk_consumption_data.csv"))
   for (j in 2:length(moms)){
      new_data = read.csv(file.path(master, moms[j], "yolk_consumption_data.csv"))
      data_final = rbind(data_final, new_data)
      
   }
  return(data_final)
}

#function that runs read_yolk_data for all dates and binds each master csv into one final data file
binder = function(treatment){
  setwd(file.path(treatment))
  dates = list.files(file.path(treatment))
  data_final = read_yolk_data(file.path(getwd(), dates[1]))
  for (i in 2:length(dates)){
    setwd(file.path(treatment))
    new_data = read_yolk_data(file.path(getwd(), dates[i]))
    data_final = rbind(data_final, new_data)
    
    
    
    
  }
  return(data_final)
}

#run binder for each all 1, 2, and mixed plates
control1 = binder("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/1 plates/control/")
control2 = binder("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/2 plates/control/")
heatshock1 = binder("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/1 plates/heat shock/")
heatshock2 = binder("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/2 plates/heat shock/")
mixed_plates = binder("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/mixed plate photos/")

#bind all csvs together to make master data file!
master_winter_yolk_raw_data = rbind(control1, control2, heatshock1, heatshock2, mixed_plates)

master_winter_yolk_raw_data = read.csv("C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/master_winter_yolk_raw_data.csv")

#trim out dead embryos by standard dummy embryo area
master_winter_yolk_raw_data = filter(master_winter_yolk_raw_data, embryo_area != "4399939")

#add elevation column to dataframe
master_winter_yolk_raw_data = arrange(master_winter_yolk_raw_data, mom)
master_winter_yolk_raw_data$Elevation = NA
master_winter_yolk_raw_data$Elevation[1:789] = "Higher"
master_winter_yolk_raw_data$Elevation[790:1277] = "Lower"

write.csv(master_winter_yolk_raw_data, "C:/Users/joemh/Desktop/1_12_Exp_Processed_Tifs/1_12_Exp/master_winter_yolk_raw_data.csv")
