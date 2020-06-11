
#### Creating subsets of the big prism table that are useful for analysis





######################
###################### Correct flaws in the full dataset (bigdata.RDS)
######################



bigdata <- bigdata_raw


#changing variable classes 

#Nas come from:
#Day, Day, Date: 477 "not applicable" and 1 "not recorded"
#Propotion surveyed: not applicable - final decision, not applicable - nest visit, not recorded
#Sighting code: 31 "not applicable - habitat description"
#Counts: 1 "not recorded". this appears in many columns becasue it is an invensive plot that is missing a final decision form


suppressWarnings(bigdata <- bigdata %>%
                   mutate_at(c("Day", "Month", "Proportion_of_Plot_surveyed", 
                               "Sighting_code", "Sub_region_code", "Count_Nests_found", 
                               "Count_Probable_nest", "Count_Pairs", "Count_Male", 
                               "Count_Female", "Count_Unknown_sex"), as.numeric) %>%
                   mutate(Date = mdy(Date)))



#not done yet:
#start: character to posixct     # these contain two different formats: 00:00 and 00:00:00. 
#end: character to posixct       # would need to write code to get rid of the sconds from the one that have it, then combine the date with this time to make a date-time object 
#duration: character to posixct   #this is being importanted incorrectly. even in excel sheet, need to change format to h:mm for it to make sense



#adding day of year variable

bigdata$yday <- yday(bigdata$Date)



# fixing capitalizations to make all consistent within a variable

bigdata$Plot_type <- toupper(bigdata$Plot_type)
bigdata$Plot_Selection_Method <- tolower(bigdata$Plot_Selection_Method)
bigdata$Standardized_Species_Code <- toupper(bigdata$Standardized_Species_Code)



#making all no data values into NAs

bigdata <- bigdata %>%
  mutate(Group = ifelse(Group == "not applicable", NA, Group)) %>%
  mutate(Group = ifelse(Group == "not recorded", NA, Group)) %>%
  mutate(Start_time_1 = ifelse(Start_time_1 %in% c("not recorded", "not applicable", "no data"), NA, Start_time_1))


#getting rid of one blank cell in human development

bigdata$Human_development_details[bigdata$Human_development_details == " "] <- "not recorded"



#edit the time column so that formats are uniform 
#NAs don't parse well with lubridate. 
#I haven't bothered turning in times with hm() becasue of the NAs becasue only planning to use it to make ID vars right now

bigdata <- bigdata %>%
  mutate(Start_time_1 = ifelse(nchar(Start_time_1) == 4, paste("0", Start_time_1, sep = ""), Start_time_1),
         Start_time_1 = ifelse(nchar(Start_time_1) == 8, substr(Start_time_1, 1, 5), Start_time_1))





######################
###################### Select only the columns with data that I'm interested in (prism.RDS)
######################


# select only the columns containing required data
prism <- select(bigdata,
                Plot = Standardized_Plot_Name, #2855 unique plots
                Survey_Lead,
                Year, #1994-2019
                Month, #mostly June, some July, 475 "not applicable"
                Day,
                Date,
                yday,
                Start_time_1,
                Region_name, #89 plots with NAs that are outside the regions (south of Mackenzie Delta)
                Region_code, #89 plots with NAs that are outside the regions (south of Mackenzie Delta)
                Sub_region_name, #170 plots with NAs. 89 as above. 78 in Foxe basin becasue of differences in how subregions were calculated over time (2019 had no subregions). 3 in North Archipelago are confusing, but may stem from weird subregions that overlap near Alert (see email from Laurent RE Tyler's map of subregions)
                Sub_region_code, #same as above
                Plot_type,
                Survey_method,
                Plot_Shape,
                Quality_1 = GIS_Habitat_Quality_2_Code,
                Quality_2 = GIS_Habitat_Quality_1_Code, # how were these defined, when were they changed, which one is the one we should use, are these being redone?
                Prop_surveyed = Proportion_of_Plot_surveyed, 
                Selection_method = Plot_Selection_Method, 
                Plot_area = Plot_area_km2,
                Sighting_code, 
                Species = Standardized_Species_Code,  #XXXX means nothing was observed
                Group,
                Count_Nests_found,
                Count_Probable_nest,
                Count_Pairs,
                Count_Male,
                Count_Female,
                Count_Unknown_sex,
                #n_surveyors,
                Human_development_details, 
                Map_datum,
                Plot_center_long = Plot_Center_X_coordinate..long.easting.,
                Plot_center_lat = Plot_Centre_Y_coordinate..lat.northing.,
                contains("UTM")) %>%
  select(-contains("Wildlife"))




######################
###################### Details for each unique plot (allplots.RDS)
######################



allplots <- prism %>%
  select(Plot,
         Plot_type,
         Survey_Lead,
         Selection_method,
         Region_code,
         Map_datum,
         Plot_center_long,
         Plot_center_lat,
         contains("UTM")) %>%
  distinct()




#Save RDS
saveRDS(bigdata, "Robjects/bigdata.RDS")
saveRDS(prism, "Robjects/prism.RDS")
saveRDS(allplots, "Robjects/allplots.RDS")

