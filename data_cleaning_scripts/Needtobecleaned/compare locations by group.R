

# all surveys has 3117 observations
# all plots has 2457 observations. this includes some duplicates
# there are 2433 unique plots

#there are 2855 unique plots in bigdata. which ones did I get rid of?
#304 plots that had no rapid data associaterd with them
#11 Plot_type = RECONNAISSANCE plots
#21 plot from PCI 1996
#86 plots from 2019



### start with a more full version of PRISM. might as well check all of the plots that I can




### Editing the raw data ###########################################################################################################################


# Create a version for editing / use for resetting without having to re-load from csv

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


#count the number of surveyors

#for(i in 1:nrow(bigdata)) {
#  bigdata$n_surveyors[i] <- count.not.na(c(
#    bigdata$Surveyor_1_Initials[i],
#    bigdata$Surveyor_2_Initials[i],
#    bigdata$Surveyor_3_Initials[i],
#    bigdata$Surveyor_4_Initials[i], 
#    bigdata$Surveyor_5_Initials[i],
#    bigdata$Surveyor_6_Initials[i]))
#}



#edit the time column so that formats are uniform 
#NAs don't parse well with lubridate. 
#I haven't bothered turning in times with hm() becasue of the NAs becasue only planning to use it to make ID vars right now

bigdata <- bigdata %>%
  mutate(Start_time_1 = ifelse(nchar(Start_time_1) == 4, paste("0", Start_time_1, sep = ""), Start_time_1),
         Start_time_1 = ifelse(nchar(Start_time_1) == 8, substr(Start_time_1, 1, 5), Start_time_1))


  
#######################################################################################################################


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
                Selection_method = Plot_Selection_Method, # why are there so many field selected plots?
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
                UTM_Zone = GIS_UTM_1_zone,
                UTM_1_Type,
                UTM_1_Easting, 
                UTM_1_Northing,
                UTM_2_Type,
                UTM_2_Easting, 
                UTM_2_Northing,
                UTM_3_Type,
                UTM_3_Easting, 
                UTM_3_Northing,
                UTM_4_Type,
                UTM_4_Easting, 
                UTM_4_Northing,
                UTM_5_Type,
                UTM_5_Easting, 
                UTM_5_Northing,
                Map_datum)


#start = Start_time_1,
#end = Finish_time_1,
#duration = DERIVED_Survey_Duration..hours.minutes.




#remove the plots that weren't rapid plots
## decided to keep the rapid surveys of intensive plots

#prism <- prism %>%
#  filter(Survey_method == "rapid") %>%
#  filter(Plot_type !="RECONNAISSANCE") 




allplots <- prism %>%
  select(Plot,
         Plot_type,
         Survey_Lead,
         Region_code,
         UTM_Zone,
         UTM_1_Type,
         UTM_1_Easting, 
         UTM_1_Northing,
         UTM_2_Type,
         UTM_2_Easting, 
         UTM_2_Northing,
         UTM_3_Type,
         UTM_3_Easting, 
         UTM_3_Northing,
         UTM_4_Type,
         UTM_4_Easting, 
         UTM_4_Northing,
         UTM_5_Type,
         UTM_5_Easting, 
         UTM_5_Northing,
         Map_datum) %>%
  distinct()



#make a versions that I will subtract entries from to prevent duplicates
allplots2 <- allplots %>%
  mutate(PlotX = paste(Plot, 1:length(Plot), sep = "_"), dup = duplicated(Plot))


### divide into groups that I want to check




#Group 1: no GIS plots that I've uploaded so far

g1_no_gis <- allplots2 %>%
  filter(Plot %notin% GIS_shapefiles$Plot) %>%
  arrange(Region_code)

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g1_no_gis$PlotX)
  


#Group 2: plots that have no coordinates at all


g2_no_coords <- allplots2 %>%
  filter(is.na(UTM_1_Easting))


allplots2 <- allplots2 %>%
  filter(PlotX %notin% g2_no_coords$PlotX)


#Group 3: Plots that have more than 4 corners


g3_perimeter <- allplots2 %>%
  filter(UTM_2_Type == "perimeter")

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g3_perimeter$PlotX)


#test <- GIS_shapefiles %>%
#  filter(Plot == "ALE-3840")
#plot(test)
#
#test <- GIS_shapefiles %>%
#  filter(Plot %in% g3_perimeter$Plot)


#Maybe check the ones that there is only a single plot? 
#The ones that are broken up into multiple chunks I could check manually.
#I'm not sure if I could use these ones anyways




#Group 4: Plots that only have a single corner

g4_point <- allplots2 %>% 
  filter(is.na(UTM_2_Northing))
  

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g4_point$PlotX)


test <- GIS_shapefiles %>%
  filter(Plot %in% g4_point$Plot)


#measure the distance for these and see if they are all similar

#Group 5: 

# one plot is missing UTM_2_Easting, the rest are mossing UTM_4
g5_partial_coords <- allplots2 %>%
  filter(is.na(UTM_2_Easting) | is.na(UTM_4_Easting))
  
allplots2 <- allplots2 %>%
  filter(PlotX %notin% g5_partial_coords$PlotX)
  
#same thing as group 4. might want to seperate the one plot that is missing utm 2 from the ones that are missing utm 4


#Group 6: missing UTM zone

g6_no_zone <- allplots2 %>%
  filter(is.na(UTM_Zone))

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g6_no_zone$PlotX)
  
#currently nothing. there are 7 plots in group 1 (no GIS)



#Group 7: Plots with duplicate entries


dplots <- data.frame(Plot = allplots2$Plot[duplicated(allplots2$Plot)])

g7_duplicated <- allplots2 %>%
  filter(Plot %in% dplots$Plot)

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g7_duplicated$PlotX)

#compare to GIS and pick one. most seem different by 1m 
#some of them also seem like typos that could be identified by plotting



#Group 8: 

g8_good_plots <- allplots2




nrow(g1_no_gis) + nrow(g2_no_coords) + nrow(g3_perimeter) + nrow(g4_point) +
  nrow(g5_partial_coords) + nrow(g6_no_zone) + nrow(g7_duplicated) + nrow(g8_good_plots) == nrow(allplots)



### which groups do I want to turn into plots?


plot_coords <- bind_rows(g5_partial_coords, g7_duplicated, g8_good_plots)
      
               
plot_points <- g4_point          #no duplicates           


plot_perimeter <- bigdata %>%     #contains some duplicates
  select(Plot = Standardized_Plot_Name,
         Plot_type,
         Region_code,
         contains("UTM")) %>%
  filter(Plot %in% g3_perimeter$Plot) %>%
  distinct()

#g1 has no gis plots to compare to
#g2 I can use whatever is in GIS
#g6 is all of them




write.csv(g1_no_gis, "comparing_plot_coords/missing_gis.csv")










