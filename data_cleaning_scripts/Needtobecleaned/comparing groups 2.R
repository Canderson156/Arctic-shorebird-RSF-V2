


#This version only has 5 UTM columns. Easier to manage when testing



source("data cleaning scripts/01_library.R")
GIS_shapefiles<- readRDS("Robjects/GIS_shapefiles.RDS")



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
                UTM_1_Status,
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
                Map_datum,
                Plot_center_long = Plot_Center_X_coordinate..long.easting.,
                Plot_center_lat = Plot_Centre_Y_coordinate..lat.northing.)



#start = Start_time_1,
#end = Finish_time_1,
#duration = DERIVED_Survey_Duration..hours.minutes.





allplots <- prism %>%
  select(Plot,
         Plot_type,
         Survey_Lead,
         Selection_method,
         Region_code,
         UTM_Zone,
         UTM_1_Type,
         UTM_1_Easting, 
         UTM_1_Northing,
         UTM_1_Status,
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
         Map_datum,
         Plot_center_long,
         Plot_center_lat) %>%
  distinct()





## add in the missing data from rasmussen 2019


plots_ras_2019_raw <- read.csv("data/PRISM/Rasmussen_PlotCoordinates_2019s.csv") 


plots_ras_2019 <- plots_ras_2019_raw %>%
  select(-Lat, -Long) %>%
  rename(Plot = Plot_Name) %>%
  unite(coords, UTM_E, UTM_N) %>%
  spread(Corner, coords) %>%
  separate(SW, c("UTM_1_Easting", "UTM_1_Northing")) %>%
  separate(NW, c("UTM_2_Easting", "UTM_2_Northing")) %>%
  separate(NE, c("UTM_3_Easting", "UTM_3_Northing")) %>%
  separate(SE, c("UTM_4_Easting", "UTM_4_Northing")) %>%
  mutate(UTM_1_Type = "SW", UTM_2_Type = "NW", UTM_3_Type = "NE", UTM_4_Type = "SE", 
         UTM_1_Status = "field", UTM_5_Type = NA, UTM_5_Easting = NA, UTM_5_Northing = NA) 


allplots_ras_2019 <- allplots %>%
  filter(Plot %in% plots_ras_2019$Plot) %>%
  select(-contains("UTM"))

allplots_sub <- allplots %>%
  filter(Plot %notin% plots_ras_2019$Plot)


allplots_ras_2019 <- merge(allplots_ras_2019, plots_ras_2019, by = "Plot")

allplots <- rbind(allplots_sub, allplots_ras_2019)

rm(allplots_ras_2019, allplots_sub, plots_ras_2019)

## add in missing data from reconnaissance plots

plots_rec_2007_raw <- read.csv("data/PRISM/2007 Reconnaissance waypoints.csv")



plots_rec_2007 <- plots_rec_2007_raw %>%
  select(Plot = Standardized_Plot_Name, Easting, Northing, Order) %>%
  drop_na() %>%
  unite(coords, Easting, Northing) %>%
  mutate(Order = paste("coord_", Order, sep = "")) %>%
  spread(Order, coords) %>%
  separate(coord_1, c("UTM_1_Easting", "UTM_1_Northing"), sep = "_") %>%
  separate(coord_2, c("UTM_2_Easting", "UTM_2_Northing"), sep = "_") %>%
  separate(coord_3, c("UTM_3_Easting", "UTM_3_Northing"), sep = "_") %>%
  separate(coord_4, c("UTM_4_Easting", "UTM_4_Northing"), sep = "_") %>%
  separate(coord_5, c("UTM_5_Easting", "UTM_5_Northing"), sep = "_") %>%
  separate(coord_6, c("UTM_6_Easting", "UTM_6_Northing"), sep = "_") %>%
  separate(coord_7, c("UTM_7_Easting", "UTM_7_Northing"), sep = "_") %>%
  separate(coord_8, c("UTM_8_Easting", "UTM_8_Northing"), sep = "_") %>%
  separate(coord_9, c("UTM_9_Easting", "UTM_9_Northing"), sep = "_") %>%
  separate(coord_10, c("UTM_10_Easting", "UTM_10_Northing"), sep = "_") %>%
  separate(coord_11, c("UTM_11_Easting", "UTM_11_Northing"), sep = "_") %>%
  separate(coord_12, c("UTM_12_Easting", "UTM_12_Northing"), sep = "_") 


#reduced version that has the same columns as allplots

plots_rec_2007_sm <- plots_rec_2007_raw %>%
  select(Plot = Standardized_Plot_Name, Easting, Northing, Order) %>%
  drop_na() %>%
  unite(coords, Easting, Northing) %>%
  mutate(Order = paste("coord_", Order, sep = "")) %>%
  spread(Order, coords) %>%
  separate(coord_1, c("UTM_1_Easting", "UTM_1_Northing"), sep = "_") %>%
  separate(coord_2, c("UTM_2_Easting", "UTM_2_Northing"), sep = "_") %>%
  separate(coord_3, c("UTM_3_Easting", "UTM_3_Northing"), sep = "_") %>%
  separate(coord_4, c("UTM_4_Easting", "UTM_4_Northing"), sep = "_") %>%
  separate(coord_5, c("UTM_5_Easting", "UTM_5_Northing"), sep = "_") %>%
  select(-coord_6, -coord_7, -coord_8, -coord_9, -coord_10, -coord_11, -coord_12) %>%
  mutate(UTM_1_Type = "point in line", UTM_2_Type = "point in line", UTM_3_Type = "point in line", UTM_4_Type = "point in line", 
         UTM_5_Type = "point in line", UTM_1_Status = "LAT_LONG")


allplots_rec_2007 <- allplots %>%
  filter(Plot %in% plots_rec_2007_sm$Plot) %>%
  rename(Zone = UTM_Zone) %>%
  select(-contains("UTM")) %>%
  rename(UTM_Zone = Zone)

allplots_sub2 <- allplots %>%
  filter(Plot %notin% plots_rec_2007_sm$Plot)


allplots_rec_2007 <- merge(allplots_rec_2007, plots_rec_2007_sm, by = "Plot")

allplots <- rbind(allplots_sub2, allplots_rec_2007)

rm(allplots_rec_2007, allplots_sub2, plots_rec_2007_sm, plots_rec_2007)

###remove anything past 5
#add in columns that aren't in allplots
#merge with allplots
#be careful, these are lat/longs, not utms. should I convert to UTM to help out ISAbel? 








#make a versions that I will subtract entries from to prevent duplicates
allplots2 <- allplots %>%
  mutate(PlotX = paste(Plot, 1:length(Plot), sep = "_"), dup = allDuplicated(Plot))


### divide into groups that I want to check





################Plots that we should not use the GIS files



#group 1: field modified plots


g1_field_modified <- allplots2 %>%
  filter(Selection_method == "field modified gis selected") %>%
  arrange(Region_code)

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g1_field_modified$PlotX)


table(g1_field_modified$UTM_1_Status)


###One of these plots is missing all coordinates



############### Plots that we could use the GIS files but we don't appear to have them


#Group 2: no GIS plots that I've uploaded so far

g2_no_gis <- allplots2 %>%
  filter(Plot %notin% GIS_shapefiles$Plot) 


allplots2 <- allplots2 %>%
  filter(PlotX %notin% g2_no_gis$PlotX)








################# Plots that have no coordinates in either dataset



g0_no_coords <- g2_no_gis %>%
  filter(is.na(UTM_1_Easting)) %>%
  mutate(UTM_1_Easting = Plot_center_long, UTM_1_Northing = Plot_center_lat, 
         UTM_1_Status = ifelse(is.na(UTM_1_Easting), NA, "LAT_LONG"))


  

g2_no_gis <- g2_no_gis %>%
  filter(not.na(UTM_1_Easting))



LL1 <- g0_no_coords %>%
  filter(UTM_1_Status == "LAT_LONG")

LL2 <- g2_no_gis %>%
  filter(UTM_1_Status == "LAT_LONG")

g2a_needs_transformation <- rbind(LL1, LL2)


g0_no_coords <- g0_no_coords %>%
  filter(PlotX %notin% g2a_needs_transformation $PlotX)

g2_no_gis <- g2_no_gis %>%
  filter(PlotX %notin% g2a_needs_transformation $PlotX)




table(g0_no_coords$Plot_type)




###relating back to g2

table(g2_no_gis$Plot_type, g2_no_gis$Survey_Lead)
#there are 2 duplicates: SHI-3261 (CWS Rapid and) PAL-0079 (Industry Rapid)

table(g2_no_gis$Plot_type[is.na(g2_no_gis$UTM_1_Easting)], g2_no_gis$Survey_Lead[is.na(g2_no_gis$UTM_1_Easting)])


table(g2_no_gis$UTM_1_Status)
#143 plots were "GIS confirmed" but I don't have these in the GIS files. Look for them?




test <- g2_no_gis %>%
  filter(Plot_type == "RAPID") %>%
  filter(Survey_Lead == "Canadian Wildlife Service")

table(test$Selection_method, is.na(test$UTM_1_Easting))






############### Plots that we can use the GIS files
g3_yes_gis <- allplots2 %>%
  arrange(Region_code)




nrow(g0_no_coords)+ nrow(g1_field_modified) + nrow(g2_no_gis) + nrow(g2a_needs_transformation) + nrow(g3_yes_gis) == nrow(allplots)





####################################################################################
#subcategories of group 3


#Group A: Plots with duplicate entries

### re do this use the dup column
g3a_duplicated <- allplots2 %>%
  filter(allDuplicated(Plot) == TRUE)

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g3a_duplicated$PlotX)






#group B: Field selected plots


g3b_field_selected <- allplots2 %>%
  filter(Selection_method == "field selected")

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g3b_field_selected$PlotX)

table(allDuplicated(g3b_field_selected$Plot))


#Group C: plots that have no coordinates at all


g3c_no_coords <- allplots2 %>%
  filter(is.na(UTM_1_Easting))


allplots2 <- allplots2 %>%
  filter(PlotX %notin% g3c_no_coords$PlotX)


#Group D: Plots that have more than 4 corners


g3d_perimeter <- allplots2 %>%
  filter(UTM_2_Type == "perimeter")

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g3d_perimeter$PlotX)




#Group E: Plots that only have a single corner

g3e_point <- allplots2 %>% 
  filter(is.na(UTM_2_Northing))


allplots2 <- allplots2 %>%
  filter(PlotX %notin% g3e_point$PlotX)



#Group F: Plots missing one coord

g3f_partial_coords <- allplots2 %>%
  filter(is.na(UTM_2_Easting) | is.na(UTM_4_Easting))

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g3f_partial_coords$PlotX)




#Group G: Normal plots

g3g_normal <- allplots2




nrow(g3a_duplicated) + nrow(g3b_field_selected) + nrow(g3c_no_coords) + nrow(g3d_perimeter) +
  nrow(g3e_point) + nrow(g3f_partial_coords) + nrow(g3g_normal) == nrow(g3_yes_gis)



#Group 6: missing UTM zone
#
#g6_no_zone <- allplots2 %>%
#  filter(is.na(UTM_Zone))
#
#allplots2 <- allplots2 %>%
#  filter(PlotX %notin% g6_no_zone$PlotX)
#
#currently nothing. there are 7 plots in group 1 (no GIS)







