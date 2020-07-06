
#### Organinzing PRISM plots into groups that need to have their coordinates treated differently



prism <- readRDS("Robjects/prism.RDS")
allplots <- readRDS("Robjects/allplots.RDS")
GIS_shapefiles<- readRDS("Robjects/GIS_shapefiles.RDS")





######################
###################### Add missing coordinates to allplots
######################



## add in the missing data from rasmussen 2019

#import data
plots_ras_2019_raw <- read.csv("data/PRISM/Rasmussen_PlotCoordinates_2019s.csv") 

#adjust columns to be compatible with allplots
plots_ras_2019 <- plots_ras_2019_raw %>%
  select(-Lat, -Long, -UTM_Zone) %>%
  rename(Plot = Plot_Name) %>%
  unite(coords, UTM_E, UTM_N) %>%
  spread(Corner, coords) %>%
  separate(SW, c("UTM_1_Easting", "UTM_1_Northing")) %>%
  separate(NW, c("UTM_2_Easting", "UTM_2_Northing")) %>%
  separate(NE, c("UTM_3_Easting", "UTM_3_Northing")) %>%
  separate(SE, c("UTM_4_Easting", "UTM_4_Northing")) %>%
  mutate(UTM_1_Type = "SW", UTM_2_Type = "NW", UTM_3_Type = "NE", UTM_4_Type = "SE", 
         UTM_1_Status = "field", UTM_5_Type = NA, UTM_5_Easting = NA, UTM_5_Northing = NA) 

#names of the columns I want to use the new data from 
ras_cols <- colnames(plots_ras_2019)[colnames(plots_ras_2019) %notin% c("Plot", "UTM_Zone")]

#select rasmussen plots that I want to use the old data for
allplots_ras_2019 <- allplots %>%
  filter(Plot %in% plots_ras_2019$Plot) %>%
  select(-one_of(ras_cols))

#the rest of the allplots data that is not being edited, for recombining with edited data
allplots_sub <- allplots %>%
  filter(Plot %notin% plots_ras_2019$Plot)

#merge old and new rasmussen data
allplots_ras_2019 <- merge(allplots_ras_2019, plots_ras_2019, by = "Plot")

#merge edited rasmussen data with the rest of allplots
allplots <- rbind(allplots_sub, allplots_ras_2019)




######################
###################### Grouping coordinates
######################




#make a version of allplots that I will subtract entries from to prevent duplicates
allplots2 <- allplots %>%
  mutate(PlotX = paste(Plot, 1:length(Plot), sep = "_"), dup = allDuplicated(Plot))



############### group 0: plots that have no coordinates anywhere

g0_no_coords <- allplots2 %>%
  filter(Plot %notin% GIS_shapefiles$Plot) %>%
  filter(is.na(UTM_1_Easting)) %>%
  mutate(UTM_1_Easting = Plot_center_long, UTM_1_Northing = Plot_center_lat, 
         UTM_1_Status = ifelse(is.na(UTM_1_Easting), NA, "LAT_LONG_DM")) %>%
  filter(is.na(UTM_1_Easting))
  

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g0_no_coords$PlotX)




############### group 1: field modified plots 


g1_field_modified <- allplots2 %>%
  filter(Selection_method == "field modified gis selected") %>%
  filter(not.na(UTM_1_Easting)) %>%
  filter(not.na(UTM_4_Easting)) %>%
  filter(Plot %in% GIS_shapefiles$Plot) %>%
  arrange(Region_code)


allplots2 <- allplots2 %>%
  filter(PlotX %notin% g1_field_modified$PlotX)




############### Group 2: no GIS plots 

g2_no_gis <- allplots2 %>%
  filter(Plot %notin% GIS_shapefiles$Plot) 


allplots2 <- allplots2 %>%
  filter(PlotX %notin% g2_no_gis$PlotX)




################# Separate out plots with LAT/LONG instead of UTM




g2_no_gis <- g2_no_gis %>%
  mutate(UTM_1_Easting = ifelse(is.na(UTM_1_Easting), Plot_center_lat, UTM_1_Easting),
         UTM_1_Northing = ifelse(is.na(UTM_1_Northing), Plot_center_long, UTM_1_Northing),
         UTM_1_Status = ifelse(is.na(UTM_1_Status), "LAT_LONG_DM", UTM_1_Status))


g2a_needs_transformation <- g2_no_gis %>%
  filter(UTM_1_Status %in% c("LAT_LONG_DD", "LAT_LONG_DM"))


g2_no_gis <- g2_no_gis %>%
  filter(UTM_1_Status %notin% c("LAT_LONG_DD", "LAT_LONG_DM"))



############# Seperate out plots with only one coordinate


g2b_point <- g2_no_gis %>%
  filter(is.na(UTM_4_Easting))

g2_no_gis <- g2_no_gis %>%
  filter(not.na(UTM_4_Easting))




############### Plots that we can use the GIS files


g3_yes_gis <- allplots2 %>%
  arrange(Region_code)



############### Test if all of these groups are equal to the total number of plots

nrow(g0_no_coords)+ nrow(g1_field_modified) + nrow(g2_no_gis) + nrow(g2a_needs_transformation) + nrow(g2b_point) + nrow(g3_yes_gis) == nrow(allplots)


#Save RDS

saveRDS(g0_no_coords, "Robjects/g0_no_coords.RDS")
saveRDS(g1_field_modified, "Robjects/g1_field_modified.RDS")
saveRDS(g2_no_gis, "Robjects/g2_no_gis.RDS")
saveRDS(g2a_needs_transformation, "Robjects/g2a_needs_transformation.RDS")
saveRDS(g2b_point, "Robjects/g2b_point")
saveRDS(g3_yes_gis, "Robjects/g3_yes_gis.RDS")







