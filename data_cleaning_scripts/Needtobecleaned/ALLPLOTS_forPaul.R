prismplots2 <- bigdata %>%
  select(
                  Plot = Standardized_Plot_Name, #2855 unique plots
                  Region_name, #89 plots with NAs that are outside the regions (south of Mackenzie Delta)
                  Region_code, #89 plots with NAs that are outside the regions (south of Mackenzie Delta)
                  Sub_region_name, #170 plots with NAs. 89 as above. 78 in Foxe basin becasue of differences in how subregions were calculated over time (2019 had no subregions). 3 in North Archipelago are confusing, but may stem from weird subregions that overlap near Alert (see email from Laurent RE Tyler's map of subregions)
                  Sub_region_code, #same as above
                  GIS_UTM_1_zone,
                  UTM_1_Easting, 
                  UTM_1_Northing,
                  UTM_1_Type) %>%
  distinct() %>%
  group_by(Plot) %>%
  slice(1)
  