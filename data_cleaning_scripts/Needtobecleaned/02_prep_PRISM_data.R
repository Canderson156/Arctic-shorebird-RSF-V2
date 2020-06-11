








### Filtering to all unique surveys (no counts) ###########################################################################################################################



#create a dataset of all surveyed plots

# all of the plots that I've inlcluded use GIS_1 as the southwest corner. 
# Also, since some plots are missing one corner, or are perimiters, I think it would be best to 

allsurveys <- dplyr::select(prism,
                          Plot,
                          Survey_Lead,
                          Year, 
                          Month, 
                          Day,
                          Date,
                          yday,
                          Start_time_1,
                          Region_name, 
                          Region_code, 
                          Sub_region_name, 
                          Sub_region_code, 
                          Plot_type,
                          Survey_method,
                          Plot_Shape,
                          #Quality_1,
                          #Quality_2, 
                          quality,
                          #Prop_surveyed, 
                          Selection_method, 
                          Plot_area,
                          #n_surveyors, 
                          comparison, 
                          plot_date,
                          plot_year,
                          Human_development_details,
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
                          Map_datum
                          )

allsurveys <- distinct(allsurveys) 







### Filtering to all unique plots (no counts) ###########################################################################################################################








### filtering to only shorebird data ###########################################################################################################################



## only shorebird observations


sb_list <- prism %>%
  filter(Group == "Shorebirds") %>%
  select(Species) %>%
  unique() %>%
  na.omit() %>%
  pull(Species)
  
  
#all shorebirds, all years, all surveys
#sb <- prism %>%
#  filter(Group == "Shorebirds") %>%


#all shorebirds, all years, mean of within year surveys
sb <- prism %>%
  filter(Group == "Shorebirds") %>%
  group_by(plot_year, Species) %>%
  mutate(mean_birds_year = mean(total_birds)) %>%
  select(Plot,
         Species,
         Survey_Lead,
         Year, 
         Region_name, 
         Region_code, 
         Sub_region_name, 
         Sub_region_code, 
         Plot_type,
         Survey_method,
         Plot_Shape,
         quality,
         Selection_method, 
         Plot_area,
         comparison,
         Human_development_details,
         plot_year,
         plot_species,
         mean_birds_year) %>%
  distinct()




#all shorebirds, mean between years (from mean within years)


sb <- sb %>%
  ungroup() %>%
  group_by(Plot) %>%
  ungroup() %>%
  group_by(plot_species) %>%
  mutate(mean_birds_all = mean(mean_birds_year)) %>%
  select(Plot,
         Species,
         Survey_Lead,
         Region_name, 
         Region_code, 
         Sub_region_name, 
         Sub_region_code, 
         Plot_type,
         Survey_method,
         #Plot_Shape, # a few plots differ between years
         #quality, # a few plots differ between years
         Selection_method, 
         Plot_area,
         comparison,
         plot_species,
         mean_birds_all) %>%
  distinct()
  

# above, with all shorebird species added together

SB <- sb %>%
  ungroup() %>%
  group_by(Plot) %>%
  mutate(sum_shorebirds = sum(mean_birds_all)) %>%
  select(Plot,
         #Species,
         Survey_Lead,
         Region_name, 
         Region_code, 
         Sub_region_name, 
         Sub_region_code, 
         Plot_type,
         Survey_method,
         #Plot_Shape, # a few plots differ between years
         #quality, # a few plots differ between years
         Selection_method, 
         #Plot_area,
         comparison,
         #plot_species,
         sum_shorebirds) %>%
  distinct()



SB <- SB %>%
  merge(allplots, all = TRUE) %>%
  mutate(sum_shorebirds = ifelse(is.na(sum_shorebirds), 0, round(sum_shorebirds)))



## use my expand grid from previous data cleaning script when I want to model by species





### Adding goose data ###########################################################################################################################


geese_sp <- c("CAGO", "SNGO", "ROGO", "CACG", "GWFG")

geese_plots <- prism %>%
  filter(Species %in% geese_sp) %>%
  select(Plot,
         plot_date,
         plot_year,
         total_birds) %>%
  group_by(plot_date) %>% 
  mutate(sum_geese_survey = sum(total_birds)) %>%
  ungroup() %>%
  select(-plot_date, -total_birds) %>%
  distinct() %>%
  group_by(plot_year) %>%
  mutate(mean_geese_year = mean(sum_geese_survey)) %>%
  ungroup() %>%
  select(-plot_year, -sum_geese_survey) %>%
  distinct() %>%
  group_by(Plot) %>% 
  mutate(mean_geese = round(mean(mean_geese_year))) %>%
  ungroup() %>%
  select(-mean_geese_year) %>%
  distinct()
  
  
#would I rather add this to the sb object that has individual species rather than summaries for all shorebirds? 
SB <- SB %>%
  merge(geese_plots, all = TRUE) %>%
  mutate(mean_geese = ifelse(is.na(mean_geese), 0, mean_geese))

rm(geese_sp, geese_plots, sb)


