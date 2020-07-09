#longer version
obs_lc <- bigdata_raw %>%
  select(Standardized_Plot_Name, 
         Year,
         GIS_Habitat_Quality_1_Code,
         GIS_Habitat_Quality_2_Code,
         GIS_Habitat_Quality_1_Code_General_Desc,
         GIS_Habitat_Quality_2_Code_General_Desc,
         Total_Snow_cover_percent_in_plot,
         General_Plot_Description,
         Habitat_data_recorded,
         Habitat_Protocol,
         Habitat_Protocol_Integration_method,
         Comment_Habitat_description_protocol,
         Habitat_Quality_Code_GIS_and_Field_Match,
         Upland_Habitat_percent_of_Plot:LOW_Landform_Comment_4_Type) %>%
  distinct() %>%
  filter(Standardized_Plot_Name %in% all_polygons_df$Plot) %>% 
  group_by(Standardized_Plot_Name) %>%
  mutate(count = n())




#smaller version. removed the plots that have conflicting information within the same year

obs_lc <- bigdata_raw %>%
  select(Plot = Standardized_Plot_Name, 
         Year,
         Total_Snow_cover_percent_in_plot,
         Upland_Habitat_percent_of_Plot,
         Lowland_Habitat_percent_of_Plot,
         Permanent_Water_percent_of_plot,
         Pond_._of_water,
         Lake_._of_water,                                                            
         River.Stream_._of_water,                                                   
         Ocean_._of_water, 
         UP_Standing_Water,                                                         
         UP_Barren,                                                                 
         UP_Moss.Lichen,                                                            
         UP_Graminoid,                                                             
         UP_Herbs,                                                                
         UP_Dwarf_Shrub_Heath,                                                     
         UP_total_shrub_cover,
         LOW_Standing_Water,                                                     
         LOW_Barren,                                                                
         LOW_Moss.Lichen,                                                        
         LOW_Graminoid,                                                           
         LOW_Herbs,                                                               
         LOW_Dwarf_Shrub_Heath,                                                  
         LOW_total_shrub_cover
         ) %>%
  distinct() %>%
  #filter(Plot %in% all_polygons_df$Plot) %>% 
  group_by(Plot, Year) %>%
  mutate(count = n()) %>%
  #filter(count == 1) %>%
  ungroup()

snow1 <- obs_lc %>%
  select(Plot, Total_Snow_cover_percent_in_plot) %>%
  distinct()

snow2 <- group %>%
  select(Plot, group_ice_snow) %>%
  distinct()

snow <- snow1 %>%
  merge(snow2) %>%
  filter(Total_Snow_cover_percent_in_plot != "not recorded") %>%
  mutate(Total_Snow_cover_percent_in_plot = as.numeric(Total_Snow_cover_percent_in_plot))

gram1 <- obs_lc %>%
  select(Plot, UP_Graminoid, LOW_Graminoid,Upland_Habitat_percent_of_Plot, Lowland_Habitat_percent_of_Plot) %>%
  filter_all(all_vars(. != "not recorded")) %>%
  distinct() %>%
  mutate(UP_Graminoid2 = (as.numeric(UP_Graminoid)*0.01)*(as.numeric(Upland_Habitat_percent_of_Plot)*0.01),
         LOW_Graminoid2 = (as.numeric(LOW_Graminoid)*0.01)*(as.numeric(Lowland_Habitat_percent_of_Plot)*0.01),
         total_gram_obs = UP_Graminoid2 + LOW_Graminoid2) %>%
  select(Plot, total_gram_obs) %>%
  group_by(Plot) %>%
  mutate(count = n()) %>%
  filter(count == 1) %>%
  ungroup()

gram2 <- group %>%
  select(Plot, group_graminoid, group_wetland) %>%
  distinct() %>%
  mutate(group_gram_wetland = group_graminoid + group_wetland)


gram <- gram1 %>% 
  merge(gram2) %>%
  select(-count) %>%
  mutate(dif_gram = total_gram_obs - group_graminoid,
         dif_wet = total_gram_obs - group_wetland,
         dif_both = total_gram_obs - (group_graminoid+group_wetland))



quantile(abs(gram$dif_gram))
quantile(abs(gram$dif_wet))
quantile(abs(gram$dif_both))

#adding wetlands doesn't improve accuracy of graminoid




water1 <- obs_lc %>%
  select(Plot, Permanent_Water_percent_of_plot) %>%
  filter_all(all_vars(. != "not recorded")) %>%
  distinct() %>%
  mutate(Permanent_Water_percent_of_plot = as.numeric(Permanent_Water_percent_of_plot)) %>%
  group_by(Plot) %>%
  mutate(count = n()) %>%
  filter(count == 1) %>%
  ungroup() %>%
  select(-count)
  
  
  
water2 <- group %>%
  select(Plot, group_water) %>%
  distinct()

water <- water1 %>%
  merge(water2) %>%
  mutate(dif = (Permanent_Water_percent_of_plot*0.01) - group_water)

hist(water$dif[])
quantile(abs(water$dif))



#not useful, few observations oh habitat, mostly NAs
shadow <- obs_lc %>%
  filter(Plot %in% group$Plot[group$group_shadow > 0.05])

