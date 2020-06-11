
#### Finalizing coordinates for Group 3




g3_yes_gis <- readRDS("Robjects/g3_yes_gis.RDS")
Alert_shapefile <- readRDS("Robjects/Alert_shapefile.RDS")
GIS_shapefiles <- readRDS("Robjects/GIS_shapefiles.RDS")


######################
###################### Group 3
######################



good_g3_gis <- GIS_shapefiles %>%
  filter(Plot %in% g3_yes_gis$Plot) %>%
  distinct(Plot) %>%
  mutate(dup = allDuplicated(Plot)) %>%
  filter(dup == FALSE) %>%
  select(-dup)


######################
###################### Group 3a
######################

# Alert plots that have multiple polygons - multiple rows per polygons with one polygon in each row
good_g3a_alert_dups <- Alert_shapefile %>%
  st_as_sf() %>%
  select(Plot = FN, Historic_Plot = LABEL) %>%
  filter(Plot %in% g3_yes_gis$Plot) %>%
  distinct(Plot, Historic_Plot) %>%
  mutate(dup = allDuplicated(Plot)) %>%
  filter(dup == TRUE)  %>%
  select(-dup)


# Alert plots that have multiple polygons - one row per polygon with a multipolygon geometry
good_g3a_alert_multi <- good_g3a_alert_dups %>%
  group_by(Plot) %>%
  summarise_all(first) %>%
  select(-Historic_Plot) %>%
  st_cast()


# Alert plots that have multiple polygons - convex hulls
good_g3a_alert_ch <- st_convex_hull(good_g3a_alert_multi)


# All alert plots (duplicated and unduplicated)
alert_ids <- bigdata_raw %>%
  select(Standardized_Plot_Name, Sub_region_name) %>%
  distinct() %>%
  filter(Sub_region_name == "Alert")


alert_all <- good_g3a_alert_dups %>%
  select(-Historic_Plot) %>%
  rbind(good_g3_gis[good_g3_gis$Plot %in% alert_ids$Standardized_Plot_Name,]) %>%
  mutate(dup = allDuplicated(Plot))
 

# Plotting Alert plots

#ggplot(good_g3a_alert_dups) +
#  geom_sf(aes(fill = Plot)) +
#  scale_fill_brewer(palette="Set3")
#
#ggplot(good_g3a_alert_ch) +
#  geom_sf(aes(fill = Plot))
#
#ggplot(alert_all) +
#  geom_sf(aes(fill = dup))





#SaveRDS

saveRDS(good_g3_gis, "Robjects/good_g3_gis.RDS")
saveRDS(good_g3a_alert_dups , "Robjects/good_g3a_alert_dups.RDS")
saveRDS(good_g3a_alert_multi , "Robjects/good_g3a_alert_multi.RDS")
saveRDS(good_g3a_alert_ch , "Robjects/good_g3a_alert_ch.RDS")

        

