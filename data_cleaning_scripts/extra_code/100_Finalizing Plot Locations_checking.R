#finalizing plot locations: checking

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#G1 - checking why some plots are in different locations than the shapefiles
###CONCLUSION: FOR the plots that are not in the same location as the GIS, some of the plots in GIS are 1994, while the excel uses 1995 when there is an option


##Merge 1927 and 1983 plots together

test_g1 <- rbind(prism_polys_proj27, prism_polys_proj83) %>%
  select(-PlotX) %>%
  arrange(Plot)


#compare distance to GIS shapefile plots

test_gis <- GIS_shapefiles %>%
  filter(Plot %in% test_g1$Plot) %>%
  arrange(Plot)



dist <- st_distance(test_g1, test_gis, by_element = TRUE)
dist <- set_units(dist, km)
test_g1$dist <- dist

#which plots are not in the same location?

check <- test_g1 %>%
  filter(dist > as_units(0.01, "km"))

cc <- test_g1 %>%
  filter(dist < as_units(0.01, "km"))


check2 <- prism %>%
  filter(Plot %in% check$Plot) %>%
  select(-contains("UTM")) %>%
  select(Plot, Year, Plot_type, Survey_method) %>%
  distinct()


xx <- merge(check, check2)
xx2 <- allplots %>%
  filter(Plot %in% test_g1$Plot)
xx2 <- xx2[,1:30]


yr <- bigdata_raw %>%
  filter(Standardized_Plot_Name %in% check$Plot) 

#write.csv(yr, "check_RAS.csv")  






### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# G2 - fixing mistakes
###CONCLUSION: I came up with some kind of fix for all of them. document3ed in plot_typos. May still recieve some fixes from IB/PW

check <- plot_coords %>%
  filter(Plot == "SOI-0860")

plot(good_g2 $geometry[good_g2 $Plot == "PAL-0108"])


apply(check, 2, nchar)

#send to Isabel
plots_typos <- plot_coords %>%
  filter(Plot %in% c("BAK-0002",
                     "SOI-0915",
                     "PBP-INT1A18",
                     "SHI-3011",
                     "PAL-0065",
                     "PAL-0068",
                     "PAL-0107",
                     "PAL-0108",
                     "PAL-0053",
                     "PAL-0073",
                     "MGP-7006",
                     "MELI-IND14",
                     "QMW-IntG",
                     "SOI-0860",
                     "WAL-IntD",
                     "WAL-IntB",
                     "WAL-Int23",
                     "WBI-IntD"))

#write.csv(plots_typos, "plots_typos.csv")


#checked already                     

check2 <- plot_coords %>%
  filter(Plot %notin% plots_typos$Plot) %>%
  filter(!str_detect(Plot, 'PAL')) %>%
  filter(!str_detect(Plot, 'QMW')) %>%
  filter(!str_detect(Plot, 'QTP')) %>%
  filter(Plot %notin% c("PBP-INT1C15", "PBP-INT1A16", "BAK-0077", "BAK-0071", "NIG-1009"))




#for(i in 1:nrow(check2)) {
#  plot(prism_polys_proj$geometry[prism_polys_proj$Plot == check2$Plot[i]])  
#}

#these all look good



table(g1_field_modified$Plot %in% GIS_shapefiles$Plot)                     








### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



# G2 -  SEND TO TYLER
### CONCLUSION: He hasn't gotten back to me, but I don't care anymore


gis_should_exist <- g2_no_gis %>%
  rbind(g0_no_coords) %>%
  filter(Selection_method == "gis selected") %>%
  filter(Plot_type != "INTENSIVE") %>%
  arrange(Region_code, Plot_type)


write.csv(gis_should_exist, "GIS seems like it should exist.csv")



##### SEND TO JENNIE AND ISABEL

missing_coords <- g0_no_coords[,1:5] %>%
  merge(IBnotes)

write.csv(missing_coords, "missing_coords.csv")


#looking at PDF for somerset Island plots

editing_missing_coords <- bigdata_raw %>%
  filter(Standardized_Plot_Name %in% g0_no_coords$Plot) %>%
  filter(not.na(Plot_Center_X_coordinate..long.easting.)) 

write.csv(editing_missing_coords, "editing_missing_coords.csv")



test <- editing_missing_coords[,5:40] %>%
  distinct()



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


# G3 - Stuff I was sending to Laurent

historic_plot_names2 <- historic_plot_names %>%
  rename(Historic_Plot = FN)


saveRDS(good_g3_NAD83, "exported/good_plot_coords_1.RDS")
saveRDS(Alert_dups, "exported/good_plot_coords_Alert.RDS")
saveRDS(historic_plot_names2, "exported/historic_plot_names.RDS")


st_write(good_g3_NAD83, "exported/good_g3_NAD83.shp")







