
#choose a small subset of plots that seem to be worked correctly


plot_coords <- g3_yes_gis %>%
  filter(is.na(UTM_5_Easting)) %>%
  filter(Plot_type == "RAPID") %>%
  filter(Selection_method == "gis selected") %>%
  drop_na(UTM_1_Easting, UTM_2_Easting, UTM_3_Easting, UTM_4_Easting) %>%
  select(-c(UTM_5_Easting, UTM_5_Northing, UTM_5_Type, 
            Plot_center_long, Plot_center_lat, dup, PlotX,
            UTM_1_Status, UTM_1_Type, UTM_2_Type, UTM_3_Type, UTM_4_Type)) %>%
  drop_na() %>%
  sample_n(100) %>%
  arrange(Region_code) %>%
  mutate(PlotX = paste(Plot, 1:length(Plot), sep = "_"), dup = allDuplicated(Plot))
  
  
  

#### seperate by map datum


plot_coords27 <- plot_coords %>%
  filter(Map_datum == "NAD 1927")


plot_coords83 <- plot_coords %>%
  filter(Map_datum == "NAD 1983 Transverse Mercator")




###########################################################################################################################
###########################################################################################################################

##Conversion process for NAD 1927 Plots




# create a list of all utm zones in the dataset and their relevent crs
utms27 <- sort(unique(plot_coords27$UTM_Zone))

utm_crs27 <- vector(mode = "list", length = length(utms27))
epsg27 <- utms27 + 26700

for(i in 1:length(utms27)){
  crstext <- paste("+init=EPSG:", epsg27[i], sep = "")
  utm_crs27[[i]] <- CRS(crstext)
}
names(utm_crs27) <- utms27



##################################

#Make plot_coords long instead of wide


plot_coords27 <- plot_coords27 %>%
  unite("SW", contains("1")) %>%
  unite("NW", contains("2")) %>%
  unite("NE", contains("3")) %>%
  unite("SE", contains("4")) %>%
  mutate(SW2 = SW) %>%
  gather("corner", "coords", SW, NW, NE, SE, SW2) %>%
  separate(coords, c("Easting", "Northing"), sep = "_") %>%
  mutate(Easting = as.numeric(Easting), Northing = as.numeric(Northing))  %>%
  drop_na(Easting)





##################################



# split into a list where each element is the plots in one utm zone
plots_by_utm27 <- group_split(plot_coords27, UTM_Zone)
names(plots_by_utm27) <- utms27



# use above to create list of polygons by UTM zone
prism_polys_utm27 <- lapply(plots_by_utm27, df_to_stPolygons,  "Easting", "Northing", "PlotX")





#give each element of the list it's appropriate CRS
for(i in 1:length(utms27)){
  st_crs(prism_polys_utm27[[i]]) <- st_crs(utm_crs27[[i]])
}





#####################################




#reproject each one

#### change this projection to whatever tyler's are in
prism_polys_proj_list27 <- lapply(prism_polys_utm27, st_transform, crs = NAD83)



#combine them together


prism_polys_proj27 <-  rbind(prism_polys_proj_list27[[1]], prism_polys_proj_list27[[2]])
for(i in 3:length(prism_polys_proj_list27)){
  prism_polys_proj27 <- rbind(prism_polys_proj27, prism_polys_proj_list27[[i]])
}


prism_polys_proj27 <- prism_polys_proj27 %>%
  separate(PlotX, c("Plot", "X"), sep = "_", remove = FALSE) %>%
  select(-X)


#re-project
prism_polys_proj27 <- st_transform(prism_polys_proj27, NAD83)





###########################################################################################################################
###########################################################################################################################

##Conversion process for NAD 1983 Plots


# create a list of all utm zones in the dataset and their relevent crs
utms83 <- sort(unique(plot_coords83$UTM_Zone))

utm_crs83 <- vector(mode = "list", length = length(utms83))
epsg83 <- utms83 + 26900

for(i in 1:length(utms83)){
  crstext <- paste("+init=EPSG:", epsg83[i], sep = "")
  utm_crs83[[i]] <- CRS(crstext)
}
names(utm_crs83) <- utms83



##################################

#Make plot_coords long instead of wide


plot_coords83 <- plot_coords83 %>%
  unite("SW", contains("1")) %>%
  unite("NW", contains("2")) %>%
  unite("NE", contains("3")) %>%
  unite("SE", contains("4")) %>%
  mutate(SW2 = SW) %>%
  gather("corner", "coords", SW, NW, NE, SE, SW2) %>%
  separate(coords, c("Easting", "Northing"), sep = "_") %>%
  mutate(Easting = as.numeric(Easting), Northing = as.numeric(Northing)) %>%
  drop_na(Easting)






##################################



# split into a list where each element is the plots in one utm zone
plots_by_utm83 <- group_split(plot_coords83, UTM_Zone)
names(plots_by_utm83) <- utms83



# use above to create list of polygons by UTM zone
prism_polys_utm83 <- lapply(plots_by_utm83, df_to_stPolygons,  "Easting", "Northing", "PlotX")





#give each element of the list it's appropriate CRS
for(i in 1:length(utms83)){
  st_crs(prism_polys_utm83[[i]]) <- st_crs(utm_crs83[[i]])
}





#####################################




#reproject each one

#### change this projection to whatever tyler's are in
prism_polys_proj_list83 <- lapply(prism_polys_utm83, st_transform, crs = AEA)



#combine them together


prism_polys_proj83 <-  rbind(prism_polys_proj_list83[[1]], prism_polys_proj_list83[[2]])
for(i in 3:length(prism_polys_proj_list83)){
  prism_polys_proj83 <- rbind(prism_polys_proj83, prism_polys_proj_list83[[i]])
}


prism_polys_proj83 <- prism_polys_proj83 %>%
  separate(PlotX, c("Plot", "X"), sep = "_", remove = FALSE) %>%
  select(-X)

#re-project
prism_polys_proj83 <- st_transform(prism_polys_proj83, NAD83)




###########################################################################################################################
###########################################################################################################################

##Merge 1927 and 1983 plots together

prism_polys_proj <- rbind(prism_polys_proj27, prism_polys_proj83) %>%
  select(-PlotX) 


prism_polys_proj <- merge(prism_polys_proj, plot_coords[,1:5]) %>%
  arrange(Plot)


st_write(prism_polys_proj, "exported/proj_test.shp")


good_test <- good_g3_NAD83 %>%
  filter(Plot %in% prism_polys_proj$Plot)

st_write(good_test, "exported/good_test.shp")


#save the results of this test

saveRDS(prism_polys_proj, "testing/test1_prism.RDS")
saveRDS(good_test, "testing/test1_GIS.RDS")



##############################################

## measuring distance between them


dist <- st_distance(good_test, prism_polys_proj, by_element = TRUE)
dist <- set_units(dist, km)
prism_polys_proj$dist <- dist

#2 out of 100 plots were >0m apart. 
#KWI-7980A = 242m
#SOI-0071 = 68m



