
#### Finalizing coordinates for Group 1




g1_field_modified <- readRDS("Robjects/g1_field_modified.RDS")





######################
###################### Group by datum
######################


#Select the necessary columns

plot_coords <- g1_field_modified %>%
  select(-Plot_center_long, -Plot_center_lat, -GIS_UTM_2_zone, -GIS_UTM_3_zone, -GIS_UTM_4_zone, 
         -contains("Type"), -contains("Order"), -contains("Status"), -Comment_UTM) %>%
  mutate(Map_datum = ifelse(Map_datum == "not recorded", "NAD 1983 Transverse Mercator", Map_datum)) %>%
  rename(UTM_Zone = GIS_UTM_1_zone)


#### seperate by map datum


plot_coords27 <- plot_coords %>%
  filter(Map_datum == "NAD 1927")


plot_coords83 <- plot_coords %>%
  filter(Map_datum == "NAD 1983 Transverse Mercator")




######################
###################### Converting NAD 1927 Plots
######################




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




cols <- plot_coords27 %>%
  select(contains("UTM"), -UTM_Zone) %>%
  colnames()


test <- plot_coords27
corners <- character()

for(i in 1:44){
  c1 <- paste("UTM_", i, "_Easting", sep = "")
  c2 <- paste("UTM_", i, "_Northing", sep = "")
  name <- paste("corner", i, sep = "")
  test <- unite(test, !!name, c(!!c1, !!c2))
  corners <- c(corners, name)
}


plot_coords27 <- test %>%
  gather("corner", "coords", one_of(corners)) %>%
  filter(coords != "NA_NA") %>%
  separate(coords, c("Easting", "Northing"), sep = "_") %>%
  mutate(Easting = as.numeric(Easting), Northing = as.numeric(Northing)) 





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
#for(i in 3:length(prism_polys_proj_list27)){
#  prism_polys_proj27 <- rbind(prism_polys_proj27, prism_polys_proj_list27[[i]])
#}


prism_polys_proj27 <- prism_polys_proj27 %>%
  separate(PlotX, c("Plot", "X"), sep = "_", remove = FALSE) %>%
  select(-X)






######################
###################### Converting NAD 1983 Plots
######################





######################################

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




cols <- plot_coords83 %>%
  select(contains("UTM"), -UTM_Zone) %>%
  colnames()


test <- plot_coords83
corners <- character()

for(i in 1:44){
  c1 <- paste("UTM_", i, "_Easting", sep = "")
  c2 <- paste("UTM_", i, "_Northing", sep = "")
  name <- paste("corner", i, sep = "")
  test <- unite(test, !!name, c(!!c1, !!c2))
  corners <- c(corners, name)
}


plot_coords83 <- test %>%
  gather("corner", "coords", one_of(corners)) %>%
  filter(coords != "NA_NA") %>%
  separate(coords, c("Easting", "Northing"), sep = "_") %>%
  mutate(Easting = as.numeric(Easting), Northing = as.numeric(Northing)) 





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
prism_polys_proj_list83 <- lapply(prism_polys_utm83, st_transform, crs = NAD83)



#combine them together


prism_polys_proj83 <-  rbind(prism_polys_proj_list83[[1]], prism_polys_proj_list83[[2]])
for(i in 3:length(prism_polys_proj_list83)){
  prism_polys_proj83 <- rbind(prism_polys_proj83, prism_polys_proj_list83[[i]])
}


prism_polys_proj83 <- prism_polys_proj83 %>%
  separate(PlotX, c("Plot", "X"), sep = "_", remove = FALSE) %>%
  select(-X)



######################
###################### Merge NAD 1927 and NAD 1983 plots together
######################

good_g1 <- rbind(prism_polys_proj27, prism_polys_proj83) %>%
  select(-PlotX) %>%
  arrange(Plot)

#save RDS
saveRDS(good_g1, "Robjects/good_g1.RDS")


