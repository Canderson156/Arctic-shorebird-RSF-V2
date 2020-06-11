

#coordsALL2 is all plots, with multiple enties if they sometimes have different coordinates, and duplicates for the ones with unkown UTM zone
#Remove the plots that only have a southwest corner, and come back to them later
#I'm not sure if I should try to plot the perimeter ones with all of isabels coordiantes, or if I should create a box based on SW corner
#Come back to this later, and only do the straighforward cases now

plot_coords <- coordsALL2 %>%
  filter(not.na(UTM_4_Easting)) %>%
  filter(not.na(UTM_2_Easting)) %>%
  filter(UTM_2_Type != "perimeter") %>%
  select(-contains("Type")) %>%
  arrange(Plot) %>%
  mutate(PlotX = paste(Plot, 1:length(Plot), sep = "_"), dup = duplicated(Plot))



####################################


# create a list of all utm zones in the dataset and their relevent crs
utms <- sort(unique(plot_coords$UTM_Zone))

utm_crs <- vector(mode = "list", length = length(utms))
epsg <- seq(from = 26907, length.out = length(utms))

for(i in 1:length(utms)){
  crstext <- paste("+init=EPSG:", epsg[i], sep = "")
  utm_crs[[i]] <- CRS(crstext)
}
names(utm_crs) <- utms



##################################

#Make plot_coords long instead of wide


plot_coords2 <- plot_coords %>%
  unite("SW", contains("1")) %>%
  unite("NW", contains("2")) %>%
  unite("NE", contains("3")) %>%
  unite("SE", contains("4")) %>%
  mutate(SW2 = SW) %>%
  gather("corner", "coords", SW, NW, NE, SE, SW2) %>%
  separate(coords, c("Easting", "Northing"), sep = "_") %>%
  mutate(Easting = as.numeric(Easting), Northing = as.numeric(Northing))
  
  




##################################



# split into a list where each element is the plots in one utm zone
plots_by_utm <- group_split(plot_coords2, UTM_Zone)
names(plots_by_utm) <- utms

# use above to create list of polygons by UTM zone
prism_polys_utm <- lapply(plots_by_utm, df_to_SpatialPolygons, keys = "PlotX", coords = c("Easting", "Northing"), CRS())

#give each element of the list it's appropriate CRS
for(i in 1:length(utms)){
  proj4string(prism_polys_utm[[i]]) <- utm_crs[[i]]
}





#####################################




#reproject each one

#### change this projection to whatever tyler's are in
prism_polys_proj_list <- lapply(prism_polys_utm, spTransform, CRSobj = NPLAEA)

#give each list element the same names as prism_polys_utm 

for(i in 1:length(utms)){
  names(prism_polys_proj_list[[i]]@polygons) <- names(prism_polys_utm[[i]]@polygons)
}


#combine them together


prism_polys_proj <-  bind(prism_polys_proj_list[[1]], prism_polys_proj_list[[2]])
for(i in 3:length(prism_polys_proj_list)){
  prism_polys_proj <- bind(prism_polys_proj, prism_polys_proj_list[[i]])
}






#####################################

#get plot names for all polygons

poly_names <- plots_by_utm[[1]]$Plot[plots_by_utm[[1]]$corner == "SW"]

for(i in 2:length(plots_by_utm)){
  x <- plots_by_utm[[i]]$Plot[plots_by_utm[[i]]$corner == "SW"]
  poly_names <- c(poly_names, x)
}


plot_coords3 <- allplots %>%
  select(Plot,
         Region_name, 
         Region_code, 
         Sub_region_name, 
         Sub_region_code) %>%
  filter(Plot %in% plot_coords$Plot) %>%
  arrange(Plot)

plot_coords3 <- merge(data.frame(Plot = poly_names), plot_coords3)


prism_polys_df <- SpatialPolygonsDataFrame(prism_polys_proj, as.data.frame(plot_coords3), match.ID = F)  

prism_polys_df@data$Plot <- as.character(prism_polys_df@data$Plot)


## about 6 plots have coordinates that are visually wrong





