

#coordsALL2 is all plots, with multiple enties if they sometimes have different coordinates, and duplicates for the ones with unkown UTM zone
#Remove the plots that only have a southwest corner, and come back to them later
#I'm not sure if I should try to plot the perimeter ones with all of isabels coordiantes, or if I should create a box based on SW corner
#Come back to this later, and only do the straighforward cases now


plot_coords <- bind_rows(g5_partial_coords, g7_duplicated, g8_good_plots) %>%
  select(-contains("Type"), -contains("UTM_5")) %>%
  arrange(Plot) %>%
  mutate(PlotX = paste(Plot, 1:length(Plot), sep = "_"), dup = allDuplicated(Plot))








# create a list of all utm zones in the dataset and their relevent crs
utms <- sort(unique(plot_coords$UTM_Zone))

utm_crs <- vector(mode = "list", length = length(utms))
epsg <- utms + 26900

for(i in 1:length(utms)){
  crstext <- paste("+init=EPSG:", epsg[i], sep = "")
  utm_crs[[i]] <- CRS(crstext)
}
names(utm_crs) <- utms



##################################

#Make plot_coords long instead of wide


plot_coords <- plot_coords %>%
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
plots_by_utm <- group_split(plot_coords, UTM_Zone)
names(plots_by_utm) <- utms



# use above to create list of polygons by UTM zone
prism_polys_utm <- lapply(plots_by_utm, df_to_stPolygons,  "Easting", "Northing", "PlotX")





#give each element of the list it's appropriate CRS
for(i in 1:length(utms)){
  st_crs(prism_polys_utm[[i]]) <- st_crs(utm_crs[[i]])
}





#####################################




#reproject each one

#### change this projection to whatever tyler's are in
prism_polys_proj_list <- lapply(prism_polys_utm, st_transform, crs = NPLAEA)



#combine them together


prism_polys_proj <-  rbind(prism_polys_proj_list[[1]], prism_polys_proj_list[[2]])
for(i in 3:length(prism_polys_proj_list)){
  prism_polys_proj <- rbind(prism_polys_proj, prism_polys_proj_list[[i]])
}


prism_polys_proj <- prism_polys_proj %>%
  separate(PlotX, c("Plot", "X"), sep = "_", remove = FALSE) %>%
  select(-X)


###########################################################################################################################
###########################################################################################################################



saveRDS(prism_polys_proj, "Robjects/prism_polys_proj_no27.RDS")
