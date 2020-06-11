


# compare GIS data vs excel data

## all geogrpahic data should be extracted from the GIS files as if may be unreliable in the excel spreadsheet: 


#Weird shapes in excel file - do we see the same weird shapes in GIS files? 
#do the GIS files represent what WAS done or what SHOUld have been done on paper?




#for the comparisons with the shapefiles, I think I'll want to do each type of corner seperately. 
#SW is the majority, and most of those have 4 points, althought I saw some with 2
#NE and NW ones from Arviat are single points
#most of the NA ones it appears that Isabelle just doesn't know them, there were some that it seemed weirder that they were missing



# the prism polygons perhaps should just be points. I'm not sure how this will affect analysis downstream



################################################################


#create a simpfied geospatial refererence dataset for each plot, then combine it with the desired data after
#remove the plots that don't have utm zone associated


plot_coords <- allplots %>%
  select(Plot,
         Region_name, 
         Region_code, 
         Sub_region_name, 
         Sub_region_code, 
         UTM_Zone, 
         UTM_Easting, 
         UTM_Northing) %>%
  filter(not.na(UTM_Zone)) %>%
  filter(not.na(UTM_Easting))




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

# split into a list where each element is the plots in one utm zone
plots_by_utm <- group_split(plot_coords, UTM_Zone)
names(plots_by_utm) <- utms

# use above to create list of polygons by UTM zone
prism_polys_utm <- lapply(plots_by_utm, df_to_SpatialPolygons, keys = "Plot", coords = c("UTM_Easting", "UTM_Northing"), CRS())

#give each element of the list it's appropriate CRS
for(i in 1:length(utms)){
  proj4string(prism_polys_utm[[i]]) <- utm_crs[[i]]
}


#####################################


#reproject each one

#### change this projection to whatever tyler's are in
prism_polys_proj_list <- lapply(prism_polys_utm, spTransform, CRSobj = WGS84)


#combine them together


prism_polys_proj <-  bind(prism_polys_proj_list[[1]], prism_polys_proj_list[[2]])
for(i in 3:length(prism_polys_proj_list)){
  prism_polys_proj <- bind(prism_polys_proj, prism_polys_proj_list[[i]])
}




#do they have any data associated?

#turn into a spatial polygon with plot ID and region

##broken
#plots_id <- do.call(rbind.data.frame, allplots4) %>%
#  select(plot, region, quality) %>%
#  unique()

#this name isn't intuitive, I called plot_coords2 only becasue it was a subset of plot_coords
plot_coords2 <- plot_coords %>%
  select(Plot,
         Region_name, 
         Region_code, 
         Sub_region_name, 
         Sub_region_code, 
         Corner = UTM_1_Type)
  

prism_polys_df <- SpatialPolygonsDataFrame(prism_polys_proj, as.data.frame(plot_coords2$Plot), match.ID = F)  



#about 4 plots visually seem obviously wrong, not in canada


