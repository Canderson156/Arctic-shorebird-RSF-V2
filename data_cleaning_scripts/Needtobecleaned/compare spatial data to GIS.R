

## rename FN to Plat so that it will match Isabel's dataset

GIS_shapefiles@data$Plot <- GIS_shapefiles@data$FN

## remove the GIS shapefiles that aren't in the list of plots I want to check

GIS_check1 <- GIS_shapefiles[GIS_shapefiles@data$Plot %in% prism_polys_df@data$Plot,] 


## make a version of Isabel's plots that matches this one

PRISM_check1 <- prism_polys_df[prism_polys_df@data$Plot %in% GIS_check1@data$Plot,]


##### Both of these have some duplicates, I'm not sure how distance measured would be affected


### export these as shapefiles

writeOGR(obj=GIS_check1, dsn="exported", layer="GIS_check1", driver="ESRI Shapefile")

writeOGR(obj=PRISM_check1, dsn="exported", layer="PRISM_check1", driver="ESRI Shapefile")




#turn st objects into sf objects

GIS_check1 <- st_as_sf(GIS_check1) %>%
  arrange(Plot) %>%
  filter(duplicated(Plot) == FALSE) 



PRISM_check1 <- st_as_sf(PRISM_check1) %>%
  arrange(Plot) %>%
  filter(duplicated(Plot) == FALSE)



## measureding distance between them

dist <- st_distance(GIS_check1, PRISM_check1, by_element = TRUE)
dist <- set_units(dist, km)
dist <- as.numeric(dist)

### looks like they just don't match. I need to figure out how to look at them




#check in gis. it looks liek the prism ones have the wrong name
#work back through to see if this is an error I created or something messed up in Isabel's files
# test plot: WSC-0035A

test <- prism[prism$Plot == "WSC-0035A",]
#it must be something I did. this is correctly in UTM 13. if it was in soper, it would be at 18


test <- plot_coords[plot_coords$Plot == "WSC-0035A",]
#still good at this point

prism_polys_utm 
# correctly in UTM 13 list here









###########################################


##Which plots don't have a GIS shapefile?

MissingGIS <- prism_polys_df@data$Plot[prism_polys_df@data$Plot %notin% GIS_shapefiles@data$Plot] %>%
  unique()
# 9 duplicates
# I could find out if I have these by importing more shapefiles


##Which of Tyler's GIS plots aren't in Isabel's database plots?

MissingObservations <- GIS_shapefiles@data$Plot[GIS_shapefiles@data$Plot %notin% prism_polys_df@data$Plot] %>%
  unique()
# 27 duplicates
# Maybe they were plots that were supposed to be surveyed and weren't?
  

