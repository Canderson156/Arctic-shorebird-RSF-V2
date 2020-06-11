

#### Coordinates for exporting as csv - wide format

g0_no_coords <- readRDS("Robjects/g0_no_coords.RDS")
good_g1 <- readRDS("Robjects/good_g1.RDS")
good_g2 <- readRDS("Robjects/good_g2.RDS")
good_g2a <- readRDS("Robjects/good_g2a.RDS")
good_g2b <- readRDS("Robjects/goood_g2b.RDS")
good_g3_gis <- readRDS("Robjects/good_g3_gis.RDS")
good_g3a_alert_dups  <- readRDS("Robjects/good_g3a_alert_dups.RDS")
good_g3a_alert_multi  <- readRDS("Robjects/good_g3a_alert_multi.RDS")
good_g3a_alert_ch  <- readRDS("Robjects/good_g3a_alert_ch.RDS")

all_points <- readRDS("Robjects/all_points.RDS")
all_polygons <- readRDS("Robjects/all_polygons.RDS")

allplots <- readRDS("Robjects/allplots.RDS")





######################
###################### Points - Lat/Long
######################




all_points_LL <- as.data.frame(all_points) %>%
  mutate(geometry = str_remove(geometry, "c\\("), geometry = str_remove(geometry, "\\)")) %>%
  separate(geometry, c("long_1", "lat_1"), ", ")




######################
###################### Polygons - Lat/Long
######################


#version with alert polygons as convex hulls
all_polygons_LL <- rbind(good_g1, good_g2, good_g3_gis, good_g3a_alert_ch)

#create a id column for for merging plot names and coordinates
all_polygons_LL$L2 <- 1:nrow(all_polygons_LL)

#remove coordinates from the geometry column
all_polygons_coords <- data.frame(st_coordinates(all_polygons_LL)) %>%
  select(-L1)

#remove the geometry column
all_polygons_LL <- as.data.frame(all_polygons_LL) %>%
  select(-geometry)

#Merge plot names and coordinates
all_polygons_LL <- merge(all_polygons_LL, all_polygons_coords) %>%
  select(-L2) %>%
  distinct()

#group pairs of coordinates into a single column, and index the corner numbers of polygon
all_polygons_LL <- all_polygons_LL %>%
  unite(coords, X,Y) %>%
  group_by(Plot) %>%
  mutate(index = 1:n()) %>%
  ungroup()

#dataframe of grouped coordinate column names, and the two column names they will be split into
coord_cols <- data.frame(col = paste("coords", 1:max(all_polygons_LL$index), sep = "_"),
                         long = paste("long", 1:max(all_polygons_LL$index), sep = "_"),
                         lat = paste("lat", 1:max(all_polygons_LL$index), sep = "_"),
                         stringsAsFactors = FALSE)

#put the coordinate pairs into their own columns
all_polygons_LL <- all_polygons_LL %>%
  mutate(index = paste("coords", index, sep = "_")) %>%
  spread(index, coords)

#re-order the columns to ascending order
all_polygons_LL <- all_polygons_LL[, c("Plot", coord_cols$col )]


#split each pair into two columns
for(i in 1:nrow(coord_cols)){
  col <- coord_cols$col[i]
  long <- coord_cols$long[i]
  lat <- coord_cols$lat[i]
  all_polygons_LL <- separate(all_polygons_LL, col, c(long, lat), sep = "_")}





######################
###################### Points - UTM
######################





all_zones <- allplots %>%
  select(Plot, UTM_Zone = GIS_UTM_1_zone)

#Convert points to UTM

all_points_utm <- merge(all_points, all_zones)

# create a list of all utm zones in the dataset and their relevent crs
utms83 <- sort(unique(all_points_utm$UTM_Zone))

utm_crs83 <- vector(mode = "list", length = length(utms83))
epsg83 <- utms83 + 26900

for(i in 1:length(utms83)){
  crstext <- paste("+init=EPSG:", epsg83[i], sep = "")
  utm_crs83[[i]] <- CRS(crstext)
}
names(utm_crs83) <- utms83



# split into a list where each element is the plots in one utm zone
points_by_utm83 <- group_split(all_points_utm , UTM_Zone)
names(points_by_utm83) <- utms83



#give each element of the list it's appropriate CRS
for(i in 1:length(utms83)){
  points_by_utm83[[i]] <- st_transform(points_by_utm83[[i]], utm_crs83[[i]])
}

#convert to dataframe, separate into northing and easting
points_by_utm83 <- lapply(points_by_utm83, as.data.frame)
points_by_utm83 <- lapply(points_by_utm83, mutate, geometry = str_remove(geometry, "c\\("), geometry = str_remove(geometry, "\\)"))
points_by_utm83 <- lapply(points_by_utm83, separate, geometry, c("UTM_1_Easting", "UTM_1_Northing"), ", ")



#merge all UTM groups
all_points_utm <-  rbind(points_by_utm83[[1]], points_by_utm83[[2]])
for(i in 3:length(points_by_utm83)){
  all_points_utm <- rbind(all_points_utm, points_by_utm83[[i]])
}

all_points_utm <- all_points_utm %>%
  mutate(UTM_1_Easting = ifelse(UTM_1_Easting == "NaN", NA, UTM_1_Easting),
         UTM_1_Northing = ifelse(UTM_1_Northing == "NaN", NA, UTM_1_Northing),
         UTM_1_Easting = as.numeric(UTM_1_Easting),
         UTM_1_Northing = as.numeric(UTM_1_Northing))





######################
###################### Polygons - UTM
######################



#version with alert polygons as convex hulls
all_polygons_utm <- rbind(good_g1, good_g2, good_g3_gis, good_g3a_alert_ch)

#add utm zones
all_polygons_utm  <- merge(all_polygons_utm, all_zones)


# create a list of all utm zones in the dataset and their relevent crs
utms83 <- sort(unique(all_polygons_utm$UTM_Zone))

utm_crs83 <- vector(mode = "list", length = length(utms83))
epsg83 <- utms83 + 26900

for(i in 1:length(utms83)){
  crstext <- paste("+init=EPSG:", epsg83[i], sep = "")
  utm_crs83[[i]] <- CRS(crstext)
}
names(utm_crs83) <- utms83


# split into a list where each element is the plots in one utm zone
polygons_by_utm83 <- group_split(all_polygons_utm , UTM_Zone)
names(polygons_by_utm83) <- utms83



#give each element of the list it's appropriate CRS
for(i in 1:length(utms83)){
  polygons_by_utm83[[i]] <- st_transform(polygons_by_utm83[[i]], utm_crs83[[i]])
}


#create a id column for for merging plot names and coordinates
polygons_by_utm83 <- lapply(polygons_by_utm83, function(x) mutate(x, L2 = 1:nrow(x)))

#remove coordinates from the geometry column
coords_by_utm83 <- lapply(polygons_by_utm83, function(x) data.frame(st_coordinates(x)))
coords_by_utm83 <- lapply(coords_by_utm83, function(x) select(x, -L1))

#remove the geometry column
polygons_by_utm83 <- lapply(polygons_by_utm83, as.data.frame)
polygons_by_utm83 <- lapply(polygons_by_utm83, select, -geometry)

#Merge plot names and coordinates
for(i in 1:length(polygons_by_utm83)){
  polygons_by_utm83[[i]] <- merge(polygons_by_utm83[[i]], coords_by_utm83[[i]]) %>%
    select(-L2) %>%
    distinct()}

#group pairs of coordinates into a single column, and index the corner numbers of polygon
polygons_by_utm83 <- lapply(polygons_by_utm83, unite, coords, X, Y)
polygons_by_utm83 <- lapply(polygons_by_utm83, group_by, Plot)
polygons_by_utm83 <- lapply(polygons_by_utm83, function(x) mutate(x, index = 1:n()))
polygons_by_utm83 <- lapply(polygons_by_utm83, ungroup)


#put the coordinate pairs into their own columns
polygons_by_utm83 <- lapply(polygons_by_utm83, function(x) mutate(x, index = paste("coords", index, sep = "_")))
polygons_by_utm83 <- lapply(polygons_by_utm83, spread, index, coords)


coord_cols2 <- data.frame(col = paste("coords", 1:44, sep = "_"),
                          UTM_Easting = paste("UTM", 1:44, "Easting", sep = "_"),
                          UTM_Northing = paste("UTM", 1:44, "Northing", sep = "_"),
                          stringsAsFactors = FALSE)



#add missing columns for the ones that didn't have all 44
add <- data.frame(matrix(ncol = nrow(coord_cols2), nrow = 0))
colnames(add) <- coord_cols2$col
polygons_by_utm83 <- lapply(polygons_by_utm83, merge, add, all = TRUE)



#re-order the columns to ascending order
polygons_by_utm83 <- lapply(polygons_by_utm83, function(x) x[, c("Plot", "UTM_Zone", coord_cols2$col )])

#split each pair into two columns
for(j in 1:length(polygons_by_utm83)){
  for(i in 1:nrow(coord_cols2)){
    col <- coord_cols2$col[i]
    UTM_Easting <- coord_cols2$UTM_Easting[i]
    UTM_Northing <- coord_cols2$UTM_Northing[i]
    polygons_by_utm83[[j]] <- separate(polygons_by_utm83[[j]], col, c(UTM_Easting, UTM_Northing), sep = "_")}}


#merge all UTM groups
all_polygons_utm <-  rbind(polygons_by_utm83[[1]], polygons_by_utm83[[2]])
for(i in 3:length(polygons_by_utm83)){
  all_polygons_utm <- rbind(all_polygons_utm, polygons_by_utm83[[i]])
}



######################
###################### Combine all together
######################

all_LL <- merge(all_polygons_LL, all_points_LL, all = TRUE)

all_UTM <- merge(all_points_utm, all_polygons_utm, all = TRUE)

all_coordsWIDE <- merge(all_UTM, all_LL)

write.csv(all_coordsWIDE, "exported/all_coordsWIDE.csv")





