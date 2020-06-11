### tring to export the coordiantes in the same format





test <- st_coordinates(PRISM_checkOne[1,])
test <- as.data.frame(round(test[1:4,1:2]))
test <- unite(test, both, X, Y)

df <- data.frame()
df <- rbind(df, test$both)
colnames(df) <- c("c1", "c2", "c3", "c4")
df2 <- df %>%
  separate("c1", c("LON1", "LAT1")) %>%
  separate("c2", c("LON2", "LAT2")) %>%
  separate("c3", c("LON3", "LAT3")) %>%
  separate("c4", c("LON4", "LAT4"))
df2$Plot <- PRISM_checkOne$Plot[1]


### remove every column except for geometry

xx <- PRISM_checkOne[PRISM_checkOne$dist > 0,] %>%
  as.data.frame() %>%
  select(-geometry)

###this is too hard and a waste of time. just send them the shapefiles

PRISM_checkOne2 <- PRISM_checkOne %>%
  filter(Plot %in% checkOne_plots$Plot) %>%
  select(-PlotX)

GIS_checkOne2 <- GIS_checkOne %>%
  filter(Plot %in% checkOne_plots$Plot)
GIS_checkOne2$dist <- PRISM_checkOne2$dist

    
  
st_write(PRISM_checkOne2, "comparing_plot_coords/Database_check.shp")
st_write(GIS_checkOne2, "comparing_plot_coords/GIS_check.shp")



######################### 


#how many plots are field modified GIS selected

fmgs <- checkOne_data %>%
  select(Plot, Plot_Selection_Method) %>%
  distinct()

table(fmgs$Plot_Selection_Method)


#how many say utm status is gis confirmed?
utms <- checkOne_data %>%
  select(Plot, UTM_1_Status) %>%
  distinct()

table(utms$UTM_1_Status)


length(checkOne_plots$dist[checkOne_plots$dist < 50])
length(checkOne_plots$dist[checkOne_plots$dist > 50])



##for laurent: export all tghe plot's I've checked with the distance appended


compare_locations <- data.frame(Plot = PRISM_check1$Plot, dist = PRISM_check1$dist)
write.csv(compare_locations, "Robjects/compare_locations.csv")
