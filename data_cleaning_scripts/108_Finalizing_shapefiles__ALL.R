

#### Merge and export finalized coordinates



g0_no_coords <- readRDS("Robjects/g0_no_coords.RDS")
good_g1 <- readRDS("Robjects/good_g1.RDS")
good_g2 <- readRDS("Robjects/good_g2.RDS")
good_g2a <- readRDS("Robjects/good_g2a.RDS")
good_g2b <- readRDS("Robjects/goood_g2b.RDS")
good_g3_gis <- readRDS("Robjects/good_g3_gis.RDS")
good_g3a_alert_dups  <- readRDS("Robjects/good_g3a_alert_dups.RDS")
good_g3a_alert_multi  <- readRDS("Robjects/good_g3a_alert_multi.RDS")
good_g3a_alert_ch  <- readRDS("Robjects/good_g3a_alert_ch.RDS")



#Group polygons together for exporting

all_polygons <- rbind(good_g1, good_g2, good_g3_gis)
all_polygons <- st_cast(all_polygons, "MULTIPOLYGON")
all_polygons <- rbind(all_polygons, good_g3a_alert_multi)


#Group points and no data together for exporting

suppressWarnings(good_g0 <- st_sf(Plot = g0_no_coords$Plot,
                 geometry = st_sfc(st_point()),
                 crs = NAD83))
all_points <- rbind(good_g0, good_g2a, good_g2b) 



#group all together to save as RDS
all_coords <-  rbind(all_points, all_polygons)



#exporting
#change the dates if any substantive chnages have been made

st_write(all_polygons, "exported/all_coords/all_polygons_NAD83_2020_06_24.shp", append=FALSE)
st_write(all_points, "exported/all_coords/all_points_NAD83_2020_06_24.shp", append=FALSE)
saveRDS(all_coords, "Robjects/all_coords.RDS")
saveRDS(all_points, "Robjects/all_points.RDS")
saveRDS(all_polygons, "Robjects/all_polygons.RDS")



