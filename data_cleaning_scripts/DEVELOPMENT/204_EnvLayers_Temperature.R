
#### Preparing temperature data for analysis


bb_wgs <- readRDS("Robjects/bb_wgs.RDS")



#June temperature

june_temp <- raster("data/climate/WorldClim/wc2.1_30s_tavg_06.tif")
june_temp <- crop(june_temp, bb_wgs)
writeRaster(june_temp, "data/climate/WorldClim/june_temp_cropped.tif", format="GTiff", overwrite = TRUE)

june_temp <- projectRaster(june_temp, crs = LCC)
writeRaster(june_temp, "data/climate/WorldClim/june_temp_LCC.tif", format="GTiff", overwrite = TRUE)





#July temperature

july_temp <- raster("data/climate/WorldClim/wc2.1_30s_tavg_07.tif")
july_temp <- crop(july_temp, bb_wgs)
writeRaster(july_temp, "data/climate/WorldClim/july_temp_cropped.tif", format="GTiff", overwrite = TRUE)

july_temp <- projectRaster(july_temp, crs = LCC)
writeRaster(july_temp, "data/climate/WorldClim/july_temp_LCC.tif", format="GTiff", overwrite = TRUE)


