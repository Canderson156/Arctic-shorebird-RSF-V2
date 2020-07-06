
#### Preparing temperature data for analysis


bb_wgs <- readRDS("Robjects/bb_wgs.RDS")



### 30 year climate averages



#June min temperature

temp_30_min_june <- raster("data/climate/WorldClim/raw_30_year_avg/wc2.1_30s_tmin_06.tif")
temp_30_min_june <- crop(temp_30_min_june, bb_wgs)
temp_30_min_june <- projectRaster(temp_30_min_june, crs = LCC)
writeRaster(temp_30_min_june, "data/climate/WorldClim/temp_30_min_june_LCC.tif", format="GTiff", overwrite = TRUE)




#June avg temperature

temp_30_avg_june <- raster("data/climate/WorldClim/raw_30_year_avg/wc2.1_30s_tavg_06.tif")
temp_30_avg_june <- crop(temp_30_avg_june, bb_wgs)
temp_30_avg_june <- projectRaster(temp_30_avg_june, crs = LCC)
writeRaster(temp_30_avg_june, "data/climate/WorldClim/temp_30_avg_june_LCC.tif", format="GTiff", overwrite = TRUE)


#July avg temperature

temp_30_avg_july <- raster("data/climate/WorldClim/raw_30_year_avg/wc2.1_30s_tavg_07.tif")
temp_30_avg_july <- crop(temp_30_avg_july, bb_wgs)
temp_30_avg_july <- projectRaster(temp_30_avg_july, crs = LCC)
writeRaster(temp_30_avg_july, "data/climate/WorldClim/temp_30_avg_july_LCC.tif", format="GTiff", overwrite = TRUE)




## single years

filelist <- paste("data/climate/WorldClim/raw_1_year/raw/", 
                     list.files("data/climate/WorldClim/raw_1_year/raw"), sep = "")


temp_list <- lapply(filelist, raster)
temp_list <- lapply(temp_list, crop, bb_wgs)
temp_list <- lapply(temp_list, projectRaster, crs = LCC)

filenames <- list.files("data/climate/WorldClim/raw_1_year/raw")
filenames <- str_remove_all(filenames, "wc2.1_2.5m_tmin_")
filenames <- str_remove_all(filenames, ".tif")
filenames <- paste("data/climate/WorldClim/raw_1_year/LCC/", filenames, sep = "")

names(temp_list) <- filenames 

writeRaster(stack(temp_list), names(temp_list), bylayer=TRUE, format='GTiff')

