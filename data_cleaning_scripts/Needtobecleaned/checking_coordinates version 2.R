

lcpath <- "E://Christine_temp//Arctic shorebird RSF//Version 2 - Manuscript//data//landcover//Northern_Land_Cover_2000//tifs"



#Create a list of the file names within the working directory (characters)
file_list <- list.files(lcpath, full.names = TRUE)
file_list <- file_list[file_list %notin% "E://Christine_temp//Arctic shorebird RSF//Version 2 - Manuscript//data//landcover//Northern_Land_Cover_2000//tifs/Thumbs.db"]



mosaic_rasters(file_list, "landcover_mosaic_lcc.tif")


gdalwarp("landcover_mosaic_lcc.tif", dstfile = "landcover_mosaic_nplaea.tif", t_srs=NPLAEA,
         output_Raster=TRUE,
         overwrite=TRUE,verbose=TRUE)


LC3 <- raster("E://Christine_temp//Arctic shorebird RSF//Version 2 - Manuscript//data//landcover//Northern_Land_Cover_2000//tifs//landcover_mosaic_nplaea.tif")

#Set the working directory back to what it was
setwd("J:/Christine_temp")

#plot_lc <- raster::extract(LC3, GIS_shapefiles) this one gives the values of every cell within the plot, but can't tell if they are fully in or not

plot_lc2 <- raster::extract(LC3, GIS_shapefiles_tempfix, df = T, weights = T, normalizeWeights = T) #I wonder if the normalize weights is what is causing very high or low proportions

#plot_lc3 <- raster::extract(LC3, GIS_shapefiles, df = T, weights = T, normalizeWeights = F) 

