
#### Preparing landcover data for analysis



#bugs in spatial packages were making this code difficult to use (11 June 2020). 
#I don't recall having any issues the first time I did this (March? Easlier?)
#https://github.com/r-spatial/discuss/issues/31
#They suggested a solution here, but also may be fixing the problem

#file_list <- list.files("data/landcover/Northern_Land_Cover_2000/tifs/")
#file_list <- file_list[file_list %notin% "Thumbs.db"]
#file_list <- paste("data/landcover.,/Northern_Land_Cover_2000/tifs/", file_list, sep = "")

# This isn't working on PC, but is on MAC. But still get error messages
#mosaic_rasters(file_list, "data/landcover/Northern_Land_Cover_2000/landcover_mosaic_lcc.tif.tif")


#gdalwarp("data/landcover/Northern_Land_Cover_2000/landcover_mosaic_lcc.tif", 
#         dstfile = "data/landcover/Northern_Land_Cover_2000/landcover_mosaic_nad83.tif", 
#         t_srs=NAD83,
#         output_Raster=TRUE,
#         overwrite=TRUE,verbose=TRUE)

#lcNAD83 <- raster("data/landcover/Northern_Land_Cover_2000/landcover_mosaic_nad83.tif")
#I made this but I don't think it's what I want to use for my analyses



landcover <- raster("data/landcover/Northern_Land_Cover_2000/landcover_mosaic_lcc.tif")





#######creating bounding box for clipping other rasters

extent_lcc <- extent(landcover)

bb_lcc <- bb_poly(landcover, steps = 1000)
bb_wgs <- st_transform(bb_lcc, WGS84)
bb_wgs <- as_Spatial(bb_wgs)

saveRDS(bb_wgs, "Robjects/bb_wgs.RDS")














































