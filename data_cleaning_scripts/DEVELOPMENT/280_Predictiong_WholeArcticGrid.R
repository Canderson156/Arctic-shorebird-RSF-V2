

library(MASS)
library(tidyr)
library(rgeos)
library(maptools)
library(rgdal)
library(reshape)
library(raster)
library(SDMTools)
library(velox)
library(gdalUtils)
library(plyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(scales)




LC <- raster("E://Christine_temp//Arctic shorebird RSF//Version 2 - Manuscript//data//landcover//Northern_Land_Cover_2000//landcover_mosaic_lcc.tif")
b.box <- bbox(LC)




x <- seq(from = b.box[1,1], to = b.box[1,2], by = 300) 
y <- seq(from = b.box[2,1], to = b.box[2,2], by = 400)

xy <- expand.grid(x = x, y = y)



grid.pts<-SpatialPointsDataFrame(coords= xy, data=xy, proj4string = crs(LC))
gridded(grid.pts) <- TRUE
gridg <- as(grid.pts, "SpatialGrid") 

gridgspdf <- SpatialGridDataFrame(gridg, data=data.frame(id=row.names(gridg), row.names=row.names(gridg))) 
gridr <- raster(gridgspdf)

#now I just need to trim it to be the right size