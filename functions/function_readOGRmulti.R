# for loading shapefiles from geodatabase. 

#Keeps the dsn(filepath) constant, and loads multiple layers from that geodatabase



readOGR_multi <- function(DSN, LAYER_VEC){
  shapefiles <- lapply(LAYER_VEC, readOGR, dsn = DSN)
  return(shapefiles)
}