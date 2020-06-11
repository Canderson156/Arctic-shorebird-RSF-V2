#import the northern land cover map

#Create a temporary working directory to make things easier
#PC
lcpath <- "E://Christine_temp//Arctic shorebird RSF//Version 2 - Manuscript//data//landcover//Northern_Land_Cover_2000//tifs"

#MAC
lcpath <- "//Volumes//Seagate//Christine_temp//Arctic shorebird RSF//Version 2 - Manuscript//data//landcover//Northern_Land_Cover_2000//tifs"


#Create a list of the file names within the working directory (characters)
lc_file_list <- list.files(lcpath, full.names = TRUE)

#Create an empty list
LClist<-list()
 
#Fill the empty list with the actual rasters
for (i in lc_file_list){
  LClist<-append(LClist, raster(i))
}

#Merge rasters
LClist$tolerance <- 1
LClist$filename <- paste(path.mrg, "/AST14DMO_sd_", z, "m_mrg.tif", sep = "")
LClist$overwrite <- TRUE
landcover <- do.call(merge, LClist)
plot(landcover)
 
# #Create another temporary working directory
# setwd("J:/Christine_temp/Data/Land Cover/Northern_Land_Cover_2000/output tifs")

lc_file_list2 <- paste("v2", lc_file_list, sep = ".")

#Project rasters before merging
LClist2<-lapply(lc_file_list, gdalwarp,
                dstfile = lc_file_list2,
                t_srs=NPLAEA,
                output_Raster=TRUE,
                overwrite=TRUE,verbose=TRUE)

LClist3<-mapply(gdalwarp, srcfile = lc_file_list,
                dstfile = lc_file_list2,
                t_srs=NPLAEA,
                output_Raster=TRUE,
                overwrite=TRUE,verbose=TRUE)

#Merge the new rasters
LClist2$tolerance <- 1
landcover2.1 <- do.call(merge, LClist2)

############# actually just try importing what's already in the folder
#MAC
LC1 <- raster("//Volumes//Seagate//Christine_temp//Arctic shorebird RSF//Version 2 - Manuscript//data//landcover//Northern_Land_Cover_2000//completeLC.tif")
#this one is enormous and doesn't have any values associated with it

LC2 <- raster("//Volumes//Seagate//Christine_temp//Arctic shorebird RSF//Version 2 - Manuscript//data//landcover//Northern_Land_Cover_2000//completeLC2.tif")


LC1 <- raster("E://Christine_temp//Arctic shorebird RSF//Version 2 - Manuscript//data//landcover//Northern_Land_Cover_2000//completeLC.tif")
#this one is enormous and doesn't have any values associated with it


LCC <- crs(LC1)

test <- raster::extract(LC1, GIS_shapefiles)


#### breaking this down to just the individual raster to see if that works

test <- raster(lc_file_list[99])

gdalwap(lc_file_list[99], paste(lc_file_list[99], "v2"))

LClist2<-lapply(lc_file_list, gdalwarp,
                dstfile = lc_file_list2,
                t_srs=NPLAEA,
                output_Raster=TRUE,
                overwrite=TRUE,verbose=TRUE)




##############

#Trying Laurent's script again


#Create a temporary working directory to make things easier
setwd("J:/Christine_temp/Data/Land Cover/Northern_Land_Cover_2000/tifs")

#MAC
setwd("//Volumes//Seagate//Christine_temp//Arctic shorebird RSF//Version 2 - Manuscript//data//landcover//Northern_Land_Cover_2000//tifs")


#Create a list of the file names within the working directory (characters)
file_list <- list.files()
file_list <- file_list[file_list %notin% "Thumbs.db"]
#would be nice to prevent it brom making this file in the first place

#Create an empty list
LClist<-list()

#Fill the empty list with the actual rasters
for (i in file_list){
  LClist<-append(LClist, raster(i))
}

#Merge rasters
LClist$tolerance <- 1
LClist$filename <- paste(path.mrg, "/AST14DMO_sd_", z, "m_mrg.tif", sep = "")
LClist$overwrite <- TRUE
landcover <- do.call(merge, LClist)
plot(landcover)
# 
# #Create another temporary working directory
# setwd("J:/Christine_temp/Data/Land Cover/Northern_Land_Cover_2000/output tifs")

file_list2 <- paste("v2", file_list, sep = ".")

#Project rasters before merging
lapply(file_list[1:10], gdalwarp,
                dstfile = file_list2[1:10],
                t_srs=NPLAEA,
                output_Raster=TRUE,
                overwrite=TRUE,verbose=TRUE)

LClist2<-mapply(gdalwarp, srcfile = file_list,
                dstfile = file_list2,
                t_srs=NPLAEA,
                output_Raster=TRUE,
                overwrite=TRUE,verbose=TRUE)

test <- raster(file_list2[1])

gdalwarp(file_list[1], dstfile = file_list2[1], t_srs=NPLAEA,
         output_Raster=TRUE,
         overwrite=TRUE,verbose=TRUE)

#try doing this as a loop instead of as lapply

for(i in 1:length(file_list)) {
  gdalwarp(file_list[i], 
           dstfile = file_list2[i], 
           t_srs=NPLAEA,
           output_Raster=TRUE,
           overwrite=TRUE,
           verbose=TRUE)
}


#Create an empty list
LClist<-list()

#Fill the empty list with the actual rasters
for (i in 1:length(file_list2)){
  LClist<-append(LClist, raster(file_list2[i]))
}


LClist$tolerance <- 1
LClist$filename <- 'test.tif'
LClist$overwrite <- TRUE


landcover <- do.call(merge, LClist)
plot(landcover)


#try to mosaic

mosaic_rasters(file_list2, "landcover_mosaic.tif")
test <- raster("landcover_mosaic.tif")

#this version works' but has many gaps in it
####try to do it the reverse way: mosaic, the reproject



mosaic_rasters(file_list, "landcover_mosaic_lcc.tif")


gdalwarp("landcover_mosaic_lcc.tif", dstfile = "landcover_mosaic_nplaea.tif", t_srs=NPLAEA,
         output_Raster=TRUE,
         overwrite=TRUE,verbose=TRUE)


LC3 <- raster("E://Christine_temp//Arctic shorebird RSF//Version 2 - Manuscript//data//landcover//Northern_Land_Cover_2000//landcover_mosaic_lcc.tif")

#Set the working directory back to what it was
setwd("J:/Christine_temp")

#plot_lc <- raster::extract(LC3, GIS_shapefiles) this one gives the values of every cell within the plot, but can't tell if they are fully in or not

plot_lc2 <- raster::extract(LC3, GIS_shapefiles_tempfix, df = T, weights = T, normalizeWeights = T) #I wonder if the normalize weights is what is causing very high or low proportions

#plot_lc3 <- raster::extract(LC3, GIS_shapefiles, df = T, weights = T, normalizeWeights = F) 




########### remove the plots that appear more than once

GIS_shapefiles_tempfix <- GIS_shapefiles[!duplicated(GIS_shapefiles$FN),]
GIS_shapefiles_tempfix <- GIS_shapefiles_tempfix[GIS_shapefiles_tempfix$FN %notin% as.character(1:1000),]




#re-read how extract works and re-think how to aggregate this to get what i want

prop_lc <- plot_lc2 %>%
  group_by(ID, landcover_mosaic_nplaea) %>%
  dplyr::summarize(prop = sum(weight))


names <- data.frame(plot = GIS_shapefiles_tempfix$FN, ID = 1:length(GIS_shapefiles_tempfix$FN))

prop_lc <- merge(prop_lc, names)

prop_lc <- prop_lc %>%
  dplyr::select(-ID) %>%
  dplyr::rename(lc_code = landcover_mosaic_nplaea)

landcover_classes <- data.frame(lc_code = 1:15,
                                lc_class = c(
                                  "1_tussock graminoid tundra ",
                                  "2_wet sedge",
                                  "3_moist to dry non-tussock graminoid / dwarf shrub tundra",
                                  "4_dry graminoid prostrate dwarf shrub tundra",
                                  "5_low shrub",
                                  "6_tall shrub",
                                  "7_prostrate dwarf shrub",
                                  "8_sparsely vegetated bedrock",
                                  "9_sparsely vegetated till-colluvium",
                                  "10_bare soil with cryptogam crust - frost boils",
                                  "11_wetlands",
                                  "12_barren",
                                  "13_ice / snow",
                                  "14_shadow",
                                  "15_water"),
                                lc_group = c(
                                  "graminoid",
                                  "graminoid",
                                  "graminoid",
                                  "graminoid",
                                  "shrub",
                                  "shrub",
                                  "shrub",
                                  "sparsely vegetated",
                                  "sparsely vegetated",
                                  "sparsely vegetated",
                                  "wetland",
                                  "barren",
                                  "ice / snow",
                                  "shadow",
                                  "water")
                                )


prop_lc <- merge(prop_lc, landcover_classes)





###exploratory data analysis


#do the proportion really add up to 1?

plot_sums <- prop_lc %>%
  group_by(plot) %>%
  dplyr::summarize(total_sum = sum(prop))


plot_sums_weird <- plot_sums %>%
  filter(total_sum < 0.95)


plot_sums_good  <- plot_sums %>%
  filter(total_sum > 0.95) 


prop_lc_good <- prop_lc %>%
  filter(plot %in% plot_sums_good$plot)


###there are 35 plots that have less than 95% of the plot covered - I don't know what that means


hist(test3$total_sum[test3$total_sum < 0.95], breaks = 100)
hist(test3$total_sum[test3$total_sum > 1.05], breaks = 100)




data.frame(sort(table(highest$lc_class)))
data.frame(sort(table(highest$lc_group)))






#Things that need to be fixed:
#plots with just a number need an actuaL plot name
#need to figure out why some plots are there more than once and deal wioth them properly instead of just picking one
#qaulity control to match Isabel's data with tylers data








#what is the highest proportion in each plot? how high is that?

test2 <- prop_lc[prop_lc$plot == "MDW-0080",]

highest <- prop_lc_good %>%
  group_by(plot) %>%
  dplyr::summarize(prop = max(prop))

highest <- merge(highest, prop_lc)


SB2 <- SB %>%
  dplyr::select(Plot, sum_shorebirds, mean_geese) %>%
  dplyr::rename(plot = Plot)

highest <- merge(highest, SB2)



m1 <- lm(sum_shorebirds ~ lc_group, data = highest)
m2 <- lm(sum_shorebirds ~ lc_group*prop, data = highest) #could i do only the interaction effect?


#how does the proportion of certain habitat types relate to shorebird abundance?






#can I do some kind of multivariate analysis? like what I did in my masters paper? Maybe save this one for later





