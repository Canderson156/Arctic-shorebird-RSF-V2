
#extract environmental variables for surveyed plots

### some inconsitiencies in this code:
#why do the avg temps end up with 8 plots with "NaN", but the annual ones don't?
#Why does the finaly version of all_polygons_df have so many duplicated rows, which I got rid of using distinct()?
    #I had figured this out and now I don't recall the answer



### Split this into 2 different scripts



filelist1 <- list.files("data/climate/WorldClim/raw_1_year/LCC/June/", full.names = TRUE)
temp_rasters <- stack(filelist1)

temp_30_min_june <- raster("data/climate/WorldClim/temp_30_min_june.tif")
temp_30_min_july <- raster("data/climate/WorldClim/temp_30_min_july.tif")
landcover <- raster("data/landcover/Northern_Land_Cover_2000/landcover_mosaic_lcc.tif")

all_polygons_raw <- readRDS("Robjects/all_polygons.RDS")
all_polygons <- all_polygons_raw
included_surveys <- readRDS("Robjects/included_surveys.RDS")
prism <- readRDS("Robjects/prism.RDS")



###TEMPERATURE


#add year so that plots can be matched with annual temp
years <- prism %>%
  select(Plot, Year) %>%
  filter(Plot %in% included_surveys$Plot) %>%
  filter(Plot %in% all_polygons$Plot) %>%
  distinct()

all_polygons <- merge(all_polygons, years, all.y = TRUE)


all_polygons <- st_transform(all_polygons, LCC)
#all_polygons <- as_Spatial(all_polygons)



  
###extract annual temperature




#split the dataset up by year

all_polygons_year <- all_polygons %>%
  filter(Year != 2019) %>%
  group_split(Year)


year_names <- paste("temp", head(sort(unique(all_polygons$Year)), -1), sep = "") 
names(all_polygons_year) <- year_names


# extract annual temperature for each group
names(temp_rasters) <-year_names

for(i in 1:length(all_polygons_year)){
  yr <- names(all_polygons_year[i])
  ras <- subset(temp_rasters, yr)
  all_polygons_year[[i]] <- raster::extract(ras, all_polygons_year[[i]], na.rm = TRUE, fun = mean, sp = TRUE)
}
#ignore error messages, should be fine -  https://gis.stackexchange.com/questions/364667/r-4-0-1-not-sure-i-understand-this-message-warning-message-in-proj4stringx

#save incase I make a mistake
#all_polygons_year_tmp <- all_polygons_year
#all_polygons_year <- all_polygons_year_tmp

all_polygons_year <- lapply(all_polygons_year, st_as_sf)


#rename temp columns so that I can merge them together
all_polygons_year <- lapply(all_polygons_year, setNames, nm = c("Plot", "Year", "temp_yr_min_june", "geometry"))

#merge together
all_polygons2 <- do.call("rbind", all_polygons_year) 








###extract 30 year temperatures
all_polygons2 <- raster::extract(temp_30_min_june, all_polygons2, na.rm = TRUE, fun = mean, sp = TRUE)
all_polygons2 <- raster::extract(temp_30_min_july, all_polygons2, na.rm = TRUE, fun = mean, sp = TRUE)



#just a dataframe
all_polygons_df <- all_polygons2@data





#fix the missing temperatures


all_polygons_df$temp_yr_min_june[all_polygons_df$Plot == "KWI-1314B"] <- -0.8

all_polygons_df$temp_30_min_june[all_polygons_df$Plot == "IGL-0175D"] <- -1.6
all_polygons_df$temp_30_min_june[all_polygons_df$Plot == "MDE-0035A"] <- -0.3
all_polygons_df$temp_30_min_june[all_polygons_df$Plot == "RAS-0105"] <- -1.7
all_polygons_df$temp_30_min_june[all_polygons_df$Plot == "RAS-0109"] <- -1.7
all_polygons_df$temp_30_min_june[all_polygons_df$Plot == "SIR-0036A"] <- -2.5
all_polygons_df$temp_30_min_june[all_polygons_df$Plot == "SIR-0036C"] <- -2.5
all_polygons_df$temp_30_min_june[all_polygons_df$Plot == "SIR-0038A"] <- -2.9
all_polygons_df$temp_30_min_june[all_polygons_df$Plot == "SIR-0038D"] <- -2.8
all_polygons_df$temp_30_min_june[all_polygons_df$Plot == "MDW-0072B"] <- 3.8
all_polygons_df$temp_30_min_june[all_polygons_df$Plot == "AFI-0024"] <- -0.9

all_polygons_df$temp_30_min_july[all_polygons_df$Plot == "IGL-0175D"] <- 3.8
all_polygons_df$temp_30_min_july[all_polygons_df$Plot == "MDE-0035A"] <- 4.7
all_polygons_df$temp_30_min_july[all_polygons_df$Plot == "RAS-0105"] <- 3.2
all_polygons_df$temp_30_min_july[all_polygons_df$Plot == "RAS-0109"] <- 3.2
all_polygons_df$temp_30_min_july[all_polygons_df$Plot == "SIR-0036A"] <- 1.7
all_polygons_df$temp_30_min_july[all_polygons_df$Plot == "SIR-0036C"] <- 1.8
all_polygons_df$temp_30_min_july[all_polygons_df$Plot == "SIR-0038A"] <- 1.8
all_polygons_df$temp_30_min_july[all_polygons_df$Plot == "SIR-0038D"] <- 1.2
all_polygons_df$temp_30_min_july[all_polygons_df$Plot == "MDW-0072B"] <- 8.3
all_polygons_df$temp_30_min_july[all_polygons_df$Plot == "AFI-0024"] <- 3.5











#####LANDCOVER







#extract landcover
#plot_lc2 <- raster::extract(landcover, all_polygons, df = T, weights = T, normalizeWeights = T) 
#saveRDS(plot_lc2, "Robjects/plot_lc2.RDS")
plot_lc2 <- readRDS("Robjects/plot_lc2.RDS")


prop_lc <- plot_lc2 %>%
  group_by(ID, landcover_mosaic_lcc) %>%
  dplyr::summarize(prop = sum(weight))


names <- data.frame(Plot = all_polygons_raw$Plot, ID = 1:length(all_polygons_raw$Plot))

n_unique(prop_lc$ID) == n_unique(names$Plot)


prop_lc <- merge(prop_lc, names)


prop_lc <- prop_lc %>%
  ungroup %>%
  dplyr::select(-ID) %>%
  dplyr::rename(lc_code = landcover_mosaic_lcc)

landcover_classes <- data.frame(lc_code = 0:15,
                                lc_class = c(
                                  "00_missing_data",
                                  "01_tussock_graminoid",
                                  "02_wet_sedge",
                                  "03_nontussock_graminoid_dwarf_shrub",
                                  "04_dry_graminoid_prostrate_dwarf_shrub_tundra",
                                  "05_low_shrub",
                                  "06_tall_shrub",
                                  "07_prostrate_dwarf_shrub",
                                  "08_sparsely_vegetated_bedrock",
                                  "09_sparsely_vegetated_till",
                                  "10_bare_soil_cryptogam_frost_boils",
                                  "11_wetlands",
                                  "12_barren",
                                  "13_ice_snow",
                                  "14_shadow",
                                  "15_water"),
                                lc_group = c(
                                  "missing data",
                                  "graminoid",
                                  "graminoid",
                                  "graminoid",
                                  "graminoid",
                                  "shrub",
                                  "shrub",
                                  "shrub",
                                  "sparsely_vegetated",
                                  "sparsely_vegetated",
                                  "sparsely_vegetated",
                                  "wetland",
                                  "barren",
                                  "ice_snow",
                                  "shadow",
                                  "water"))



prop_lc <- merge(prop_lc, landcover_classes) %>%
  distinct()

prop_lc$Plot <- as.character(prop_lc$Plot)




#wide version

prop_lc_wide <- prop_lc %>%
  select(-lc_group, -lc_code) %>%
  spread(lc_class, prop, fill = 0)


#summarize group

group <- prop_lc %>%
  group_by(Plot, lc_group) %>%
  summarize(sum_group = sum(prop)) %>%
  mutate(lc_group = paste("group", lc_group, sep = "_")) %>%
  spread(lc_group, sum_group, fill = 0)



all_polygons_df <- merge(all_polygons_df, prop_lc_wide, by = "Plot")
all_polygons_df <- merge(all_polygons_df, group, by = "Plot")














#saveRDS
saveRDS(all_polygons_df, "Robjects/all_polygons_df.RDS")






#for editing temps

#writeOGR(all_polygons, "all_polygons.shp", "all_polygons", driver="ESRI Shapefile")
#writeRaster(temp_30_min_june, "data/climate/WorldClim/temp_30_min_june_LCC.tif")
#writeRaster(temp_30_min_july, "data/climate/WorldClim/temp_30_min_july_LCC.tif")
#write_csv(all_polygons_df, "exported/all_polygons_df_june26.csv")
#write_csv(fix_missing_temps, "data/climate/WorldClim/fix_missing_temps.csv")




#background values for predictor variables

bckgrd_temp_30_min_june <- sampleRandom(temp_30_min_june, 10000)

bckgrd_temp_30_min_july <- sampleRandom(temp_30_min_june, 10000)










