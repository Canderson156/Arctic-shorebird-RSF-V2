
#extract environmental variables for surveyed plots

### some inconsitiencies in this code:
#why do the avg temps end up with 8 plots with "NaN", but the annual ones don't?
#Why does the finaly version of all_polygons_df have so many duplicated rows, which I got rid of using distinct()?
    #I had figured this out and now I don't recall the answer


filelist1 <- list.files("data/climate/WorldClim/raw_1_year/LCC/June/", full.names = TRUE)
temp_rasters <- stack(filelist1)

temp_30_avg_june <- raster("data/climate/WorldClim/temp_30_avg_june_LCC.tif")
temp_30_avg_july <- raster("data/climate/WorldClim/temp_30_avg_july_LCC.tif")
landcover <- raster("data/landcover/Northern_Land_Cover_2000/landcover_mosaic_lcc.tif")

all_polygons <- readRDS("Robjects/all_polygons.RDS")
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
all_polygons_year <- do.call("rbind", all_polygons_year) 

### need to manually find the KWI data, as I did for the previous plots





all_polygons <- all_polygons_year
#merge with the version where missing values have been added manually
#maybe chenage them to be added programmatically? 
# Do I need to add anything to this version? apparently not, which is weird to me






###extract 30 year temperatures
all_polygons <- raster::extract(temp_30_avg_june, all_polygons, na.rm = TRUE, fun = mean, sp = TRUE)
all_polygons <- raster::extract(temp_30_avg_july, all_polygons, na.rm = TRUE, fun = mean, sp = TRUE)



#just a dataframe
all_polygons_df <- all_polygons@data


###fix missing data


#import version where missing temp have been manually added
fix_missing_temps <- read.csv("data/climate/WorldClim/fix_missing_temps.csv")


all_polygons_df2 <- all_polygons_df %>%
  filter(is.na(temp_30_avg_june)) %>%
  select(-temp_30_avg_june, -temp_30_avg_july) %>%
  merge(fix_missing_temps)


all_polygons_df <- all_polygons_df %>%
  filter(not.na(temp_30_avg_june)) %>%
  merge(all_polygons_df2, all = TRUE)



#fix the missing annual temperature


all_polygons_df$temp_yr_min_june[all_polygons_df$Plot == "KWI-1314B"] <- -0.8











#####LANDCOVER







#extract landcover
plot_lc2 <- raster::extract(landcover, all_polygons, df = T, weights = T, normalizeWeights = T) 
saveRDS(plot_lc2, "Robjects/plot_lc2.RDS")
plot_lc2 <- readRDS("Robjects/plot_lc2.RDS")


prop_lc <- plot_lc2 %>%
  group_by(ID, landcover_mosaic_lcc) %>%
  dplyr::summarize(prop = sum(weight))


names <- data.frame(Plot = all_polygons$Plot, ID = 1:length(all_polygons$Plot))

prop_lc <- merge(prop_lc, names)


prop_lc <- prop_lc %>%
  ungroup %>%
  dplyr::select(-ID) %>%
  dplyr::rename(lc_code = landcover_mosaic_lcc)

landcover_classes <- data.frame(lc_code = 0:15,
                                lc_class = c(
                                  "00_missing data",
                                  "01_tussock graminoid tundra",
                                  "02_wet sedge",
                                  "03_moist to dry non-tussock graminoid / dwarf shrub tundra",
                                  "04_dry graminoid prostrate dwarf shrub tundra",
                                  "05_low shrub",
                                  "06_tall shrub",
                                  "07_prostrate dwarf shrub",
                                  "08_sparsely vegetated bedrock",
                                  "09_sparsely vegetated till-colluvium",
                                  "10_bare soil with cryptogam crust - frost boils",
                                  "11_wetlands",
                                  "12_barren",
                                  "13_ice / snow",
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
                                  "sparsely vegetated",
                                  "sparsely vegetated",
                                  "sparsely vegetated",
                                  "wetland",
                                  "barren",
                                  "ice / snow",
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



m <- merge(prop_lc_wide, group)


all_polygons_df <- merge(all_polygons_df, prop_lc_wide, by = "Plot")




#saveRDS
saveRDS(all_polygons_df, "Robjects/all_polygons_df.RDS")






#for editing temps

#writeOGR(all_polygons, "all_polygons.shp", "all_polygons", driver="ESRI Shapefile")
#writeRaster(temp_30_avg_june, "temp_30_avg_june.tif")
#writeRaster(temp_30_avg_july, "temp_30_avg_july.tif")
#write_csv(all_polygons_df, "exported/all_polygons_df_june26.csv")
#write_csv(fix_missing_temps, "data/climate/WorldClim/fix_missing_temps.csv")



