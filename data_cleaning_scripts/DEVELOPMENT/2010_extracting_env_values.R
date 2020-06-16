




june_temp <- raster("data/climate/WorldClim/june_temp_LCC.tif")
july_temp <- raster("data/climate/WorldClim/july_temp_LCC.tif")
landcover <- raster("data/landcover/Northern_Land_Cover_2000/landcover_mosaic_lcc.tif")

all_polygons <- readRDS("Robjects/all_polygons.RDS")
included_surveys <- readRDS("Robjects/included_surveys.RDS")





#extract for each one seperately because they have different resolutions

#extract temperatures
all_polygons <- st_transform(all_polygons, LCC)
all_polygons <- as_Spatial(all_polygons)


all_polygons <- raster::extract(june_temp, all_polygons, fun = mean, sp = TRUE)
all_polygons <- raster::extract(july_temp, all_polygons, fun = mean, sp = TRUE)



#extract landcover
plot_lc2 <- raster::extract(landcover, all_polygons, df = T, weights = T, normalizeWeights = T) 



prop_lc <- plot_lc2 %>%
  group_by(ID, landcover_mosaic_lcc) %>%
  dplyr::summarize(prop = sum(weight))


names <- data.frame(plot = all_polygons$Plot, ID = 1:length(all_polygons$Plot))

prop_lc <- merge(prop_lc, names)

prop_lc <- prop_lc %>%
  ungroup %>%
  dplyr::select(-ID) %>%
  dplyr::rename(lc_code = landcover_mosaic_lcc)

landcover_classes <- data.frame(lc_code = 1:15,
                                lc_class = c(
                                  "1_tussock graminoid tundra",
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
                                  "water"))



prop_lc <- merge(prop_lc, landcover_classes)




#### adding to a version of allpolygons df

all_polygons_df <- all_polygons@data

#class 1

tussock_graminoid_1 <- prop_lc %>%
  filter(lc_class == "1_tussock graminoid tundra ") %>%
  select(plot, prop) %>%
  rename(Plot = plot, tussock_graminoid_1 = prop)

all_polygons_df <- merge(all_polygons_df, tussock_graminoid_1, all = TRUE) %>%
  mutate(tussock_graminoid_1 = ifelse(is.na(tussock_graminoid_1),0,tussock_graminoid_1))



#class 2

wet_sedge_2 <- prop_lc %>%
  filter(lc_class == "2_wet sedge") %>%
  select(plot, prop) %>%
  rename(Plot = plot, wet_sedge_2 = prop)

all_polygons_df <- merge(all_polygons_df, wet_sedge_2, all = TRUE) %>%
  mutate(wet_sedge_2 = ifelse(is.na(wet_sedge_2),0,wet_sedge_2))


#class 3

nontussock_graminoid_3 <- prop_lc %>%
  filter(lc_class == "3_moist to dry non-tussock graminoid / dwarf shrub tundra") %>%
  select(plot, prop) %>%
  rename(Plot = plot, nontussock_graminoid_3 = prop)

all_polygons_df <- merge(all_polygons_df, nontussock_graminoid_3, all = TRUE) %>%
  mutate(nontussock_graminoid_3 = ifelse(is.na(nontussock_graminoid_3),0,nontussock_graminoid_3))


#class 4

prostrate_shrub_4 <- prop_lc %>%
  filter(lc_class == "4_dry graminoid prostrate dwarf shrub tundra") %>%
  select(plot, prop) %>%
  rename(Plot = plot, prostrate_shrub_4 = prop)

all_polygons_df <- merge(all_polygons_df, prostrate_shrub_4, all = TRUE) %>%
  mutate(prostrate_shrub_4 = ifelse(is.na(prostrate_shrub_4),0,prostrate_shrub_4))


#class 11


wetlands_11 <- prop_lc %>%
  filter(lc_class == "11_wetlands") %>%
  select(plot, prop) %>%
  rename(Plot = plot, wetlands_11 = prop)

all_polygons_df <- merge(all_polygons_df, wetlands_11, all = TRUE) %>%
  mutate(wetlands_11 = ifelse(is.na(wetlands_11),0,wetlands_11))




#graminoid group

graminoid_group <- prop_lc %>%
  filter(lc_group == "graminoid") %>%
  select(plot, prop) %>%
  group_by(plot) %>%
  summarize(mean = mean(prop)) %>%
  rename(Plot = plot, graminoid_group = mean)

all_polygons_df <- merge(all_polygons_df, graminoid_group, all = TRUE) %>%
  mutate(graminoid_group = ifelse(is.na(graminoid_group),0,graminoid_group))




#merge with SB

all_data <- merge(SB3, all_polygons_df, all.x = TRUE)
