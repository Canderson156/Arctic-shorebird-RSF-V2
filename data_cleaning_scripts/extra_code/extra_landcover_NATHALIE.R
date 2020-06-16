
all_polygons <- readRDS("Robjects/all_polygons.RDS")


LC3 <- raster("E://Christine_temp//Arctic shorebird RSF//Version 2 - Manuscript//data//landcover//Northern_Land_Cover_2000//landcover_mosaic_lcc.tif")


nat_raw <- read.csv("exported/Nat_Plots2.csv")


nat <- all_polygons %>%
  filter(Plot %in% nat_raw$Standardized_Plot_Name) %>%
  st_transform(crs(LC3))
  
st_write(nat, "exported/nat_plots.shp")



plot_lc2 <- raster::extract(LC3, nat, df = T, weights = T, normalizeWeights = T) #I wonder if the normalize weights is what is causing very high or low proportions



#re-read how extract works and re-think how to aggregate this to get what i want

prop_lc <- plot_lc2 %>%
  group_by(ID, landcover_mosaic_lcc) %>%
  dplyr::summarize(prop = sum(weight))


names <- data.frame(plot = nat$Plot, ID = 1:length(nat$Plot))

prop_lc <- merge(prop_lc, names)

prop_lc <- prop_lc %>%
  ungroup %>%
  dplyr::select(-ID) %>%
  dplyr::rename(lc_code = landcover_mosaic_lcc)

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
                                  "water"))



prop_lc <- merge(prop_lc, landcover_classes)



# export


write.csv(prop_lc, "exported/nat_landcover_proportions2.csv")
