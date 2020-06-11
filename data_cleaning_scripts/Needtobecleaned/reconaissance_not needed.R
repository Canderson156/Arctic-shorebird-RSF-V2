## add in missing data from reconnaissance plots



plots_rec_2007_raw <- read.csv("data/PRISM/2007 Reconnaissance waypoints.csv")



plots_rec_2007 <- plots_rec_2007_raw %>%
  select(Plot = Standardized_Plot_Name, Easting, Northing, Order) %>%
  drop_na() %>%
  unite(coords, Easting, Northing) %>%
  mutate(Order = paste("coord_", Order, sep = "")) %>%
  spread(Order, coords) %>%
  separate(coord_1, c("UTM_1_Easting", "UTM_1_Northing"), sep = "_") %>%
  separate(coord_2, c("UTM_2_Easting", "UTM_2_Northing"), sep = "_") %>%
  separate(coord_3, c("UTM_3_Easting", "UTM_3_Northing"), sep = "_") %>%
  separate(coord_4, c("UTM_4_Easting", "UTM_4_Northing"), sep = "_") %>%
  separate(coord_5, c("UTM_5_Easting", "UTM_5_Northing"), sep = "_") %>%
  separate(coord_6, c("UTM_6_Easting", "UTM_6_Northing"), sep = "_") %>%
  separate(coord_7, c("UTM_7_Easting", "UTM_7_Northing"), sep = "_") %>%
  separate(coord_8, c("UTM_8_Easting", "UTM_8_Northing"), sep = "_") %>%
  separate(coord_9, c("UTM_9_Easting", "UTM_9_Northing"), sep = "_") %>%
  separate(coord_10, c("UTM_10_Easting", "UTM_10_Northing"), sep = "_") %>%
  separate(coord_11, c("UTM_11_Easting", "UTM_11_Northing"), sep = "_") %>%
  separate(coord_12, c("UTM_12_Easting", "UTM_12_Northing"), sep = "_") %>%
  mutate(UTM_1_Type = "point in line", UTM_2_Type = "point in line", UTM_3_Type = "point in line", UTM_4_Type = "point in line", 
         UTM_1_Status = "LAT_LONG_DD")

#I'm not sure if I wan't this for the ones that are NAs
#UTM_5_Type = "point in line", UTM_6_Type = "point in line", UTM_7_Type = "point in line", UTM_8_Type = "point in line",
#UTM_9_Type = "point in line", UTM_10_Type = "point in line", UTM_11_Type = "point in line", UTM_12_Type = "point in line",


rec_cols <- colnames(plots_rec_2007)[colnames(plots_rec_2007) %notin% c("Plot", "UTM_Zone")]




allplots_rec_2007 <- allplots %>%
  filter(Plot %in% plots_rec_2007$Plot) %>%
  select(-one_of(rec_cols))


allplots_sub2 <- allplots %>%
  filter(Plot %notin% plots_rec_2007$Plot)


allplots_rec_2007 <- merge(allplots_rec_2007, plots_rec_2007, by = "Plot")

allplots <- rbind(allplots_sub2, allplots_rec_2007)

rm(allplots_rec_2007, allplots_sub2, plots_rec_2007)
