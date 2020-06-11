##### checking shapefile plot names against historic names




shapefile_proj_list[[4]]@data$FN


historic_plot_names <- bigdata %>%
  select(Standardized_Plot_Name, contains("Historic"), Region_name, Region_code) %>%
  gather(key = "col", value = "FN", contains("Historic")) %>%
  distinct() %>%
  filter(not.na(FN)) %>%
  select(-col, Plot = Standardized_Plot_Name)

#635 plots have the same plot name


# set it up to check by region



test <- shapefile_proj_list[[4]]@data$FN %in% historic_plot_names$FN[historic_plot_names$Region_code == 2]
  
test2 <- merge(shapefile_proj_list[[4]]@data, historic_plot_names[historic_plot_names$Region_code == 2,], by = "FN")  
  