





### which plots did we survey in 2019

pci <- bigdata %>%
  filter(Region_code == 3 & Year %in% c(1996, 1997, 2019)) %>%
  select(Plot = Standardized_Plot_Name, #2855 unique plots
         Year, #1994-2019
         Month, #mostly June, some July, 475 "not applicable"
         Day,
         Date,
         yday,
         Region_name, #89 plots with NAs that are outside the regions (south of Mackenzie Delta)
         Region_code, #89 plots with NAs that are outside the regions (south of Mackenzie Delta)
         Sub_region_name, #170 plots with NAs. 89 as above. 78 in Foxe basin becasue of differences in how subregions were calculated over time (2019 had no subregions). 3 in North Archipelago are confusing, but may stem from weird subregions that overlap near Alert (see email from Laurent RE Tyler's map of subregions)
         Sub_region_code, #same as above
         Plot_type,
         Survey_method,
         Plot_Shape,
         Prop_surveyed = Proportion_of_Plot_surveyed, 
         Selection_method = Plot_Selection_Method, # why are there so many field selected plots?
         Plot_area = Plot_area_km2) %>%
  distinct() %>%
  group_by(Plot) %>%
  add_tally(name = "n_surveys") %>%
  ungroup()

table(pci$Year, pci$n_surveys)

newsurv <- pci %>%
  filter(n_surveys == 1 & Year == 2019)

resurv <- pci %>%
  filter(n_surveys > 1 & Year == 2019)
