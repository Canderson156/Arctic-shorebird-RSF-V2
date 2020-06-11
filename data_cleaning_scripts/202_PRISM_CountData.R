

#### Filter out plots that aren't suitable for SDM analysis. Combine count data and spatial data

bigdata <- readRDS("Robjects/bigdata.RDS")
prism <- readRDS("Robjects/prism.RDS")
all_coords <- readRDS("Robjects/all_coords.RDS")


######################
###################### Filter out plots that aren't suitable for SDM analysis
######################


# select only the columns containing required data
incuded_surveys <- select(prism,
                Plot,
                Survey_Lead,
                Year, #1994-2019
                Month, #mostly June, some July, 475 "not applicable"
                Day,
                Date,
                yday,
                Start_time_1,
                Region_name, #89 plots with NAs that are outside the regions (south of Mackenzie Delta)
                Region_code, #89 plots with NAs that are outside the regions (south of Mackenzie Delta)
                Sub_region_name, #170 plots with NAs. 89 as above. 78 in Foxe basin becasue of differences in how subregions were calculated over time (2019 had no subregions). 3 in North Archipelago are confusing, but may stem from weird subregions that overlap near Alert (see email from Laurent RE Tyler's map of subregions)
                Sub_region_code, #same as above
                Plot_type,
                Survey_method,
                Plot_Shape,
                Quality_1,
                Quality_2, # how were these defined, when were they changed, which one is the one we should use, are these being redone?
                Prop_surveyed, 
                Selection_method,
                Plot_area,
                Sighting_code, 
                Species,  #XXXX means nothing was observed
                Group,
                Count_Nests_found,
                Count_Probable_nest,
                Count_Pairs,
                Count_Male,
                Count_Female,
                Count_Unknown_sex,
                Human_development_details) %>%
  distinct()




#remove the plots that weren't rapid plots


incuded_surveys <- incuded_surveys %>%
  filter(Survey_method == "rapid") %>%
  filter(Plot_type !="RECONNAISSANCE") 


#keep only on plot observations
## 0 = final decision, 1 = on plot, 2 = incidental, 3 = mammal, 4 = wildlife sign 
incuded_surveys <- filter(incuded_surveys, Sighting_code == 1)



#replace missing values of counts with 0s

incuded_surveys <- incuded_surveys %>%
  mutate_at(c("Count_Nests_found", 
              "Count_Probable_nest", "Count_Pairs", "Count_Male", 
              "Count_Female", "Count_Unknown_sex"), if.na.0)



#create a total birds column and unique survey identifiers
#re-evaluate if this is still useful

incuded_surveys <- incuded_surveys %>%
  mutate(total_birds = (Count_Nests_found*2) + (Count_Probable_nest*2) + (Count_Pairs*2) + Count_Male + Count_Female + Count_Unknown_sex) %>%
  mutate(plot_date = paste(Plot, Date, Start_time_1)) %>%
  mutate(plot_year = paste(Plot, Year)) %>%
  mutate(plot_species = paste(Plot, Species))

#%>%
#  group_by(plot_date) %>%
#  add_tally(total_birds)




# add a column for comparing plot selection methods biases
#re-evaluate if this is still useful
incuded_surveys <- incuded_surveys %>%
  mutate(comparison = Selection_method, 
         comparison = ifelse(Selection_method == "field selected" & Survey_Lead == "Industry", "field selected - industry", comparison),
         comparison = ifelse(Selection_method == "field selected" & Plot_type == "INTENSIVE", "field selected - intensive", comparison),
         comparison = ifelse(comparison == "field selected", "field selected - other", comparison))




#add a column that subs in quality code 1 when quality code 2 is missing

incuded_surveys <- incuded_surveys %>%
  mutate(quality = ifelse(is.na(Quality_2), Quality_1, Quality_2),
         quality = ifelse(quality %in% c(1,2,3), quality, NA))





#### filtering out data that is problematic

#remove plots from Prince Charles Island 1996 - surveys were quite different due to flooding
#remove plots that were surveyed in 2019 - does it make sense to include plots that I will be looking at the difference between later?

incuded_surveys <- incuded_surveys %>%
  filter(!(Region_code == 3 & Year == 1996))

incuded_surveys <- incuded_surveys %>%
  filter(!(Year == 2019))



#save RDS
saveRDS(incuded_surveys, "Robjects/included_surveys.RDS")































### filtering to only shorebird data ###########################################################################################################################



## only shorebird observations


sb_list <- incuded_surveys %>%
  filter(Group == "Shorebirds") %>%
  select(Species) %>%
  unique() %>%
  na.omit() %>%
  pull(Species)


#all shorebirds, all years, all surveys
#sb <- incuded_surveys %>%
#  filter(Group == "Shorebirds") %>%


#all shorebirds, all years, mean of within year surveys
sb <- incuded_surveys %>%
  filter(Group == "Shorebirds") %>%
  group_by(plot_year, Species) %>%
  mutate(mean_birds_year = mean(total_birds)) %>%
  select(Plot,
         Species,
         Survey_Lead,
         Year, 
         Region_name, 
         Region_code, 
         Sub_region_name, 
         Sub_region_code, 
         Plot_type,
         Survey_method,
         Plot_Shape,
         quality,
         Selection_method, 
         Plot_area,
         comparison,
         Human_development_details,
         plot_year,
         plot_species,
         mean_birds_year) %>%
  distinct()




#all shorebirds, mean between years (from mean within years)


sb <- sb %>%
  ungroup() %>%
  group_by(Plot) %>%
  ungroup() %>%
  group_by(plot_species) %>%
  mutate(mean_birds_all = mean(mean_birds_year)) %>%
  select(Plot,
         Species,
         Survey_Lead,
         Region_name, 
         Region_code, 
         Sub_region_name, 
         Sub_region_code, 
         Plot_type,
         Survey_method,
         #Plot_Shape, # a few plots differ between years
         #quality, # a few plots differ between years
         Selection_method, 
         Plot_area,
         comparison,
         plot_species,
         mean_birds_all) %>%
  distinct()


# above, with all shorebird species added together

SB <- sb %>%
  ungroup() %>%
  group_by(Plot) %>%
  mutate(sum_shorebirds = sum(mean_birds_all)) %>%
  select(Plot,
         #Species,
         Survey_Lead,
         Region_name, 
         Region_code, 
         Sub_region_name, 
         Sub_region_code, 
         Plot_type,
         Survey_method,
         #Plot_Shape, # a few plots differ between years
         #quality, # a few plots differ between years
         Selection_method, 
         #Plot_area,
         comparison,
         #plot_species,
         sum_shorebirds) %>%
  distinct()



SB <- SB %>%
  merge(allplots, all = TRUE) %>%
  mutate(sum_shorebirds = ifelse(is.na(sum_shorebirds), 0, round(sum_shorebirds)))



## use my expand grid from previous data cleaning script when I want to model by species





### Adding goose data ###########################################################################################################################


geese_sp <- c("CAGO", "SNGO", "ROGO", "CACG", "GWFG")

geese_plots <- incuded_surveys %>%
  filter(Species %in% geese_sp) %>%
  select(Plot,
         plot_date,
         plot_year,
         total_birds) %>%
  group_by(plot_date) %>% 
  mutate(sum_geese_survey = sum(total_birds)) %>%
  ungroup() %>%
  select(-plot_date, -total_birds) %>%
  distinct() %>%
  group_by(plot_year) %>%
  mutate(mean_geese_year = mean(sum_geese_survey)) %>%
  ungroup() %>%
  select(-plot_year, -sum_geese_survey) %>%
  distinct() %>%
  group_by(Plot) %>% 
  mutate(mean_geese = round(mean(mean_geese_year))) %>%
  ungroup() %>%
  select(-mean_geese_year) %>%
  distinct()


#would I rather add this to the sb object that has individual species rather than summaries for all shorebirds? 
SB <- SB %>%
  merge(geese_plots, all = TRUE) %>%
  mutate(mean_geese = ifelse(is.na(mean_geese), 0, mean_geese))

rm(geese_sp, geese_plots, sb)


