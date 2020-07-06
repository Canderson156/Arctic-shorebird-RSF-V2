

#### Filter out plots that aren't suitable for SDM analysis. Combine count data and spatial data

prism <- readRDS("Robjects/prism.RDS")
all_coords <- readRDS("Robjects/all_coords.RDS")
all_polygons <- readRDS("Robjects/all_polygons.RDS")


test <- included_surveys %>%
  filter(Plot == "TAG-IntE")



######################
###################### Filter out plots that aren't suitable for SDM analysis
######################


# select only the columns containing required data
included_surveys <- select(prism,
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


included_surveys <- included_surveys %>%
  filter(Survey_method == "rapid") %>%
  filter(Plot_type !="RECONNAISSANCE") 


#keep only on plot observations
## 0 = final decision, 1 = on plot, 2 = incidental, 3 = mammal, 4 = wildlife sign 
included_surveys <- filter(included_surveys, Sighting_code == 1)



#replace missing values of counts with 0s

included_surveys <- included_surveys %>%
  mutate_at(c("Count_Nests_found", 
              "Count_Probable_nest", "Count_Pairs", "Count_Male", 
              "Count_Female", "Count_Unknown_sex"), if.na.0)



#create a total birds column and unique survey identifiers
#re-evaluate if this is still useful

included_surveys <- included_surveys %>%
  mutate(total_birds = (Count_Nests_found*2) + (Count_Probable_nest*2) + (Count_Pairs*2) + Count_Male + Count_Female + Count_Unknown_sex) %>%
  mutate(plot_date = paste(Plot, Date)) %>%
  mutate(plot_year = paste(Plot, Year)) %>%
  mutate(plot_species = paste(Plot, Species))

#%>%
#  group_by(plot_date) %>%
#  add_tally(total_birds)




# add a column for comparing plot selection methods biases
#re-evaluate if this is still useful
#included_surveys <- included_surveys %>%
#  mutate(comparison = Selection_method, 
#         comparison = ifelse(Selection_method == "field selected" & Survey_Lead == "Industry", "field selected - industry", comparison),
#         comparison = ifelse(Selection_method == "field selected" & Plot_type == "INTENSIVE", "field selected - intensive", comparison),
#         comparison = ifelse(comparison == "field selected", "field selected - other", comparison))




#add a column that subs in quality code 1 when quality code 2 is missing

#included_surveys <- included_surveys %>%
#  mutate(quality = ifelse(is.na(Quality_2), Quality_1, Quality_2),
#         quality = ifelse(quality %in% c(1,2,3), quality, NA))





#### filtering out data that is problematic

#remove plots from Prince Charles Island 1996 - surveys were quite different due to flooding
#remove plots that were surveyed in 2019 - does it make sense to include plots that I will be looking at the difference between later?

included_surveys <- included_surveys %>%
  filter(!(Region_code == 3 & Year == 1996))

included_surveys <- included_surveys %>%
  filter(!(Year == 2019))





#### remove plots that we don't have coordinates for

included_surveys <- included_surveys %>%
  filter(Plot %in% all_polygons$Plot)




#remove plots with human developments in or adjacent

included_surveys <- included_surveys %>%
  filter(Plot %notin% included_surveys$Plot[included_surveys$Human_development_details != "not recorded"])
    



#save RDS
saveRDS(included_surveys, "Robjects/included_surveys.RDS")



### plots to exclude - ask Laurent for his list once he is done
#SOI-0082C - "4 of us walked in different directsions for 20 mins"
#ALE-2529 -"surveyed by skidoo"
#MGP-6102A - "Plot only partially surveyed by Charles"
#plots containing or adjacent to human developments
# 	KWI-0012, MDW-0099A, ALE-1670, ALE-7140, CLGT-92020, CLGT-92021, CLGT-92022, RAS-0072, RAS-0030
#proportion surveyed? 



test <- included_surveys %>%
  filter(Prop_surveyed < 1) %>%
  select(-contains("Count"), -total_birds, -Species, -Group, -plot_species) %>%
  distinct() %>%
  group_by(plot_date) %>%
  mutate(total_surveyed = sum(Prop_surveyed))

test <- bigdata_raw %>%
  select(Standardized_Plot_Name, Proportion_of_Plot_surveyed)
  

