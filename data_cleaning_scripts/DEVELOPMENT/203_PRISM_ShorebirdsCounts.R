### filtering to only shorebird data ###########################################################################################################################



included_surveys <- readRDS("Robjects/included_surveys.RDS")
all_polygons <- readRDS("Robjects/all_polygons.RDS")





#list of shorebird species

sb_list <- included_surveys %>%
  filter(Group == "Shorebirds") %>%
  select(Species) %>%
  unique() %>%
  na.omit() %>%
  pull(Species)


plot_sp <- expand.grid(included_surveys$Plot, sb_list) %>%
  distinct()
colnames(plot_sp) <- c("Plot", "Species")




#all plot info to merge with shorebird counts

all_plot_info <- included_surveys %>%
  select(Plot,
         Region_name, 
         Region_code, 
         Sub_region_name, 
         Sub_region_code,
         Year) %>%
  distinct() %>%
  merge(plot_sp, all = TRUE)






#all shorebirds, all years



#add together counts where surveys were done on the same day 
#(ie. was originally subplots which were the grouped together)

sb_date <- included_surveys %>%
  filter(Group == "Shorebirds") %>%
  group_by(Plot, Date, Species) %>%
  mutate(sum_birds = sum(total_birds)) %>%
  select(Plot, Year, Month, Day, Date, yday, Species, sum_birds) %>%
  distinct() %>%
  ungroup() %>%
  merge(all_plot_info, all = TRUE) %>%
  mutate(sum_birds = ifelse(is.na(sum_birds), 0, round(sum_birds)))


# asked Isabel if 4 same day observation of same species should be added or deleted



#maximum birds that were observed on each plot in one year


sb_year <- sb_date %>%
  group_by(Plot, Year, Species) %>%
  mutate(max_birds = max(sum_birds)) %>%
  select( -Month, -Day, -Date, -yday, -sum_birds) %>%
  distinct()  




#all shorebirds, mean between years (from mean within years)
#shoould I round this?

sb_mean <- sb_year %>%
  ungroup() %>%
  group_by(Plot, Species) %>%
  mutate(mean_birds_allyrs = mean(max_birds)) %>%
  select(-Year, -max_birds) %>%
  distinct()


# all shorebird species added together, years seperate

sb_year_grouped <- sb_year %>%
  ungroup() %>%
  group_by(Plot, Year) %>%
  mutate(sum_shorebirds = sum(max_birds)) %>%
  select(-Species, -max_birds) %>%
  distinct()  





# all shorebird species added together, years averaged

sb_mean_grouped <- sb_mean %>%
  ungroup() %>%
  group_by(Plot) %>%
  mutate(sum_shorebirds = sum(mean_birds_allyrs)) %>%
  select(-Species, -mean_birds_allyrs) %>%
  distinct()



#adding locations

#sb_year <- merge(sb_year, all_polygons)
#sb_year_grouped <- merge(sb_year_grouped, all_polygons)
#sb_mean <- merge(sb_mean, all_polygons)
#sb_mean_grouped <- merge(sb_mean_grouped, all_polygons)




#individual species seperated

#sb_year_species <- sb_year %>%
#  ungroup() %>%
#  group_by(Species) %>%
#  group_split()

#sb_mean_species <- sb_mean %>%
#  ungroup() %>%
#  group_by(Species) %>%
#  group_split()
  
  

#names(sb_year_species) <- sort(sb_list)
#names(sb_mean_species) <- sort(sb_list)









#remove the species that don't have a minumum sample size
###don't for the grouped one - it is all shorebirds


#### I did this based on the number of birds, but it should be based on the number of plots
## more or less works for now though



sp_remove <- sb_year %>%
  ungroup() %>%
  group_by(Species) %>%
  summarize(sum_birds = sum(max_birds)) %>%
  filter(sum_birds > 20, Species != "UNSH")

sb_year <- sb_year %>%
  filter(Species %in% sp_remove$Species)

sb_mean <- sb_mean %>%
  filter(Species %in% sp_remove$Species)

#sb_year_species <- sb_year_species[sp_remove$Species]

#sb_mean_species <- sb_mean_species[sp_remove$Species]


# make a present absent variable

sb_year <- sb_year %>%
  mutate(presence = max_birds > 0)

sb_year_grouped <- sb_year_grouped %>%
  mutate(presence = sum_shorebirds > 0)

sb_mean <- sb_mean %>%
  mutate(presence = mean_birds_allyrs > 0)

sb_mean_grouped <- sb_mean_grouped %>%
  mutate(presence = sum_shorebirds > 0)

#Fix the species ones if I actually use them. Right now I'm not using them


#saveRDS
saveRDS(sb_year, "Robjects/sb_year.RDS")
saveRDS(sb_year_grouped, "Robjects/sb_year_grouped.RDS")
saveRDS(sb_mean, "Robjects/sb_mean.RDS")
saveRDS(sb_mean_grouped, "Robjects/sb_mean_grouped.RDS")
#saveRDS(sb_year_species, "Robjects/sb_year_species.RDS")
#saveRDS(sb_mean_species, "Robjects/sb_mean_species.RDS")



