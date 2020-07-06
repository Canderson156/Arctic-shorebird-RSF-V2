#Visualizing all variables

#this name doesn't make sense once you get to this point
all_polygons_df <- readRDS("Robjects/all_polygons_df.RDS")

sb_year <- readRDS("Robjects/sb_year.RDS")
#sb_year_species <- readRDS("Robjects/sb_year_species.RDS")



#long version for making plots with ggplot



all_plots <- merge(all_polygons_df, sb_year) %>%
  gather(key = "variable", value = "value", contains("temp"), contains("lc"))


temps_only <- merge(all_polygons_df, sb_year) %>%
    select(-contains("lc_")) %>%
    gather(key = "variable", value = "value", contains("temp"))

lc_only <- merge(all_polygons_df, sb_year) %>%
  select(-contains("temp_")) %>%
  gather(key = "variable", value = "value", contains("lc"))


test <- lc_only %>%
  filter(Species == "SESA", 
         variable == "lc_nontussock_graminoid_3")


ggplot(test, aes(x = value)) +
  geom_histogram() +
  ggtitle("SESA")


####Univariate: Predictors

#temperature
ggplot(temps_only, aes(value)) +
  geom_density() +
  facet_wrap(~variable, scales = "free")
  
#landcover
ggplot(lc_only, aes(value)) +
  geom_density() +
  facet_wrap(~variable, scales = "free")





#### Univariate: Response
  
  
ggplot(all_plots[all_plots$max_birds >0,], aes(max_birds)) +
    geom_histogram(binwidth = 1) +
    facet_wrap(~Species)
  
  
  
ggplot(all_plots[all_plots$max_birds >0,], aes(max_birds)) +
    geom_histogram(binwidth = 1) +
    facet_wrap(~Species, scales = "free")
  ## can see a difference in the number counted depending if they're uniparental or not
  

#what are the prevelences of the different species? what proportions of plots were they observed



prev <- all_plots %>%
  select(Plot, Species, Year, presence) %>%
  distinct() %>%
  group_by(Species) %>%
  count(presence) %>%
  filter(presence == TRUE) %>%
  mutate(prevalence = n/2913)



#### Multivariate: Collinearity



env_test <- all_polygons_df %>%
  select(-Plot, -Year)

env_test_cor <- round(cor(env_test, use = "pair"), 2) 



pair_plot <- all_polygons_df %>%
  select(-Plot, -Year) %>%
  ggpairs()



#### Predictor-Response



#### Species plots with temperature facets

temps_sp <- temps_only %>%
  group_by(Species) %>%
  group_split()


gg_temps_sp <- function(data){
  title <- unique(data$Species)

  ggplot(data, aes(x = presence, y = value)) +
    geom_boxplot() +
    facet_wrap(~variable) +
    ggtitle(title)
} 

plots_temps_sp <- lapply(temps_sp, gg_temps_sp)





#### Species plots with landcover facets


#Including zeros. Edited so that the zeros will show up in a log scale plot
lc_sp0 <- lc_only %>%
  mutate(value = ifelse(value == 0, 0.00001, value)) %>%
  group_by(Species) %>%
  group_split()


gg_lc_sp <- function(data){
  title <- unique(data$Species)
  
  ggplot(data, aes(colour = presence, x = value)) +
    geom_density() +
    facet_wrap(~variable, scales = "free_y") +
    ggtitle(title)
} 

plots_lc0_sp <- lapply(lc_sp0, gg_lc_sp)



#excluding zeros
lc_sp1 <- lc_only %>%
  filter(value != 0) %>%
  group_by(Species) %>%
  group_split()


plots_lc1_sp <- lapply(lc_sp1, gg_lc_sp)



#### Temp plots with Species facets


temps_var <- temps_only %>%
  group_by(variable) %>%
  group_split()



gg_temps_var <- function(data){
  title <- unique(data$variable)
  
  ggplot(data, aes(x = presence, y = value)) +
    geom_boxplot() +
    facet_wrap(~Species) +
    ggtitle(title)
} 

plots_temps_var <- lapply(temps_var, gg_temps_var)




#### LC plots with species facets

#Including zeros. Edited so that the zeros will show up in a log scale plot

lc_var0 <- lc_only %>%
  mutate(value = ifelse(value == 0, 0.00001, value)) %>%
  group_by(variable) %>%
  group_split()




gg_lc_var <- function(data){
  title <- unique(data$variable)
  
  ggplot(data, aes(x = value, fill = presence)) +
    geom_histogram(bins = 100) +
    facet_wrap(~Species, scales = "free_y") +
    ggtitle(title)
} 

plots_lc0_var <- lapply(lc_var0, gg_lc_var)



#excluding zeros
lc_var1 <- lc_only %>%
  filter(value != 0) %>%
  group_by(variable) %>%
  group_split()


plots_lc1_var <- lapply(lc_var1, gg_lc_var)








