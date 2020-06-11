options(scipen=50)




## remove the GIS shapefiles that aren't in the list of plots I want to check

GIS_check1 <- GIS_shapefiles %>%
  filter(Plot %in% prism_polys_proj$Plot) %>%
  arrange(Plot) %>%
  filter(duplicated(Plot) == FALSE)


## make a version of Isabel's plots that matches this one

PRISM_check1 <- prism_polys_proj %>%
  filter(Plot %in% GIS_check1$Plot) %>%
  arrange(Plot) %>%
  filter(duplicated(Plot) == FALSE)


## measuring distance between them


dist <- st_distance(GIS_check1, PRISM_check1, by_element = TRUE)
dist <- set_units(dist, km)

dist2 <- round(as.numeric(dist))
check <- dist2[dist2 > 0]

PRISM_check1$dist <- dist2

md <- plot_coords %>%
  select(Plot, Map_datum)

PRISM_check1 <- merge(PRISM_check1, md)

test <- PRISM_check1%>%
  filter(Map_datum == "NAD 1927")

test1 <- PRISM_check1%>%
  filter(dist > 0)


test2 <- read.csv("comparison April 18/comparing_plot_coords.csv") %>%
  select(Plot, dist) %>%
  distinct()

x1 <- test1 %>%
  filter(Plot %notin% test2$Plot)

x2 <- test2 %>%
  filter(Plot %notin% test1$Plot)


#########

# breaking it down by group
# plots that had one location in Isabel's dataset

PRISM_checkOne <- prism_polys_proj %>%
  filter(Plot %in% coordsOne$Plot) %>%
  arrange(Plot)


GIS_checkOne <- GIS_shapefiles %>%
  filter(Plot %in% PRISM_checkOne$Plot) %>%
  arrange(Plot) %>%
  filter(duplicated(Plot) == FALSE)

#a bunch of them don't have a GIS plot

PRISM_checkOne <- PRISM_checkOne %>%
  filter(Plot %in% GIS_checkOne$Plot) %>%
  arrange(Plot)

PRISM_checkOne$dist <- st_distance(GIS_checkOne, PRISM_checkOne, by_element = TRUE)
PRISM_checkOne$dist <- as.numeric(round(set_units(PRISM_checkOne$dist, km)))

checkOne_plots <- PRISM_checkOne[PRISM_checkOne$dist > 0,] %>%
  as.data.frame() %>%
  select(Plot, dist)

checkOne_data <- merge(checkOne_plots, bigdata_raw, by.x = "Plot", by.y = "Standardized_Plot_Name")

write.csv(checkOne_data, "checkOne_data.csv")




