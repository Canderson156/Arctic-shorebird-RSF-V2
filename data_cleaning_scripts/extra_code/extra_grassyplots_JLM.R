

all_polygons <- readRDS("Robjects/all_polygons.RDS")

grassy <- read_csv("/Volumes/Mac HD/Users/Christine/Desktop/Jean Louis Map/grassy_plots_R.csv") %>%
  distinct()

grassy_plots <- all_polygons %>%
  filter(Plot %in% grassy$Plot)

camp <- st_read("/Volumes/Mac HD/Users/Christine/Desktop/Jean Louis Map/2019 PCI Misc points.gpx")

camp <- camp %>%
  select(name, geometry) %>%
  filter(name == "CABIN") %>%
  mutate(name = "camp") %>%
  rename(Plot = name) %>%
  st_transform(NAD83)

dist <- st_distance(camp, grassy_plots) %>%
  set_units(km)

grassy_plots$dist_km <- dist[1:17]

st_write(camp, "exported/grassy_plots/PCI_camp.shp")
st_write(grassy_plots, "exported/grassy_plots/grassy_plots.shp")
write_csv(grassy_plots, "exported/grassy_plots.csv")
