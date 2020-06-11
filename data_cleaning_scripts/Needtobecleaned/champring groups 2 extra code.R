################################################

#which plots do I have in the GIS database that aren't in the PRISM database?


no_surveys <- GIS_shapefiles %>%
  filter(Plot %notin% allplots$Plot)


test <- prism %>%
  filter(Sub_region_name %in% c("Rasmussen Lowlands (Region 7)", "Rasmussen Lowlands (Region 9)"))





#which plots have duplicates in the GIS database?

dups <- GIS_shapefiles %>%
  distinct(Plot) %>%
  filter(allDuplicated(Plot) == TRUE)

## Alert and Somerset Island (CRE - Cresswell Bay and SOI - Somerset Island) 

#which group do these 3 fall into?


table(dups$Plot %in% g1_field_modified$Plot) #2 plots
table(dups$Plot %in% g2_no_gis$Plot)
table(dups$Plot %in% g3_yes_gis$Plot) #72 plots




#### seperate out the plots that we have no location data for. 







###### Map Datums

table(g0_no_coords $Map_datum)

table(g1_field_modified $Map_datum)

table(g2_no_gis $Map_datum)


test <- prism %>%
  filter(Year == 2007) %>%
  filter(Plot_type == "RECONNAISSANCE") %>%
  distinct()




#which UTMS did Isabel convert from another format?
table(allplots$UTM_1_Status)



#looking at colums thatr Isabel reccomnded to understand her process for entering coordinates

IBnotes <- bigdata %>%
  select(Plot = Standardized_Plot_Name,
         Source_File_coordinates,
         Comment_Datamanagement_location,
         Comment_UTM) %>%
  distinct()

test <- IBnotes %>%
  filter(Plot %in% g0_no_coords$Plot) %>%
  distinct()


xx <- bigdata_raw %>%
  filter(Standardized_Plot_Name == "BAK-0025")

#testing coordinates

xAEA <- GIS_shapefiles[1,]
xLCC <- st_transform(xAEA, LCC)
xNAD83 <- st_transform(xAEA, NAD83)
