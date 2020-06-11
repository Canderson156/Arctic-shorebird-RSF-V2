
#### Prepare Tyler's shapefiles circa 2019 for extracting coordinates




######################
###################### Import shapefiles
######################


# file paths of all the PRISM geodatabases
#note that the regions are in the order 1,10,11,12,2 .... 

filegdblist <- paste("E:\\2019 PRISM GIS v2\\", list.files("E:\\2019 PRISM GIS v2"), sep = "")

#re-order so that regions are sequential
filegdblist <- filegdblist[c(1,5:12,2:4)] 


#create a list of all of the objects inside those geodatabases
subset(ogrDrivers(), grepl("GDB", name))  # I have no idea what this does, got it from this internet

fc_list <- lapply(filegdblist, ogrListLayers)


#selecting the features that I want to keep
#most of the have _Plots and _Surveyed_Plots. I only kept _Surveyed_Plots. _Plots are the entire grid of all plots
# "Reg7_Rassmussen_Plots_Surveyed" and "Rassmussen_Surveyed_Plots" are identical, so I only used the first one

fc_list_plots <- fc_list
fc_list_plots[[1]] <- fc_list[[1]][3]
fc_list_plots[[2]] <- fc_list[[2]][4:7] 
fc_list_plots[[3]] <- fc_list[[3]][3:5] 
fc_list_plots[[4]] <- fc_list[[4]][7:10] 
fc_list_plots[[5]] <- fc_list[[5]][7]
fc_list_plots[[6]] <- fc_list[[6]][4:10]
fc_list_plots[[7]] <- fc_list[[7]][c(2:3,5:7)]
fc_list_plots[[8]] <- fc_list[[8]][8]
fc_list_plots[[9]] <- fc_list[[9]][c(4:6,8)]
fc_list_plots[[10]] <- fc_list[[10]][8:12] 
fc_list_plots[[11]] <- fc_list[[11]][7] 
fc_list_plots[[12]] <- fc_list[[12]][7]


#create a list of all plot shapefiles
shapefile_list <- readOGR_multi(filegdblist[[1]], fc_list_plots[[1]])

for(i in 2:length(filegdblist)){
  add <- readOGR_multi(filegdblist[[i]], fc_list_plots[[i]])
  shapefile_list <- c(shapefile_list, add)
}

#what is the crs for each shapefile?
check_CRS <- lapply(shapefile_list, crs)
check_CRS <- unlist(check_CRS)



######################
###################### Standardize shapefiles so that they can be merged
######################




#transforms all shapefiles so that they have the same crs

shapefile_proj_list <- lapply(shapefile_list, spTransform, CRSobj = NAD83)



#Creates a dataframe for matching GIS plot names with Historic Plot names if the shapefile doesn't have the standardized version

historic_plot_names <- bigdata_raw %>%
  select(Standardized_Plot_Name, contains("Historic"), Region_name, Region_code, Sub_region_name) %>%
  gather(key = "col", value = "FN", contains("Historic")) %>%
  distinct() %>%
  filter(not.na(FN)) %>%
  select(-col, Plot_GOOD = Standardized_Plot_Name) %>%
  distinct() 


#add the standardized plot names into the historic plot names as well
add_standardized_plots <- bigdata_raw %>%
  select(Standardized_Plot_Name, contains("Historic"), Region_name, Region_code, Sub_region_name) %>%
  gather(key = "col", value = "FN", contains("Historic")) %>%
  select(-FN, -col, Plot_GOOD = Standardized_Plot_Name) %>%
  distinct() %>%
  mutate(FN = Plot_GOOD)

historic_plot_names <- rbind(historic_plot_names, add_standardized_plots) %>%
  distinct()



#functions for proodfing plot names in GIS shapefiles
t1 <- function(n){
  t <- table(shapefile_proj_list[[n]]@data$FN %in% bigdata_raw$Standardized_Plot_Name)
  return(t)
}

t2 <- function(n){
  t <- table(shapefile_proj_list[[n]]@data$Plot_GOOD %in% bigdata_raw$Standardized_Plot_Name)
  return(t)
}



#making corrections to each shapefile
#If I were to do this again I would use simple features instead of spatial polygons

shapefile_proj_list[[1]]@data$FN <- as.character(shapefile_proj_list[[1]]@data$Plot_Name_New) #Region 1
shapefile_proj_list[[1]]@data$FN <- gsub("\r\n", "", shapefile_proj_list[[1]]@data$FN)
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "69091B1"] <- "QC-0012D"
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "39426B1"] <- "QC-0016D"
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "33783D1"] <- "QC-0057C"
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "281250E1"] <- "QC-0091C"
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "230131D1"] <- "QC-0094D"
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "162572C1"] <- "QC-0095C"
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "136350C1"] <- "QC-0121B"
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "82565B1"] <- "QC-0138C"
shapefile_proj_list[[1]]@data <- merge(shapefile_proj_list[[1]]@data, historic_plot_names[historic_plot_names$Region_code == 1,], by = "FN", all.x = TRUE, sort = FALSE)  


shapefile_proj_list[[2]]@data$FN <- as.character(shapefile_proj_list[[2]]@data$Plot_Name) #Region 2.1
shapefile_proj_list[[2]]@data <- merge(shapefile_proj_list[[2]]@data, historic_plot_names[historic_plot_names$Region_code == 1,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[3]]@data$FN <- as.character(shapefile_proj_list[[3]]@data$Plot_Name) #Region 2.2
shapefile_proj_list[[3]]@data <- merge(shapefile_proj_list[[3]]@data, historic_plot_names[historic_plot_names$Region_code == 2,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[4]]@data$FN <- as.character(shapefile_proj_list[[4]]@data$Plot_Name) #Region 2.3
shapefile_proj_list[[4]]@data$FN[shapefile_proj_list[[4]]@data$FN == "BAF-7D"] <- "BAF-7B" # Guessing that BAF-7D is a typo. There should be a BAF-7B but it is missing
shapefile_proj_list[[4]]@data <- merge(shapefile_proj_list[[4]]@data, historic_plot_names[historic_plot_names$Region_code == 2,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[5]]@data$FN <- as.character(shapefile_proj_list[[5]]@data$Plot_Name_1) #Region 2.4
shapefile_proj_list[[5]]@data$FN[nchar(shapefile_proj_list[[5]]@data$FN) == 8] <- gsub("MAR-00", "MAR-0", shapefile_proj_list[[5]]@data$FN[nchar(shapefile_proj_list[[5]]@data$FN) == 8])
shapefile_proj_list[[5]]@data <- merge(shapefile_proj_list[[5]]@data, historic_plot_names[historic_plot_names$Region_code == 2,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[6]]@data$FN <- as.character(shapefile_proj_list[[6]]@data$Plot_Name) #2019 PCI surveys
shapefile_proj_list[[6]]@data <- merge(shapefile_proj_list[[6]]@data, historic_plot_names[historic_plot_names$Region_code == 3,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[7]]@data$FN <- as.character(shapefile_proj_list[[7]]@data$Plot_Name_New) #Region 3.1
shapefile_proj_list[[7]]@data <- merge(shapefile_proj_list[[7]]@data, historic_plot_names[historic_plot_names$Region_code == 3,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[8]]@data$FN <- as.character(shapefile_proj_list[[8]]@data$Plot_Name_New) #Region 3.2
shapefile_proj_list[[8]]@data$FN[shapefile_proj_list[[8]]@data$FN == "SOPER-0004A "] <- "SOPER-0004A"
shapefile_proj_list[[8]]@data <- merge(shapefile_proj_list[[8]]@data, historic_plot_names[historic_plot_names$Region_code == 3,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[9]]@data$FN <- as.character(shapefile_proj_list[[9]]@data$Plot_Name_New) #Coats Intensive Plots
shapefile_proj_list[[9]]@data <- merge(shapefile_proj_list[[9]]@data, historic_plot_names[historic_plot_names$Region_code == 4,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[10]]@data$FN <- as.character(shapefile_proj_list[[10]]@data$Plot_Name_New) #Region 4.2
shapefile_proj_list[[10]]@data <- merge(shapefile_proj_list[[10]]@data, historic_plot_names[historic_plot_names$Region_code == 4,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[11]]@data$FN <- as.character(shapefile_proj_list[[11]]@data$Plot_Name_New) #Region 4.1
shapefile_proj_list[[11]]@data$FN[shapefile_proj_list[[11]]@data$FN == "SHI-0009B\r\nSHI-0009B\r\n"] <- "SHI-0009B"
shapefile_proj_list[[11]]@data <- merge(shapefile_proj_list[[11]]@data, historic_plot_names[historic_plot_names$Region_code == 4,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[12]]@data$FN <- as.character(shapefile_proj_list[[12]]@data$Plot_Name_New) #Southampton Intensive plots
shapefile_proj_list[[12]]@data <- merge(shapefile_proj_list[[12]]@data, historic_plot_names[historic_plot_names$Region_code == 4,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[13]]@data$FN <- as.character(shapefile_proj_list[[13]]@data$Plot_Name) #Region 5
shapefile_proj_list[[13]]@data <- merge(shapefile_proj_list[[13]]@data, historic_plot_names[historic_plot_names$Region_code == 5,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[14]]@data$FN <- as.character(shapefile_proj_list[[14]]@data$Plot) #Region 6.1
shapefile_proj_list[[14]]@data <- merge(shapefile_proj_list[[14]]@data, historic_plot_names[historic_plot_names$Region_code == 6,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[15]]@data$FN <- as.character(shapefile_proj_list[[15]]@data$Plot_Name_) #Region 6.2
shapefile_proj_list[[15]]@data <- merge(shapefile_proj_list[[15]]@data, historic_plot_names[historic_plot_names$Region_code %in% c(6,7),], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[16]]@data$FN <- as.character(shapefile_proj_list[[16]]@data$Unique_Plot_ID) #Region 6.3
shapefile_proj_list[[16]]@data$FN[shapefile_proj_list[[16]]@data$FN == "AMA-005\r\n"] <- "AMA-005"
shapefile_proj_list[[16]]@data <- merge(shapefile_proj_list[[16]]@data, historic_plot_names[historic_plot_names$Region_code == 6,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[17]]@data$FN <- as.character(shapefile_proj_list[[17]]@data$plot_name) #Region 6.4
shapefile_proj_list[[17]]@data <- merge(shapefile_proj_list[[17]]@data, historic_plot_names[historic_plot_names$Region_code == 6,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[18]]@data$FN <- as.character(shapefile_proj_list[[18]]@data$Plot) #Region 6.5
shapefile_proj_list[[18]]@data <- merge(shapefile_proj_list[[18]]@data, historic_plot_names[historic_plot_names$Region_code == 6,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[19]]@data$FN <- as.character(shapefile_proj_list[[19]]@data$NAME) #Region 6.6
shapefile_proj_list[[19]]@data <- merge(shapefile_proj_list[[19]]@data, historic_plot_names[historic_plot_names$Region_code %in% c(6,7),], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[20]]@data$FN <- as.character(shapefile_proj_list[[20]]@data$Plot_Name_New) #Region 6.7
shapefile_proj_list[[20]]@data$FN[shapefile_proj_list[[20]]@data$Unique_Plot_ID == "1797377_R6"] <- "ARV-0105C"
shapefile_proj_list[[20]]@data$FN[shapefile_proj_list[[20]]@data$Unique_Plot_ID == "1804180_R6"] <- "ARV-0107C"
shapefile_proj_list[[20]]@data$FN[shapefile_proj_list[[20]]@data$Unique_Plot_ID == "887080_R6"] <- "BAK-0080C"
shapefile_proj_list[[20]]@data$FN[shapefile_proj_list[[20]]@data$Unique_Plot_ID == "993119_R6"] <- "BAK-0078N"
shapefile_proj_list[[20]]@data <- merge(shapefile_proj_list[[20]]@data, historic_plot_names[historic_plot_names$Region_code == 6,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[21]]@data$FN <- as.character(shapefile_proj_list[[21]]@data$Plot_Name_) #Region 7.1
shapefile_proj_list[[21]]@data <- merge(shapefile_proj_list[[21]]@data, historic_plot_names[historic_plot_names$Region_code %in% c(6,7),], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[22]]@data$FN <- as.character(shapefile_proj_list[[22]]@data$Plot_Name_New) #Region 7.2
shapefile_proj_list[[22]]@data$FN <- gsub("HOP", "HOB", shapefile_proj_list[[22]]@data$FN)
shapefile_proj_list[[22]]@data$FN <- gsub("-0", "-", shapefile_proj_list[[22]]@data$FN)
shapefile_proj_list[[22]] <- shapefile_proj_list[[22]][shapefile_proj_list[[22]]$FN %in% historic_plot_names$FN,]
#there were 6 plots that say theyw ere surveyed in 2011 that don't have any survey data associated with them
shapefile_proj_list[[22]]@data <- merge(shapefile_proj_list[[22]]@data, historic_plot_names[historic_plot_names$Region_code %in% c(7,8),], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[23]]@data$FN <- as.character(shapefile_proj_list[[23]]@data$plot) #Region 7.4
shapefile_proj_list[[23]]@data <- merge(shapefile_proj_list[[23]]@data, historic_plot_names[historic_plot_names$Region_code == 7,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[24]]@data$FN <- as.character(shapefile_proj_list[[24]]@data$plot) #Region 7.5
shapefile_proj_list[[24]]@data <- merge(shapefile_proj_list[[24]]@data, historic_plot_names[historic_plot_names$Region_code == 7,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[25]]@data$FN <- as.character(shapefile_proj_list[[25]]@data$Plot_Name_New) #Region 7.6
shapefile_proj_list[[25]] <- shapefile_proj_list[[25]][shapefile_proj_list[[25]]$plot!=94005,]
shapefile_proj_list[[25]] <- shapefile_proj_list[[25]][shapefile_proj_list[[25]]$plot!=94033,]
shapefile_proj_list[[25]] <- shapefile_proj_list[[25]][shapefile_proj_list[[25]]$FN %in% historic_plot_names$FN,]
shapefile_proj_list[[25]]@data <- merge(shapefile_proj_list[[25]]@data, historic_plot_names[historic_plot_names$Region_code %in% c(7,9),], by = "FN", all.x = TRUE, sort = FALSE)  

#there is no survey data for RAS-0046 and RAS-0047 that I could find

shapefile_proj_list[[26]]@data$FN <- as.character(shapefile_proj_list[[26]]@data$Unique_Plot_ID) #Region 8
shapefile_proj_list[[26]]@data$FN <- gsub("\r\n", "", shapefile_proj_list[[26]]@data$FN)
shapefile_proj_list[[26]]@data$FN[shapefile_proj_list[[26]]@data$FN == "VIC-0001VIC-0001VIC-0001"] <- "VIC-0001"
shapefile_proj_list[[26]]@data$FN <- gsub("HOP", "HOB", shapefile_proj_list[[26]]@data$FN)
shapefile_proj_list[[26]]@data$FN[grepl("HOB", shapefile_proj_list[[26]]@data$FN)] <- gsub("-0", "-", shapefile_proj_list[[26]]@data$FN[grepl("HOB", shapefile_proj_list[[26]]@data$FN)])
shapefile_proj_list[[26]]@data <- merge(shapefile_proj_list[[26]]@data, historic_plot_names[historic_plot_names$Region_code %in% c(7,8),], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[27]]@data$FN <- as.character(shapefile_proj_list[[27]]@data$Plot_Name_New) #Region 9.1
shapefile_proj_list[[27]]@data$FN[shapefile_proj_list[[27]]@data$FN == "POW-1029D\r\nPOW-1029A\r\nPOW-1029D"] <- "POW-1029D"
shapefile_proj_list[[27]]@data <- merge(shapefile_proj_list[[27]]@data, historic_plot_names[historic_plot_names$Region_code == 9,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[28]]@data$FN <- as.character(shapefile_proj_list[[28]]@data$Plot_Name_New) #Region 9.2
shapefile_proj_list[[28]]@data$FN[shapefile_proj_list[[28]]@data$FN == "POW-1029D\r\nPOW-1029A\r\nPOW-1029D"] <- "POW-1029D"
shapefile_proj_list[[28]]@data <- merge(shapefile_proj_list[[28]]@data, historic_plot_names[historic_plot_names$Region_code == 9,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[29]]@data$FN <- as.character(shapefile_proj_list[[29]]@data$Plot_Name) #Region 9.3
shapefile_proj_list[[29]]@data <- merge(shapefile_proj_list[[29]]@data, historic_plot_names[historic_plot_names$Region_code == 9,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[30]]@data$FN <- as.character(shapefile_proj_list[[30]]@data$Plot_No) #Region 9.5. Based on Lauren't notes, many of these are probably duplicates that are also in region 10
somerset_plotnames <- bigdata_raw %>%
  select(Historic_Field_Plot_name_.1, Standardized_Plot_Name, Sub_region_name, Region_code) %>%
  filter(Historic_Field_Plot_name_.1 %in% shapefile_proj_list[[30]]@data$Plot_No) %>%
  filter(Sub_region_name == "Somerset Island") %>%
  unique() %>%
  select(-Sub_region_name)
colnames(somerset_plotnames) <- c("Plot_No", "Standardized_Plot_Name", "Region_code")
shapefile_proj_list[[30]]@data <- merge(shapefile_proj_list[[30]]@data, somerset_plotnames, all = TRUE, sort = FALSE)
shapefile_proj_list[[30]]@data$FN <- as.character(shapefile_proj_list[[30]]@data$Standardized_Plot_Name) 
shapefile_proj_list[[30]] <- shapefile_proj_list[[30]][!is.na(shapefile_proj_list[[30]]$FN),]
shapefile_proj_list[[30]]@data <- merge(shapefile_proj_list[[30]]@data, historic_plot_names[historic_plot_names$Region_code %in% c(9,10),], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[31]]@data$FN <- as.character(shapefile_proj_list[[31]]@data$Plot_Name_New) #Region 10.1
shapefile_proj_list[[31]]@data$FN[shapefile_proj_list[[31]]@data$FN == "EGI-0027D"] <- "EGI-0027"
shapefile_proj_list[[31]]@data <- merge(shapefile_proj_list[[31]]@data, historic_plot_names[historic_plot_names$Region_code == 10,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[32]]@data$FN <- as.character(shapefile_proj_list[[32]]@data$Plot_Name) #Region 10.2
shapefile_proj_list[[32]]@data$FN[shapefile_proj_list[[32]]@data$FN == "ARCH-61C "] <- "ARCH-61C"
shapefile_proj_list[[32]]@data <- merge(shapefile_proj_list[[32]]@data, historic_plot_names[historic_plot_names$Region_code %in% c(10,11),], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[33]]@data$FN <- as.character(shapefile_proj_list[[33]]@data$Plot_Name_New) #Region 10.3
shapefile_proj_list[[33]]@data <- merge(shapefile_proj_list[[33]]@data, historic_plot_names[historic_plot_names$Region_code == 10,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[34]]@data$FN <- as.character(shapefile_proj_list[[34]]@data$Plot_Name) #Region 10.4
shapefile_proj_list[[34]] <- shapefile_proj_list[[34]][!is.na(shapefile_proj_list[[34]]$FN),] 
shapefile_proj_list[[34]] <- shapefile_proj_list[[34]][shapefile_proj_list[[34]]$LABEL %in% historic_plot_names$FN[historic_plot_names$Sub_region_name == "Alert"],]
shapefile_proj_list[[34]]@data <- merge(shapefile_proj_list[[34]]@data, historic_plot_names[historic_plot_names$Sub_region_name == "Alert",], by.x = "LABEL", by.y = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[35]]@data$FN <- as.character(shapefile_proj_list[[35]]@data$Plot_Name_New) #Region 10.5
shapefile_proj_list[[35]]@data <- merge(shapefile_proj_list[[35]]@data, historic_plot_names[historic_plot_names$Region_code == 10,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[36]]@data$FN <- as.character(shapefile_proj_list[[36]]@data$Plot_Name) #Region 11
shapefile_proj_list[[36]]@data$FN <- gsub(" ", "", shapefile_proj_list[[36]]@data$FN)
shapefile_proj_list[[36]]@data <- merge(shapefile_proj_list[[36]]@data, historic_plot_names[historic_plot_names$Region_code == 11,], by = "FN", all.x = TRUE, sort = FALSE)  

shapefile_proj_list[[37]]@data$FN <- as.character(shapefile_proj_list[[37]]@data$Plot_Name_New) #Region 12
shapefile_proj_list[[37]] <- shapefile_proj_list[[37]][!is.na(shapefile_proj_list[[37]]$FN),]
shapefile_proj_list[[37]] <- shapefile_proj_list[[37]][shapefile_proj_list[[37]]$FN != "",]
shapefile_proj_list[[37]]@data <- merge(shapefile_proj_list[[37]]@data, historic_plot_names, by = "FN", all.x = TRUE, sort = FALSE)  
#can't have a region becasue some plots are not in a region



#lapply(shapefile_proj_list, function(x) table.na(x$data$FN))
#lapply(shapefile_proj_list, function(x) any(table.na(x@data$FN)>1))
rm(somerset_plotnames)






#Combine the 36 seperate shapefiles into one huge shapefile with all of the GIS plots
GIS_shapefiles <-  bind(shapefile_proj_list[[1]], shapefile_proj_list[[5]])
for(i in 3:length(shapefile_proj_list)){
  GIS_shapefiles <- bind(GIS_shapefiles, shapefile_proj_list[[i]])
}

#getting rid of all the mismatched data aside from the column name
GIS_shapefiles@data <- GIS_shapefiles@data %>%
  select(Plot_GOOD) %>%
  rename(Plot = Plot_GOOD)


#turn st objects into sf objects

GIS_shapefiles <- st_as_sf(GIS_shapefiles) %>%
  arrange(Plot)

#save RDS
saveRDS(GIS_shapefiles, "Robjects/GIS_shapefiles.RDS")
saveRDS(shapefile_proj_list[[34]], "Robjects/Alert_shapefile.RDS")
