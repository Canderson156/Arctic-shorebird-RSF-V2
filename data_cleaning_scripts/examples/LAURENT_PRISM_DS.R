#Working directory and files#######################################
setwd("M:/PRISM R")
load("PRISM_DS.RData")

#Load in mega PRISM csv
bigdata<-read.csv("EIB_PRISM_Compilation_Rapid survey_CWS&Industry_to_2018_20191110_to Paul Smith.csv", header = T)

bigdata<-data.frame(bigdata)


#Checking data#####################################################

#Making sure that each subregion has unique code (and vice versa)
subregionList <- levels(bigdata$Sub_region_name)
for(i in 2:length(subregionList)){
  print(subregionList[i])
  print(unique(bigdata[bigdata$Sub_region_name==subregionList[i],"Sub_region_code"]))
}
#Some subregions are associated to more than one code: Rasmussen Lowlands (7.1-9.2), Hope Bay Mine Project (7.3-8.3), Izok Mine Project (6.5-7.5), Needs correction (7.4.99-7.5.99), Somerset Island (9.1-10.3), Tahera Mine Project (6.2-7.4)

subregioncodeList <- levels(bigdata$Sub_region_code)
for(i in 2:length(subregioncodeList)){
  print(subregioncodeList[i])
  print(unique(bigdata[bigdata$Sub_region_code==subregioncodeList[i],"Sub_region_name"]))
}
#Codes are not associated to more than one subregion name




#Function to check if any plots were surveyed at more than one date

checkplots <- function(datafile, region, column, plot, Date, year){
  #datafile: dataframe in which all data is contained
  #region: all possible sublevels for the region variable
  #column: character ID of the region variable
  #plot: character ID of the plot variable
  #Date: character ID of the Date variable
  #year: character ID of the year variable
  
  #Subsetting to a single region
  a <- datafile[datafile[,column]==region,]
  #Removing unneeded plot and date levels
  a[,plot] <- as.factor(as.character(a[,plot]))
  a[,Date] <- as.factor(as.character(a[,Date]))
  #Making a table of the frequency of combinations of plot and date
  b <- table(a[,plot], a[,Date])
  #Obtaining the number of times that each plot was surveyed
  counts <- rowSums(b!=0)
  doubleplots <- names(which(counts>=2))
  years <- unique(datafile[datafile[,plot] %in% doubleplots, year])
  if(any(counts>=2)){
    return(list(as.character(region), doubleplots, years, counts, b))
  }
}


#Making a list of subregions that have plots which were surveyed more than once
mult_survey <- vector()

#Loop to go through all subregions
for(i in unique(bigdata$Sub_region_name)){
  temp <- checkplots(bigdata[bigdata$Plot_type=="RAPID",], i, "Sub_region_name", "Standardized_Plot_Name", "Date", "Year")
  mult_survey <- append(mult_survey, temp, after = length(mult_survey))
}

pbp <- bigdata[bigdata$Protected_Area_1_Name=="Polar Bear Pass-Nanuit Itillinga National Wildlife Area",]
pbp$Year <- as.factor(as.character(pbp$Year))
pbp$Standardized_Plot_Name <- as.factor(as.character(pbp$Standardized_Plot_Name))
table(pbp$Year, pbp$Standardized_Plot_Name)
which(bigdata$Standardized_Plot_Name %in% "PBP")



#Creating more specific datasets#############################################

#selecting columns

prism <- dplyr::select(bigdata,
                       plot = Standardized_Plot_Name,
                       year = Year,
                       month = Month,
                       day = Day,
                       date = Date,
                       start = Start_time_1,
                       end = Finish_time_1,
                       type = Plot_type,
                       duration = DERIVED_Survey_Duration..hours.minutes.,
                       region = Region_name,
                       region_code = Region_code,
                       subregion = Sub_region_name,
                       subregion_code = Sub_region_code,
                       zone = Zone_name,
                       cluster = Standardized_Cluster_Code,
                       cluster_size = Cluster_Size_km2,
                       survey_method = Survey_method,
                       Tylercomment = Plot_Comment_1_attribute_Plot_name.1,
                       quality = GIS_Habitat_Quality_2_Code,
                       quality_old = GIS_Habitat_Quality_1_Code,
                       prop_surveyed = Proportion_of_Plot_surveyed,
                       Selection_Method = Plot_Selection_Method,
                       Sighting_code,
                       species = Standardized_Species_Code,
                       group = Group,
                       final_nests = FINAL_Nests_found,
                       final_centroids = FINAL_Number_of_Centroids,
                       final_decision_comment = FINAL_decision_comment,
                       Count_Nests_found,
                       Count_Probable_nest,
                       Count_Pairs,
                       Count_Male,
                       Count_Female,
                       Count_Unknown_sex,
                       Incidentals.Off_Plot_Birds_Seen_Y.N,
                       GIS_UTM_ZONE = GIS_UTM_2_zone, 
                       UTM_SW_Easting = UTM_1_Easting, 
                       UTM_SW_Northing = UTM_1_Northing, 
                       UTM_NW_Easting = UTM_2_Easting, 
                       UTM_NW_Northing = UTM_2_Northing, 
                       UTM_NE_Easting = UTM_3_Easting, 
                       UTM_NE_Northing = UTM_3_Northing, 
                       UTM_SE_Easting = UTM_4_Easting,
                       UTM_SE_Northing = UTM_4_Northing,
                       plot_area = Plot_area_km2,
                       snow_cover = Total_Snow_cover_._in_plot)

str(prism)

prism[, c("prop_surveyed", "Count_Nests_found", "Count_Probable_nest", "Count_Pairs", "Count_Male",
          "Count_Female", "Count_Unknown_sex", "final_centroids", "plot_area", "snow_cover")] <- 
  apply(prism[, c("prop_surveyed", "Count_Nests_found", "Count_Probable_nest", "Count_Pairs", "Count_Male",
                  "Count_Female", "Count_Unknown_sex", "final_centroids", "plot_area", "snow_cover")], 2, as.numeric)

#Names of subregions with associated codes
maxfreq <- function(x){
  y <- table(x)
  z <- which.max(y)
  return(names(z))
}

subregions <- aggregate(prism$subregion_code, by = list(Category=prism$subregion), FUN = maxfreq)
subregions[order(subregions$x),]

# prism$Count_Nests_found <- as.numeric(prism$Count_Nests_found)
# prism$Count_Probable_nest <- as.numeric(prism$Count_Probable_nest)
# prism$Count_Pairs <- as.numeric(prism$Count_Pairs)
# prism$Count_Male <- as.numeric(prism$Count_Male)
# prism$Count_Female <- as.numeric(prism$Count_Female)
# prism$Count_Unknown_sex <- as.numeric(prism$Count_Unknown_sex)
# prism$final_centroids <- as.numeric(prism$final_centroids)

# #Tables
# table(prism$year)
# table(prism$type)
# table(prism$region)
# table(prism$subregion)
# table(prism$survey_method)
# table(prism$quality)
# table(prism$Selection_Method)
# table(prism$Sighting_code)
# table(prism$species)
# table(prism$group)
# table(prism$Sighting_code, prism$species)
# table(prism$Sighting_code, prism$group)

#Subsetting to only include #ShorebirdsBestBirds
shorebirds <- prism[prism$group=="Shorebirds",]

#Removing unidentified shorebirds and non shorebirds (errors in datafile)
shorebirds <- shorebirds[shorebirds$species!="NOPI" & shorebirds$species!="UNSH" & shorebirds$species!="UNYE",]
shorebirds$species <- as.factor(as.character(shorebirds$species))

#Removing plots that were not selected randomly
shorebirds <- shorebirds[shorebirds$Selection_Method!="field selected" & shorebirds$Selection_Method!="Field selected",]

#Removing plots without a habitat assignment *remove this step once data are complete*
shorebirds <- shorebirds[complete.cases(shorebirds$quality),]

#Removing plots with unsuitable habitat
shorebirds$quality <- as.factor(shorebirds$quality)
shorebirds <- shorebirds[shorebirds$quality!="8",]

# table(shorebirds$Tylercomment)
# table(shorebirds$species)
# 
# table(shorebirds$Sighting_code, shorebirds$species)
# table(shorebirds$Sighting_code, shorebirds$group)
# 
# table(shorebirds$Sighting_code, shorebirds$type)

###Creating datasets specific to rapid/intensive###
rapid <- shorebirds[shorebirds$survey_method=="rapid",]
intensive <- shorebirds[shorebirds$survey_method=="not applicable - final decision",]

#Summing up the different sighting types for visits
rapid$number <- rapid$Count_Nests_found*2 + rapid$Count_Probable_nest*2 + rapid$Count_Pairs*2 + rapid$Count_Male + rapid$Count_Female + rapid$Count_Unknown_sex

#Sample sizes for rapid surveys
aggregate(rapid$number, by=list(Category=rapid$species), FUN=sum)

#Sample sizes for intensive survey final decisions
intensive$final_total <- intensive$final_nests + intensive$final_centroids

aggregate(intensive$final_total, by=list(Category=intensive$species), FUN=sum)


#Each subregion should only have one year value associated to it
unique(shorebirds[shorebirds$subregion==unique(shorebirds$subregion)[1],"year"])
unique(shorebirds[shorebirds$subregion==unique(shorebirds$subregion)[2],"year"])
unique(shorebirds[shorebirds$subregion==unique(shorebirds$subregion)[3],"year"])
unique(shorebirds[shorebirds$subregion==unique(shorebirds$subregion)[4],"year"])
unique(shorebirds[shorebirds$subregion==unique(shorebirds$subregion)[5],"year"])
unique(shorebirds[shorebirds$subregion==unique(shorebirds$subregion)[6],"year"])
unique(shorebirds[shorebirds$subregion==unique(shorebirds$subregion)[7],"year"])


#RpdCts############################################################

RpdCts <- dplyr::select(rapid,
                        Region = subregion_code,
                        Plot = plot,
                        Date = date,
                        Species = species,
                        Number = number)


#Function to check if any plots were surveyed at more than one date
checkplots <- function(datafile, region){
  #Subsetting to a single region
  a <- datafile[datafile$Region==region,]
  #Removing unneeded plot and date levels
  a$Plot <- as.factor(as.character(a$Plot))
  a$Date <- as.factor(as.character(a$Date))
  #Making a table of the frequency of combinations of plot and date
  b <- table(a$Plot, a$Date)
  #Obtaining the number of times that each plot was surveyed
  counts <- rowSums(b!=0)
  return(list(b, counts))
}

#Making a list of subregions that have plots which were surveyed more than once
mult_survey <- vector()

#Loop to go through all subregions
for(i in 1:length(unique(RpdCts$Region))){
  test1 <- checkplots(RpdCts, unique(RpdCts$Region)[i], "Region")
  if(any(test1[[2]]==2)){
    mult_survey <- append(mult_survey, as.character(unique(RpdCts$Region)[i]), after = length(mult_survey))
  }
}

mult_survey

test1 <- checkplots(RpdCts, mult_survey[3])
test1[[1]]


#Save workspace###################################################
save.image("PRISM_DS.RData")
