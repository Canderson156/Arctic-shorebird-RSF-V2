#cross-referencing plots in Isabel's DF vs Tyler's shapefiles-------------------------------------------------------

library(rgdal)
library(rgeos)
library(raster)
library(tidyverse)


#functions

'%!in%' <- function(x,y)!('%in%'(x,y))

lunique <- function(x) length(unique(x))

sunique <- function(x) sort(unique(x))

table.na <- function (..., useNA = 'always') base::table(..., useNA = useNA)

#import full dataset

bigdata <-read.csv("E:/Christine_temp/Arctic shorebird RSF/Version 2 - Manuscript/data/PRISM/PRISM 1994 to 2019_20191210.csv", stringsAsFactors = FALSE, na.strings=c("", "NA"))
#allprismrapid <- read.csv ("E:/2019 PRISM GIS v2/Z/allprismrapid.csv")

allprismrapid <- bigdata %>%
  filter(Plot_type == "RAPID") %>%
  filter(Survey_method == "rapid") %>%
  mutate(plot = Standardized_Plot_Name) %>%
  filter(Year != 2019)
  

#The input file geodatabase
filegdblist <- list.files("E:/2019 PRISM GIS v2")
setwd("E:/2019 PRISM GIS v2")

#List all feature classes in a file geodatabase
lapply(filegdblist, FUN = ogrListLayers)

#Read the feature class
surveyed1 <- readOGR(dsn=filegdblist[[1]], layer="Region1_Plots_Surveyed_ALL")@data

surveyed10.1 <- readOGR(dsn=filegdblist[[2]], layer="Reg10_Surveyed_Plots_2014")@data

surveyed10.2 <- readOGR(dsn=filegdblist[[2]], layer="Reg10_Surveyed_Plots_2013")@data

surveyed10.3 <- readOGR(dsn=filegdblist[[2]], layer="Reg10_Surveyed_Plots_2011")@data

surveyed10.4 <- readOGR(dsn=filegdblist[[2]], layer="Reg10_Surveyed_Plots_2001_Alert")@data

surveyed10.5 <- readOGR(dsn=filegdblist[[2]], layer="Reg10_Surveyed_Plots_2007")@data

surveyed10.6 <- readOGR(dsn=filegdblist[[2]], layer="Reg10_Surveyed_Plots_2001_CRE_SOI")@data

surveyed11 <- readOGR(dsn=filegdblist[[3]], layer="Region11_SurveyedPlots")@data

surveyed12 <- readOGR(dsn=filegdblist[[4]], layer="Region12_Surveyed_Plots")@data

surveyed2.1 <- readOGR(dsn=filegdblist[[5]], layer="Reg2_Mansel_Surveyed_Plots")@data

surveyed2.2 <- readOGR(dsn=filegdblist[[5]], layer="Reg2_Sirmilik_Surveyed_Plots")@data

surveyed2.3 <- readOGR(dsn=filegdblist[[5]], layer="Reg2_Baffin_Surveyed_Plots")@data

surveyed2.4 <- readOGR(dsn=filegdblist[[5]], layer="Reg2_Mary_River_Surveyed_Plots")@data

surveyed3.1 <- readOGR(dsn=filegdblist[[6]], layer="Reg3_Plots_Surveyed_1996_1997")@data

surveyed3.2 <- readOGR(dsn=filegdblist[[6]], layer="Reg3_West_Baffin_Plots_Surveyed_Plots_2003_2004")@data

surveyed4.1 <- readOGR(dsn=filegdblist[[7]], layer="SHI_rapid_plots_surveyed")@data

surveyed4.2 <- readOGR(dsn=filegdblist[[7]], layer="Coats_rapid_plots_surveyed")@data

surveyed5 <- readOGR(dsn=filegdblist[[8]], layer="Reg5_Plots_Surveyed")@data

surveyed6.1 <- readOGR(dsn=filegdblist[[9]], layer="Reg6_Gahcho_Kue_Surveyed_Plots")@data

surveyed6.2 <- readOGR(dsn=filegdblist[[9]], layer="Reg6_Izok_Surveyed_Plots")@data

surveyed6.3 <- readOGR(dsn=filegdblist[[9]], layer="Region6_Amaruq_Surveyed_Plots")@data

surveyed6.4 <- readOGR(dsn=filegdblist[[9]], layer="Region6_Kiggavik_Plots_Surveyed")@data

surveyed6.5 <- readOGR(dsn=filegdblist[[9]], layer="Region6_Meadowbank_Surveyed_Plots")@data

surveyed6.6 <- readOGR(dsn=filegdblist[[9]], layer="Region6_Tahera_Surveyed_Plots")@data

surveyed6.7 <- readOGR(dsn=filegdblist[[9]], layer="Region6_2015_Surveyed_Plots")@data

surveyed7.1 <- readOGR(dsn=filegdblist[[10]], layer="Reg7_Izok_Plots_Surveyed")@data

surveyed7.2 <- readOGR(dsn=filegdblist[[10]], layer="Reg7_Hope_Bay_Plots_Surveyed")@data

surveyed7.3 <- readOGR(dsn=filegdblist[[10]], layer="Tahera_Selected_Plots")@data

surveyed7.4 <- readOGR(dsn=filegdblist[[10]], layer="Reg7_Plots_Surveyed_2015")@data

surveyed7.5 <- readOGR(dsn=filegdblist[[10]], layer="Reg7_Plots_Surveyed_2017")@data

surveyed7.6 <- readOGR(dsn=filegdblist[[10]], layer="Reg7_Rassmussen_Plots_Surveyed")@data

surveyed8 <- readOGR(dsn=filegdblist[[11]], layer="Reg8_Surveyed_Plots")@data

surveyed9.1 <- readOGR(dsn=filegdblist[[12]], layer="Banks_Surveyed_Plots")@data

surveyed9.2 <- readOGR(dsn=filegdblist[[12]], layer="Prince_Of_Wales_Surveyed_Plots")@data

surveyed9.3 <- readOGR(dsn=filegdblist[[12]], layer="Victoria_Surveyed_Plots")@data

surveyed9.4 <- readOGR(dsn=filegdblist[[12]], layer="Rassmussen_Surveyed_Plots")@data
 
surveyed9.5 <- readOGR(dsn=filegdblist[[12]], layer="Somerset_Surveyed_Plots")@data


#Going through each shapefile individually since most have missing plot names

#SURVEYED1
surveyed1$FN <- as.character(surveyed1$Plot_Name_New)
surveyed1$FN

#Remove \r\n at the end of two plot IDs
surveyed1$FN <- gsub("\r\n", "", surveyed1$FN)

#Fix duplicate plots
#QC-0012B (68882B1) should be farther NE than QC-0012D (69091B1)
surveyed1$FN[surveyed1$Unique_Plot_ID=="69091B1"] <- "QC-0012D"

#QC-0016C (39138B1) should be farther NE than QC-0016D (39426B1)
surveyed1$FN[surveyed1$Unique_Plot_ID=="39426B1"] <- "QC-0016D"

#QC-0057A (33541D1) should be farther NE than QC-0057C (33783D1)
surveyed1$FN[surveyed1$Unique_Plot_ID=="33783D1"] <- "QC-0057C"

#QC-0091B (280950E1) should be farther NE than QC-0091C (281250E1)
surveyed1$FN[surveyed1$Unique_Plot_ID=="281250E1"] <- "QC-0091C"

#QC-0094B (229890D1) should be farther NE than QC-0094D (230131D1)
surveyed1$FN[surveyed1$Unique_Plot_ID=="230131D1"] <- "QC-0094D"

#QC-0095B (162332C1) should be farther NE than QC-0095C (162572C1)
surveyed1$FN[surveyed1$Unique_Plot_ID=="162572C1"] <- "QC-0095C"

#QC-0121A (136109C1) should be farther NE than QC-0121B (136350C1)
surveyed1$FN[surveyed1$Unique_Plot_ID=="136350C1"] <- "QC-0121B"

#QC-0138A (82427B1) should be farther NE than QC-0138C (82565B1)
surveyed1$FN[surveyed1$Unique_Plot_ID=="82565B1"] <- "QC-0138C"

table.na(surveyed1$FN)




#SURVEYED2.1
surveyed2.1$FN <- as.character(surveyed2.1$Plot_Name)
surveyed2.1$FN

table.na(surveyed2.1$FN)



#SURVEYED2.2
surveyed2.2$FN <- as.character(surveyed2.2$Plot_Name)
surveyed2.2$FN

table.na(surveyed2.2$FN)




#SURVEYED2.3
surveyed2.3$FN <- as.character(surveyed2.3$Plot_Name)
surveyed2.3$FN

table.na(surveyed2.3$FN)



#SURVEYED2.4
surveyed2.4$FN <- as.character(surveyed2.4$Plot_Name_1)
surveyed2.4$FN

table.na(surveyed2.4$FN)



#SURVEYED3.1
surveyed3.1$FN <- as.character(surveyed3.1$Plot_Name_New)
surveyed3.1$FN

table.na(surveyed3.1$FN); any(table.na(surveyed3.1$FN)>1)



#SURVEYED3.2
surveyed3.2$FN <- as.character(surveyed3.2$Plot_Name_New)
surveyed3.2$FN

table.na(surveyed3.2$FN); any(table.na(surveyed3.2$FN)>1)



#SURVEYED4.1
surveyed4.1$FN <- as.character(surveyed4.1$Plot_Name_New)
surveyed4.1$FN

surveyed4.1$FN[surveyed4.1$FN=="SHI-0009B\r\nSHI-0009B\r\n"] <- "SHI-0009B"

table.na(surveyed4.1$FN); any(table.na(surveyed4.1$FN)>1)



#SURVEYED4.2
surveyed4.2$FN <- as.character(surveyed4.2$Plot_Name_New)
surveyed4.2$FN

table.na(surveyed4.2$FN); any(table.na(surveyed4.2$FN)>1)




#SURVEYED5
surveyed5$FN <- as.character(surveyed5$Plot_Name)
surveyed5$FN

table.na(surveyed5$FN); any(table.na(surveyed5$FN)>1)



#SURVEYED6.1
surveyed6.1$FN <- as.character(surveyed6.1$Plot)
surveyed6.1$FN

table.na(surveyed6.1$FN); any(table.na(surveyed6.1$FN)>1)



#SURVEYED6.2
surveyed6.2$FN <- as.character(surveyed6.2$Plot_Name_New)
surveyed6.2$FN

table.na(surveyed6.2$FN); any(table.na(surveyed6.2$FN)>1)



#SURVEYED6.3
surveyed6.3$FN <- as.character(surveyed6.3$Unique_Plot_ID)
surveyed6.3$FN

surveyed6.3$FN[surveyed6.3$FN=="AMA-005\r\n"] <- "AMA-005"

table.na(surveyed6.3$FN); any(table.na(surveyed6.3$FN)>1)



#SURVEYED6.4
surveyed6.4$FN <- as.character(surveyed6.4$plot_name)
surveyed6.4$FN

table.na(surveyed6.4$FN); any(table.na(surveyed6.4$FN)>1)



#SURVEYED6.5
surveyed6.5$FN <- as.character(surveyed6.5$Plot)
surveyed6.5$FN

table.na(surveyed6.5$FN); any(table.na(surveyed6.5$FN)>1)



#SURVEYED6.6
surveyed6.6$FN <- as.character(surveyed6.6$NAME)
surveyed6.6$FN

table.na(surveyed6.6$FN); any(table.na(surveyed6.6$FN)>1)



#SURVEYED6.7
surveyed6.7$FN <- as.character(surveyed6.7$Plot_Name_New)
surveyed6.7$FN

#Duplicated plot names
#ARV-0105A (1797272_R6) is NE of ARV-0105C (1797377_R6)
surveyed6.7$FN[surveyed6.7$Unique_Plot_ID=="1797377_R6"] <- "ARV-0105C"

#ARV-0107B (1804077_R6) is NE of ARV-0107C (1804180_R6)
surveyed6.7$FN[surveyed6.7$Unique_Plot_ID=="1804180_R6"] <- "ARV-0107C"

#BAK-0080A (886900_R6) is NE of BAK-0080C (887080_R6)
surveyed6.7$FN[surveyed6.7$Unique_Plot_ID=="887080_R6"] <- "BAK-0080C"


table.na(surveyed6.7$FN); any(table.na(surveyed6.7$FN)>1)



#SURVEYED7.1
surveyed7.1$FN <- as.character(surveyed7.1$Plot_Name_)
surveyed7.1$FN

table.na(surveyed7.1$FN); any(table.na(surveyed7.1$FN)>1)



#SURVEYED7.2
surveyed7.2$FN <- as.character(surveyed7.2$Plot_Name_New)
surveyed7.2$FN

table.na(surveyed7.2$FN); any(table.na(surveyed7.2$FN)>1)



#SURVEYED7.3
surveyed7.3$FN <- as.character(surveyed7.3$NAME)
surveyed7.3$FN

table.na(surveyed7.3$FN); any(table.na(surveyed7.3$FN)>1)



#SURVEYED7.4
surveyed7.4$FN <- as.character(surveyed7.4$plot)
surveyed7.4$FN

table.na(surveyed7.4$FN); any(table.na(surveyed7.4$FN)>1)



#SURVEYED7.5
surveyed7.5$FN <- as.character(surveyed7.5$plot)
surveyed7.5$FN

table.na(surveyed7.5$FN); any(table.na(surveyed7.5$FN)>1)



#SURVEYED7.6
surveyed7.6$FN <- as.character(surveyed7.6$Plot_Name_New)
surveyed7.6$FN

table.na(surveyed7.6$FN); table.na(surveyed7.6$FN)[table.na(surveyed7.6$FN)>1]

#Duplicated plot names

#Two completely (~100%) overlapping plots have Plot_Name_New = RAS-0005 (plot = 94005; 95005). Deleting plot = 94005
surveyed7.6 <- surveyed7.6[surveyed7.6$plot!=94005,]

#Two partially overlapping plots have Plot_Name_New = RAS-0033. They  have plot = 94033; 95033. In Isabel's datafile, both these plot IDs (94033 and 95033) appear under the historic field plot names of RAS-0005. Given that the GIS polygon named 94033 overlaps with 94032 (RAS-0032) as well, it is likely that it is the plot that was surveyed in 1994 and whose coordinates were unreliable. Isabel's datafile mentions that the coordinates from 1995 more reliable than those from 1994, so they are the ones that are used.
#TL;DR: 95033 *should* (educated guess) be the correct GIS version of RAS-0033.
coordinates(readOGR(dsn=filegdblist[[10]], layer="Reg7_Rassmussen_Plots_Surveyed"))[surveyed7.6$plot=="94033",]
surveyed7.6 <- surveyed7.6[surveyed7.6$plot!=94033,]

table.na(surveyed7.6$FN); any(table.na(surveyed7.6$FN)>1)


#SURVEYED8
surveyed8$FN <- as.character(surveyed8$Unique_Plot_ID)
surveyed8$FN

surveyed8$FN <- gsub("\r\n", "", surveyed8$FN)
surveyed8$FN[surveyed8$FN=="VIC-0001VIC-0001VIC-0001"] <- "VIC-0001"


table.na(surveyed8$FN); any(table.na(surveyed8$FN)>1)



#SURVEYED9.1
surveyed9.1$FN <- as.character(surveyed9.1$Plot_Name_New)
surveyed9.1$FN

table.na(surveyed9.1$FN); any(table.na(surveyed9.1$FN)>1)



#SURVEYED9.2
surveyed9.2$FN <- as.character(surveyed9.2$Plot_Name_New)
surveyed9.2$FN

surveyed9.2$FN[surveyed9.2$FN=="POW-1029D\r\nPOW-1029A\r\nPOW-1029D"] <- "POW-1029D"

table.na(surveyed9.2$FN); any(table.na(surveyed9.2$FN)>1)



#SURVEYED9.3
surveyed9.3$FN <- as.character(surveyed9.3$Plot_Name)
surveyed9.3$FN

table.na(surveyed9.3$FN); any(table.na(surveyed9.3$FN)>1)



#SURVEYED9.4
surveyed9.4$FN <- as.character(surveyed9.4$Plot_Name_New)
surveyed9.4$FN

table.na(surveyed9.4$FN); any(table.na(surveyed9.4$FN)>1)



#SURVEYED9.5
surveyed9.5$FN <- as.character(surveyed9.5$Plot_No)
surveyed9.5$FN

table.na(surveyed9.5$FN); any(table.na(surveyed9.5$FN)>1)
#This shapefile only had plot numbers that could be matched with SHI plots in Isabel's dataset (using historic plot names). However, most plots are beyond region 9's limits and fall within region 10. Another shapefile in region 10 contains a duplicate set of the plots that are outside of region 9. However (again), one plot is missing from the region 10 shapefile but was in region 9's shapefile: CRE-0033 (Plot_No = 33)



#SURVEYED10.1
surveyed10.1$FN <- as.character(surveyed10.1$Plot_Name_New)
surveyed10.1$FN

table.na(surveyed10.1$FN); any(table.na(surveyed10.1$FN)>1)



#SURVEYED10.2
surveyed10.2$FN <- as.character(surveyed10.2$Plot_Name)
surveyed10.2$FN

table.na(surveyed10.2$FN); any(table.na(surveyed10.2$FN)>1)



#SURVEYED10.3
surveyed10.3$FN <- as.character(surveyed10.3$Plot_Name_New)
surveyed10.3$FN

table.na(surveyed10.3$FN); any(table.na(surveyed10.3$FN)>1)



#SURVEYED10.4
surveyed10.4$FN <- as.character(surveyed10.4$Plot_Name)
surveyed10.4$FN

table.na(surveyed10.4$FN); any(table.na(surveyed10.4$FN)>1)

#10 plots from this shapefile have nothing resembling a plot name so they were excluded
surveyed10.4 <- surveyed10.4[!is.na(surveyed10.4$FN),]


#SURVEYED10.5
surveyed10.5$FN <- as.character(surveyed10.5$Plot_Name_New)
surveyed10.5$FN

table.na(surveyed10.5$FN); any(table.na(surveyed10.5$FN)>1)



#SURVEYED10.6
surveyed10.6$FN <- as.character(surveyed10.6$Plot_Name)
surveyed10.6$FN

table.na(surveyed10.6$FN); any(table.na(surveyed10.6$FN)>1)



#SURVEYED11
surveyed11$FN <- as.character(surveyed11$Plot_Name)
surveyed11$FN

table.na(surveyed11$FN); any(table.na(surveyed11$FN)>1)



#SURVEYED12
surveyed12$FN <- as.character(surveyed12$Plot_Name_New)
surveyed12$FN

surveyed12$FN[is.na(surveyed12$FN)] <- as.character(surveyed12$Plot_No[is.na(surveyed12$FN)])

surveyed12$FN[surveyed12$FN==""] <- as.character(surveyed12$Plot_No[surveyed12$FN==""])

table.na(surveyed12$FN); any(table.na(surveyed12$FN)>1)




#The great combination
allGIS <- lapply(list(surveyed1,
                      surveyed2.1, surveyed2.2, surveyed2.3, surveyed2.4,
                      surveyed3.1, surveyed3.2,
                      surveyed4.1, surveyed4.1,
                      surveyed5,
                      surveyed6.1, surveyed6.2, surveyed6.3, surveyed6.4, surveyed6.5, surveyed6.6, surveyed6.7,
                      surveyed7.1, surveyed7.2, surveyed7.3, surveyed7.4, surveyed7.5, surveyed7.6,
                      surveyed8,
                      surveyed9.1, surveyed9.2, surveyed9.3, surveyed9.4, surveyed9.5,
                      surveyed10.1, surveyed10.2, surveyed10.3, surveyed10.4, surveyed10.5, surveyed10.6,
                      surveyed11,
                      surveyed12), function(x) x$FN)

allGIS <- data.frame(FN1 = unlist(allGIS), FN2 = unlist(allGIS))
allGIS$FN1 <- as.character(allGIS$FN1)

#Checking plot names. First, remove extra spaces:
allGIS$FN2 <- trimws(allGIS$FN2)
allGIS$FN1 <- trimws(allGIS$FN1)


allGIS$FN2[allGIS$FN2 %!in% bigdata$Standardized_Plot_Name]




unique(bigdata$Standardized_Plot_Name[bigdata$Standardized_Plot_Name %!in% allGIS$FN2])
unique(allprismrapid$plot[allprismrapid$plot %!in% allGIS$FN2])


#SIR plots should have 9 total characters (2-3 zeros)
allGIS$FN2[substr(allGIS$FN1, 1, 3)=="SIR" & nchar(allGIS$FN1)==7] <-
  gsub("-", "-00", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="SIR" & nchar(allGIS$FN1)==7])

allGIS$FN2[substr(allGIS$FN1, 1, 3)=="SIR" & nchar(allGIS$FN1)==6] <-
  gsub("-", "-000", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="SIR" & nchar(allGIS$FN1)==6])



#BAF plots should have 9 total characters (1-3 zeros)
allGIS$FN2[substr(allGIS$FN1, 1, 3)=="BAF" & nchar(allGIS$FN1)==8] <-
  gsub("-", "-0", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="BAF" & nchar(allGIS$FN1)==8])

allGIS$FN2[substr(allGIS$FN1, 1, 3)=="BAF" & nchar(allGIS$FN1)==7] <-
  gsub("-", "-00", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="BAF" & nchar(allGIS$FN1)==7])

allGIS$FN2[substr(allGIS$FN1, 1, 3)=="BAF" & nchar(allGIS$FN1)==6] <-
  gsub("-", "-000", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="BAF" & nchar(allGIS$FN1)==6])

#Plot BAF-18D2 has to be BAF-0008D2 - no other plot has a "2" suffix (that comes from the fact that the plot was moved so renamed with a "2")
allGIS$FN2[allGIS$FN1=="BAF-18D2"] <- "BAF-0008D2"

#Plot BAF-0007D should clearly be BAF-0007B (no other BAF plots)
allGIS$FN2[allGIS$FN1=="BAF-7D"] <- "BAF-0007B"



#MAR plots all need a zero removed
allGIS$FN2[substr(allGIS$FN1, 1, 3)=="MAR" & nchar(allGIS$FN1)==8] <-
  gsub("-0", "-", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="MAR" & nchar(allGIS$FN1)==8])



#Most IGL plots need an additional zero
allGIS$FN2[substr(allGIS$FN1, 1, 3)=="IGL" & nchar(allGIS$FN1)==8] <-
  gsub("-", "-0", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="IGL" & nchar(allGIS$FN1)==8])



#All SILA plots need two additional zeros
allGIS$FN2[substr(allGIS$FN1, 1, 4)=="SILA"] <- 
  gsub("-", "-00", allGIS$FN1[substr(allGIS$FN1, 1, 4)=="SILA"])



#REP plots need 1-2 zeros
allGIS$FN2[substr(allGIS$FN1, 1, 3)=="REP" & nchar(allGIS$FN1)==8] <- 
  gsub("-", "-0", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="REP" & nchar(allGIS$FN1)==8])

allGIS$FN2[substr(allGIS$FN1, 1, 3)=="REP" & nchar(allGIS$FN1)==7] <- 
  gsub("-", "-00", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="REP" & nchar(allGIS$FN1)==7])


#KUG- plots need 2-3 zeros
allGIS$FN2[substr(allGIS$FN1, 1, 4)=="KUG-" & nchar(allGIS$FN1)==7] <- 
  gsub("-", "-00", allGIS$FN1[substr(allGIS$FN1, 1, 4)=="KUG-" & nchar(allGIS$FN1)==7])

allGIS$FN2[substr(allGIS$FN1, 1, 4)=="KUG-" & nchar(allGIS$FN1)==6] <- 
  gsub("-", "-000", allGIS$FN1[substr(allGIS$FN1, 1, 4)=="KUG-" & nchar(allGIS$FN1)==6])


#Certain BAK plots need additional zeros
allGIS$FN2[allGIS$FN1 %in% c("BAK-61B", "BAK-61D", "BAK-63A", "BAK-63D", "BAK-78N")] <-
  gsub("-", "-00", allGIS$FN1[allGIS$FN1 %in% c("BAK-61B", "BAK-61D", "BAK-63A", "BAK-63D", "BAK-78N")])


#HOP plots: need to replace to HOB and remove one zero from JonSurv names
allGIS$FN2[substr(allGIS$FN1, 1, 3)=="HOP"] <-
  gsub("HOP-0", "HOB-", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="HOP"])


#WSC plots with 7 characters need 2 zeros
allGIS$FN2[substr(allGIS$FN1, 1, 3)=="WSC" & nchar(allGIS$FN1)==7] <- 
  gsub("-", "-00", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="WSC" & nchar(allGIS$FN1)==7])

#RAS-0046 and RAS-0047 don't exist in Isabel's DF

#Need to fix VIC-501A and VIC-502JF (VIC-0502 historic name = VIC-502JF)
allGIS$FN2[allGIS$FN1=="VIC-501A"] <- "VIC-0501A"
allGIS$FN2[allGIS$FN1=="VIC-502JF"] <- "VIC-0502"



#SOI plots (from "surveyed9.5") are identified by their Historic_Field_Plot_name_#1
allGIS <- merge(allGIS,
                unique(bigdata[bigdata$Sub_region_name=="Somerset Island", c("Historic_Field_Plot_name_.1", "Standardized_Plot_Name")]),
                all.x = T, all.y = F, by.x = "FN1", by.y = "Historic_Field_Plot_name_.1")
allGIS$FN2[!is.na(allGIS$Standardized_Plot_Name)] <- allGIS$Standardized_Plot_Name[!is.na(allGIS$Standardized_Plot_Name)]



#ARCH plots need two 0
allGIS$FN2[substr(allGIS$FN1, 1, 4)=="ARCH"] <-
  gsub("-", "-00", allGIS$FN1[substr(allGIS$FN1, 1, 4)=="ARCH"])


#The rest of the ARCH plots (82-85) are now considered ARI plots. ARI plots in Isabel's DF have historical plot name = ARCH-XXX (XXX conserved across the name change)
allGIS$FN2[substr(allGIS$FN1, 1, 4)=="ARCH" & substr(allGIS$FN1, 6, 7) %in% as.character(82:85)] <-
  gsub("ARCH-", "ARI-00", allGIS$FN1[substr(allGIS$FN1, 1, 4)=="ARCH" & substr(allGIS$FN1, 6, 7) %in% as.character(82:85)])



#AXE plots need two 0 when nchar=7, and three 0 when nchar=6
allGIS$FN2[substr(allGIS$FN1, 1, 3)=="AXE" & nchar(allGIS$FN1)==7] <- 
  gsub("-", "-00", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="AXE" & nchar(allGIS$FN1)==7])

allGIS$FN2[substr(allGIS$FN1, 1, 3)=="AXE" & nchar(allGIS$FN1)==6] <- 
  gsub("-", "-000", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="AXE" & nchar(allGIS$FN1)==6])



#EGI-0027D is the historic plot name for EGI-0027
allGIS$FN2[allGIS$FN1=="EGI-0027D"] <- "EGI-0027"




#ELL plots: need to replace to ELI and add 1-3 zeros
allGIS$FN2[substr(allGIS$FN1, 1, 3)=="ELL" & nchar(allGIS$FN1)==8] <-
  gsub("ELL-", "ELI-0", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="ELL" & nchar(allGIS$FN1)==8])

allGIS$FN2[substr(allGIS$FN1, 1, 3)=="ELL" & nchar(allGIS$FN1)==7] <-
  gsub("ELL-", "ELI-00", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="ELL" & nchar(allGIS$FN1)==7])

allGIS$FN2[substr(allGIS$FN1, 1, 3)=="ELL" & nchar(allGIS$FN1)==6] <-
  gsub("ELL-", "ELI-000", allGIS$FN1[substr(allGIS$FN1, 1, 3)=="ELL" & nchar(allGIS$FN1)==6])



#No record whatsoever of E-73 or MGP-6047B in Isabel's DF. Strange that MGP-6047A is there.




#E2-1, E2-2 are MDE-0002A and 0002B respectively (based on historic plot names)
allGIS$FN2[allGIS$FN1=="E2-1"] <- "MDE-0002A"
allGIS$FN2[allGIS$FN1=="E2-2"] <- "MDE-0002B"



#M47B, M48B are MGP-6047B and MGP-6048B respectively
allGIS$FN2[allGIS$FN1=="M47B"] <- "MGP-6047B"
allGIS$FN2[allGIS$FN1=="M48B"] <- "MGP-6048B"





#P22N, P22S are QTP-0022A and QTP-0022B respectively
allGIS$FN2[allGIS$FN1=="P22N"] <- "QTP-0022A"
allGIS$FN2[allGIS$FN1=="P22S"] <- "QTP-0022B"






allGIS$FN2[allGIS$FN2 %!in% bigdata$Standardized_Plot_Name & substr(allGIS$FN2, 1, 3)%!in% c("IZO", "HOB", "Tah", "RAS")]
allGIS$FN2[allGIS$FN2 %!in% bigdata$Standardized_Plot_Name & substr(allGIS$FN2, 1, 3)=="TAG"]
unique(allprismrapid$plot[substr(allprismrapid$plot, 1, 3)=="MDE"])
unique(allGIS$FN2[substr(allGIS$FN2, 1, 3)=="TAG"])


#Plots from allprismrapid that are not in allGIS
unique(allprismrapid$plot[allprismrapid$plot %!in% allGIS$FN2])
lunique(allprismrapid$plot[allprismrapid$plot %!in% allGIS$FN2])

#Plot codes of the latter
unique(substr(allprismrapid$plot[allprismrapid$plot %!in% allGIS$FN2], 1, 4))
lunique(substr(allprismrapid$plot[allprismrapid$plot %!in% allGIS$FN2], 1, 4))

#Plots from allGIS that are not in allprismrapid
allGIS$FN2[allGIS$FN2 %!in% bigdata$Standardized_Plot_Name]




#Export list of plots (+ coords) that are missing from Tyler's GIS files-----------------------------------
unique(allprismrapid[allprismrapid$plot %!in% allGIS$FN2, c("plot", "GIS_UTM_ZONE", "UTM_SW_Easting", "UTM_SW_Northing", "UTM_NW_Easting", "UTM_NW_Northing", "UTM_NE_Easting", "UTM_NE_Northing", "UTM_SE_Easting", "UTM_SE_Northing")])


#Checking if any plots for which we need to extract coordinates have more than four corners

#These are the plots from allprismrapid that we need:
unique(allprismrapid$plot[allprismrapid$plot %!in% allGIS$FN2])

#Looking at rows in bigdata that correspond to these plots
bigdata$UTM_5_Easting[bigdata$Standardized_Plot_Name %in% allprismrapid$plot[allprismrapid$plot %!in% allGIS$FN2]]

all(is.na(bigdata$UTM_5_Easting[bigdata$Standardized_Plot_Name %in% allprismrapid$plot[allprismrapid$plot %!in% allGIS$FN2]]))

#All plots from this subset have NA for the coordinates of plot corner #5 - good!


write.csv(unique(bigdata[bigdata$Standardized_Plot_Name %in% allprismrapid$plot[allprismrapid$plot%!in%allGIS$FN2],c("Standardized_Plot_Name", "GIS_UTM_1_zone", "UTM_1_Easting", "UTM_1_Northing", "UTM_1_Type", "GIS_UTM_2_zone", "UTM_2_Easting", "UTM_2_Northing", "UTM_.2_Type", "GIS_UTM_3_zone", "UTM_3_Easting", "UTM_3_Northing", "UTM_3_Type", "GIS_UTM_4_zone", "UTM_4_Easting", "UTM_4_Northing", "UTM_4_Type")]),
          file = "Z/missingGISplots.csv", row.names = FALSE)

########################

#Adding my own checking onto what Laurent did

check <- bigdata %>%
  filter(Region_code == 3) %>%
  filter(Year == 2019)


#######################

#thigns I want to use


match <- unique(allprismrapid$plot[allprismrapid$plot%in%allGIS$FN2])
dontmatch <- unique(allprismrapid$plot[allprismrapid$plot%!in%allGIS$FN2])

saveRDS(match, "match.RDS")
saveRDS(dontmatch, "dontmatch.RDS")



