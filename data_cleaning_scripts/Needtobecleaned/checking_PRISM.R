#notes about specific columns


# all of the column names so that I can search for column names, subset by column number etc
cols_bigdata <- data.frame(colnames(bigdata))









#looking at the plots that contained human development

hd <- allplots %>%
  filter(Human_development_details != "not recorded" & Human_development_details != " ")


#How many of the indusry plots have human development details

ind <- allplots %>%
  filter(Survey_Lead == "Industry")










# how many people surveyed each plot? 

table(bigdata$n_surveyors)


#why did some have 0 surveyors
check <- bigdata %>%
  filter(n_surveyors == 0)

table(check$Plot_type)
#they were all intensives


#are any regions missing intensive plots?

intensive <- allplots %>%
  filter(Plot_type == "INTENSIVE")


  


#which plots were surveyed multiple times



####SAME PLOT SAME YEAR
sameplot_sameyear <- allplots %>%
  group_by(Plot, Year) %>%
  add_tally(name = "n_surveys") %>%
  filter(n_surveys > 1)

spsy <- prism %>%
  filter(Plot %in% sameplot_sameyear$Plot) %>%
  select(plot_date, Species, total_birds) %>%
  arrange(Species, plot_date)

spsy_rapid <- sameplot_sameyear %>%
  filter(Plot_type == "RAPID")

spsy_intensive <- sameplot_sameyear %>%
  filter(Plot_type == "INTENSIVE")

spsy_plots <- unique(sameplot_sameyear$Plot)

spsy_full <- bigdata %>%
  filter(Standardized_Plot_Name %in% spsy_plots) %>%
  select(Plot = Standardized_Plot_Name, #2855 unique plots
         Sighting_code_short,
         Survey_Lead,
         Year, #1994-2019
         Month, #mostly June, some July, 475 "not applicable"
         Day,
         Date,
         yday,
         Region_name, #89 plots with NAs that are outside the regions (south of Mackenzie Delta)
         Region_code, #89 plots with NAs that are outside the regions (south of Mackenzie Delta)
         Sub_region_name, #170 plots with NAs. 89 as above. 78 in Foxe basin becasue of differences in how subregions were calculated over time (2019 had no subregions). 3 in North Archipelago are confusing, but may stem from weird subregions that overlap near Alert (see email from Laurent RE Tyler's map of subregions)
         Sub_region_code, #same as above
         Plot_type,
         Survey_method,
         Plot_Shape,
         Quality_1 = GIS_Habitat_Quality_2_Code,
         Quality_2 = GIS_Habitat_Quality_1_Code, # how were these defined, when were they changed, which one is the one we should use, are these being redone?
         Prop_surveyed = Proportion_of_Plot_surveyed, 
         Selection_method = Plot_Selection_Method, # why are there so many field selected plots?
         Plot_area = Plot_area_km2,
         Sighting_code, 
         Species = Standardized_Species_Code,  #XXXX means nothing was observed
         Group,
         Count_Probable_nest,
         Count_Pairs,
         Count_Male,
         Count_Female,
         Count_Unknown_sex)


spsy_full <- prism %>%
  filter(Plot %in% spsy_plots) %>%
  mutate(plot_date = paste(Plot, Date)) %>%
  group_by(plot_date) %>%
  add_tally(total_birds)


ggplot(spsy_full, aes(x = yday, y = n)) +
  geom_point() + 
  facet_wrap(~Region_name)



spsy_r10 <- sameplot_sameyear %>%
  filter(Plot %in% spsy_plots) %>%
  filter(Region_code == 10)



#how many surveyors were there

for(i in 1:nrow(spsy_r10)) {
  spsy_r10$n_surveyors[i] <- count.not.na(c(
    spsy_r10$Surveyor_1_Initials[i],
    spsy_r10$Surveyor_2_Initials[i],
    spsy_r10$Surveyor_3_Initials[i],
    spsy_r10$Surveyor_4_Initials[i], 
    spsy_r10$Surveyor_5_Initials[i],
    spsy_r10$Surveyor_6_Initials[i]))
}

#try doing this to allplots and then retaining it in the proceeding datasets, allows to look at just plot rather than observations
  
table(spsy_r10$n_surveyors)      
           


spsy_r10X <- spsy_r10 %>%
  filter(Sighting_code == 1) %>%
  filter(Survey_method == "rapid") %>%
  select(Plot = Standardized_Plot_Name, #2855 unique plots
       Sighting_code_short,
       Survey_Lead,
       Year, #1994-2019
       Month, #mostly June, some July, 475 "not applicable"
       Day,
       Date,
       yday,
       Region_name, #89 plots with NAs that are outside the regions (south of Mackenzie Delta)
       Region_code, #89 plots with NAs that are outside the regions (south of Mackenzie Delta)
       Sub_region_name, #170 plots with NAs. 89 as above. 78 in Foxe basin becasue of differences in how subregions were calculated over time (2019 had no subregions). 3 in North Archipelago are confusing, but may stem from weird subregions that overlap near Alert (see email from Laurent RE Tyler's map of subregions)
       Sub_region_code, #same as above
       Plot_type,
       Survey_method,
       Plot_Shape,
       Quality_1 = GIS_Habitat_Quality_2_Code,
       Quality_2 = GIS_Habitat_Quality_1_Code, # how were these defined, when were they changed, which one is the one we should use, are these being redone?
       Prop_surveyed = Proportion_of_Plot_surveyed, 
       Selection_method = Plot_Selection_Method, # why are there so many field selected plots?
       Plot_area = Plot_area_km2,
       Sighting_code, 
       Species = Standardized_Species_Code,  #XXXX means nothing was observed
       Group,
       Count_Probable_nest,
       Count_Pairs,
       Count_Male,
       Count_Female,
       Count_Unknown_sex,
       n_surveyors) %>%
  mutate(plot_date = paste(Plot, Date))




test <- bigdata %>%
  filter(Standardized_Plot_Name == "ALE-3840") %>%
  select(1:50)


test <- sameplot_sameyear %>%
  filter(Plot == "ALE-3840")



### SAME PLOT, MULTIPLE YEARS

msurv <- allplots %>%
  group_by(Plot) %>%
  add_tally(name = "n_surveys") %>%
  filter(n_surveys > 1)

years <- msurv %>%
  summarize(n_distinct(Year)) %>%
  rename(n_years = 'n_distinct(Year)')

msurv <- merge(msurv, years)

msurv <- msurv %>%
  filter(n_years > 1)
rm(years)

msurv_industry <- msurv %>%
  filter(Survey_Lead == "Industry")

msurv_intensive <- msurv %>%
  filter(Plot_type == "INTENSIVE")

msurv_rapid <- msurv %>%
  filter(Plot_type == "RAPID") %>%
  filter(Survey_Lead != "Industry")

redone <- msurv %>%
  filter(Year == 2019) %>%
  select(Plot) %>%
  unique()
redone <- as.vector(redone$Plot)  

msurv_rapid <- msurv_rapid %>%
  filter(Plot %notin% redone)

PCI <- msurv_rapid %>%
  filter(Region_code == 3)

msurv_rapid <- msurv_rapid %>%
  filter(Region_code != 3)


# Laurent's function to see which plots were done in different years



checkplots <- function(df, plots, other){
  #df = dataframe
  #plots = character ID of plot column
  #other = character ID of the other column
  blabla <- aggregate(df[,other], by=list(df[,plots]), FUN = function(x) as.character(unique(x)))
  blabla <- blabla[lapply(blabla[,2], FUN = length)>1,]
  return(blabla)
}



test <- checkplots(allplots, "Plot", "Year")















### why were plots field selected?

field<- allplots %>%
  filter(Selection_method == "field selected")


field_industry <- allplots %>%
  filter(Selection_method == "field selected") %>%
  filter(Survey_Lead == "Industry") 

find_sum <- field_industry %>%
  group_by(Sub_region_name) %>%
  summarize(n_distinct(Plot))

field_intensive <- allplots %>%
  filter(Selection_method == "field selected") %>%
  filter(Survey_Lead != "Industry") %>%
  filter(Plot_type == "INTENSIVE")


fint_sum <- field_intensive  %>%
  group_by(Sub_region_name) %>%
  summarize(n_distinct(Plot))



field_rapid <- allplots %>%
  filter(Selection_method == "field selected") %>%
  filter(Survey_Lead != "Industry") %>%
  filter(Plot_type != "INTENSIVE")

fr_sum <- field_rapid  %>%
  group_by(Region_name) %>%
  summarize(n_distinct(Plot))





### why were plots gis selected field modified?





fmgs <- allplots %>%
  filter(Selection_method == "field modified gis selected")


fmgs_sum <- fmgs  %>%
  group_by(Region_name) %>%
  summarize(n_distinct(Plot))








#checking to see if the results seem biased at all by field selection
#create a new column that categorizes into 5 groups:
#gis selected, field modified, field selected industry, field selected intensive, and field selected other
# a version of this is in 02_prep script


#prism <- prism %>%
#  mutate(comparison = Selection_method, 
#         comparison = ifelse(Selection_method == "field selected" & Survey_Lead == "Industry", "field selected - industry", comparison),
#         comparison = ifelse(Selection_method == "field selected" & Plot_type == "INTENSIVE", "field selected - intensive", comparison),
#         comparison = ifelse(comparison == "field selected", "field selected - other", comparison))
                             



#compare prism comparisons to see if counts seem biased
  #if they aren;t, does it matter if the habitats were biased?


# sum total_birds by plot_date
# mean total_birds by plot
# compare the groups to see if they get different results


test <- prism %>%
  group_by(plot_date) %>%
  summarize(sum_birds = sum(total_birds)) %>%
  merge(allplots) %>%
  group_by(Plot) %>%
  summarize(mean_sum_birds = round(mean(sum_birds))) %>%
  merge(allplots)

test$comparison <- relevel(factor(test$comparison), ref = "gis selected")


t_sum <- test %>%
  group_by(comparison) %>%
  summarize(mean_birds = mean(mean_sum_birds), 
            sd_birds = sd(mean_sum_birds), 
            cv_birds = (sd(mean_sum_birds)/mean(mean_sum_birds))*100,
            length(unique(Plot)))


lm_comp <- lm(mean_sum_birds ~ comparison, test)
summary(lm_comp)



# redo but just for shorebirds


test_sb <- prism %>%
  filter(Group == "Shorebirds") %>%
  group_by(plot_date) %>%
  summarize(sum_birds = sum(total_birds)) %>%
  merge(allplots) %>%
  group_by(Plot) %>%
  summarize(mean_sum_birds = round(mean(sum_birds))) %>%
  merge(allplots)

test_sb$comparison <- relevel(factor(test_sb$comparison), ref = "gis selected")


t_sum_sb <- test_sb %>%
  group_by(comparison) %>%
  summarize(mean_birds = mean(mean_sum_birds), 
            sd_birds = sd(mean_sum_birds), 
            #cv_birds = (sd(mean_sum_birds)/mean(mean_sum_birds))*100,
            n_plots = length(unique(Plot)))


lm_comp_sb <- lm(mean_sum_birds ~ comparison, test_sb)
summary(lm_comp_sb)


# what happens when you include habitat quality?



lm_comp2 <- lm(mean_sum_birds ~ comparison + quality, test)
summary(lm_comp2)

lm_comp_sb2 <- lm(mean_sum_birds ~ comparison + quality, test_sb)
summary(lm_comp_sb2)




# what happens when you look at number of surveyors


test$n_surveyors <- relevel(factor(test$n_surveyors), ref = 2)

lm_surv <- lm(mean_sum_birds ~ factor(n_surveyors), test)
summary(lm_surv)


surv_sum <- test %>%
  group_by(n_surveyors) %>%
  summarize(mean_birds = mean(mean_sum_birds), 
            sd_birds = sd(mean_sum_birds), 
            length(unique(Plot)))




test_sb$n_surveyors <- relevel(factor(test_sb$n_surveyors), ref = 2)

lm_surv <- lm(mean_sum_birds ~ n_surveyors, test_sb)
summary(lm_surv)

test_sb$n_surveyors <- relevel(factor(test_sb$n_surveyors), ref = 1)


surv_sum <- test_sb %>%
  group_by(n_surveyors) %>%
  summarize(mean_birds = mean(mean_sum_birds), 
            sd_birds = sd(mean_sum_birds), 
            length(unique(Plot)))




## what are the plots with abnormal plot shapes?


irr <- allplots %>%
  filter(Plot_Shape == "irregular polygon")






##### size


small <- allplots %>%
  filter(Plot_area < 0.09)

large <- allplots %>%
  filter(Plot_area > 0.2)




##proportion surveyed

ps <- allplots %>%
  filter(Prop_surveyed < 0.5)




#checking the field selected comments

rmcols <- which(1:814 %notin% c(1:3, 83:306, 342, 421:814))

fplots <- bigdata %>%
  filter(Standardized_Plot_Name %in% allplots$Plot[allplots$Selection_method != "gis selected"]) %>%
  filter(Survey_Lead != "Industry") %>%
  filter(Plot_type != "INTENSIVE") %>%
  filter(Survey_method == "rapid") %>%
  filter(Plot_type !="RECONNAISSANCE") %>%
  select(rmcols) %>%
  distinct()
  
  
write.csv(fplots, "field_selected_modified.csv", na = "")
#can NAs be exported as blanks?



# what if I just used the GIS ones?
#all: 2540 plots
#only gis selected: 1480 plots
#filed selected or field modified: 1060 plots


test <- allplots %>%
  filter(Selection_method == "gis selected")





#what are the different sighting codes

sighting_codes <- bigdata %>%
  select(Sighting_code, Sighting_code_short) %>%
  distinct()







length(unique(bigdata$Standardized_Plot_Name))

table(bigdata$Year)
table(bigdata$Month) #477 NA


#checking start, finidh, durations
x <- select(bigdata, Start_time_1, Finish_time_1, DERIVED_Survey_Duration..hours.minutes., Field_recorded_Survey_Duration_hours)
# very few have field recorded survey hours
# the dreived durations arn't calculated properly 
# two plots have both start and finish times of 00:00 which seems like an error
# start and finish still have times in two different formats which is annoying

table(is.na(bigdata$Start_time_1))
table(is.na(bigdata$Finish_time_1))



table(bigdata$Region_name)

#what are the plots that don't have a region?

x <- filter(bigdata, is.na(Region_name))

# Note from Tyler that they are outside of the region


table(bigdata$Sub_region_code)

#what are the plots that have a region but no subregion?

x <- filter(bigdata, is.na(Sub_region_code))

table(x$Region_name) #same 89 above that have no region, 78 in foxe basin, 3 in north archipelago



#whats going on in region 10

test <- filter(bigdata, Region_code == 10)
table(test$Year, test$Sub_region_code)
# are the 3 that don't have a subregion code a mistake?









#looking at the structure of the whole bigdata dataset

str(bigdata, list.len=ncol(df))


# all of the column names so that I can ceanf or column names, subset by column number etc
cols_bigdata <- data.frame(colnames(bigdata))



### Looking at subsets of the full dataset


cols <- c(17, 36, 41:42, 45, 331:338, 344, 677:714, 77, 84, 90, 97, 77:302)
cols <- c(313:322)

test <- bigdata %>%
  #filter(!is.na(UTM_44_Easting)) %>%
  select(cols)




#check if all of the UTMS are the same
cols <- c(77, 84, 90, 97)

test <- bigdata %>%
  #filter(!is.na(UTM_44_Easting)) %>%
  select(cols)

#returns true if first value matches all other values, 
#false if atleast one doesn't match,
#NA if there is an NA in any of the values

all.identical <- function(vec){
  all(vec[1] == vec)
} 


test$same <- apply(test, 1, all.identical)


h <- head(prism)





#checking month 


test <- prism %>%
  filter(month == "not applicable")



#looking at relationship between survey method, plot type, and plot shape














#checking species x group combinations


table(bigdata$Standardized_Species_Code[bigdata$Group == "Shorebirds"])




# what are the entires where group == NA
# they are all incidental observatons or habitat descriptions

test <- bigdata %>%
  filter(is.na(Group))




#checking if species and groups match up

table(bigdata$Standardized_Species_Code[bigdata$Group == "Shorebirds"])
table(bigdata$Standardized_Species_Code[bigdata$Group == "Cranes"])
table(bigdata$Standardized_Species_Code[bigdata$Group == "Grouse"])
table(bigdata$Standardized_Species_Code[bigdata$Group == "Mammal"])
table(bigdata$Standardized_Species_Code[bigdata$Group == "Owls"])
table(bigdata$Standardized_Species_Code[bigdata$Group == "Passerines"])
table(bigdata$Standardized_Species_Code[bigdata$Group == "Waterfowl"])
table(bigdata$Standardized_Species_Code[bigdata$Group == "Gull"])







#### looking for rows where one of the counts seems like it should be there but it is an NA

b2 <- mutate(bigdata, total_birds = (Count_Nests_found*2) + (Count_Probable_nest*2) + (Count_Pairs*2) + Count_Male + Count_Female + Count_Unknown_sex)
b2 <- b2 %>%
  filter(is.na(total_birds)) %>%
  filter(Sighting_code == 1) %>%
  filter(Survey_method == "rapid")



#looking to see which columns I need to change for the plot where I eeded to create the sighting code = 1 row

test <- vector()
for(i in 1:ncol(add)) {
  test[i] <- identical(add[1,i], add[2,i])
}


z <- which(test == FALSE)
check <- add[,z]













#### Checking functions created by Laurent

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
  temp <- checkplots(bigdata[bigdata$Plot_type=="bigdata",], i, "Sub_region_name", "Standardized_Plot_Name", "Date", "Year")
  mult_survey <- append(mult_survey, temp, after = length(mult_survey))
}

pbp <- bigdata[bigdata$Protected_Area_1_Name=="Polar Bear Pass-Nanuit Itillinga National Wildlife Area",]
pbp$Year <- as.factor(as.character(pbp$Year))
pbp$Standardized_Plot_Name <- as.factor(as.character(pbp$Standardized_Plot_Name))
table(pbp$Year, pbp$Standardized_Plot_Name)
which(bigdata$Standardized_Plot_Name %in% "PBP")