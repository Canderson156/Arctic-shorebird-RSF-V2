plot_coords <- coordsALL2 %>%
  filter(not.na(UTM_4_Easting)) %>%
  filter(not.na(UTM_2_Easting)) %>%
  filter(UTM_2_Type != "perimeter") %>%
  select(-contains("Type")) %>%
  arrange(Plot) %>%
  mutate(PlotX = paste(Plot, 1:length(Plot), sep = "_"), dup = duplicated(Plot))


################# first verion of this script

### Which plots have no coordiantes

coordsMissing<- allsurveys %>%
  filter(is.na(UTM_1_Easting)) %>%
  select(Plot, contains("UTM"), Map_datum) %>%
  distinct()


coordsNoUTM<- allsurveys %>%
  filter(Plot %notin% coordsMissing$Plot) %>%
  filter(is.na(UTM_Zone)) %>%
  select(Plot, contains("UTM"), Map_datum) %>%
  distinct()
#UTM is either 16 or 17. Try both

missingUTM16 <- coordsNoUTM
missingUTM16$UTM_Zone <- 16
missingUTM17 <- coordsNoUTM
missingUTM17$UTM_Zone <- 17
missingUTM <- rbind(missingUTM16, missingUTM17)



### Which plots have records with different coordiantes?

dplots <- data.frame(Plot = allsurveys$Plot[duplicated(allsurveys$Plot)]) %>%
  filter(Plot %notin% coordsMissing$Plot) %>%
  filter(Plot %notin% coordsNoUTM$Plot)

coordsDup <- allsurveys %>%
  filter(Plot %in% dplots$Plot) %>%
  select(Plot, contains("UTM"), Map_datum) %>%
  distinct()

dplots2 <- coordsDup$Plot[duplicated(coordsDup$Plot)]

coordsDup <- coordsDup  %>%
  filter(Plot %in% dplots2) 




### which plots have only one set of coordinates per plot

coordsOne <- allsurveys %>%
  filter(Plot %notin% coordsMissing$Plot) %>%
  filter(Plot %notin% coordsNoUTM$Plot) %>%
  filter(Plot %notin% coordsDup$Plot)%>%
  select(Plot, contains("UTM"), Map_datum) %>%
  distinct()


coordsALL <- rbind(coordsMissing, coordsNoUTM, coordsDup, coordsOne)
# the difference between this and all plots is that 
# this version includes duplicates for plots that have more than one set of coordiantes
# while allplots takes the mean of the duplicates

coordsALL2 <- rbind(coordsDup, coordsOne, missingUTM)
