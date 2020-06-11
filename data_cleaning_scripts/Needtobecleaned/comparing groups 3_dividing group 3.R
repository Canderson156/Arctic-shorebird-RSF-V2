####################################################################################
#subcategories of group 3


#Group A: Plots with duplicate entries

### re do this use the dup column
g3a_duplicated <- allplots2 %>%
  filter(allDuplicated(Plot) == TRUE)

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g3a_duplicated$PlotX)






#group B: Field selected plots


g3b_field_selected <- allplots2 %>%
  filter(Selection_method == "field selected")

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g3b_field_selected$PlotX)

table(allDuplicated(g3b_field_selected$Plot))


#Group C: plots that have no coordinates at all


g3c_no_coords <- allplots2 %>%
  filter(is.na(UTM_1_Easting))


allplots2 <- allplots2 %>%
  filter(PlotX %notin% g3c_no_coords$PlotX)


#Group D: Plots that have more than 4 corners


g3d_perimeter <- allplots2 %>%
  filter(UTM_2_Type == "perimeter")

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g3d_perimeter$PlotX)




#Group E: Plots that only have a single corner

g3e_point <- allplots2 %>% 
  filter(is.na(UTM_2_Northing))


allplots2 <- allplots2 %>%
  filter(PlotX %notin% g3e_point$PlotX)



#Group F: Plots missing one coord

g3f_partial_coords <- allplots2 %>%
  filter(is.na(UTM_2_Easting) | is.na(UTM_4_Easting))

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g3f_partial_coords$PlotX)




#Group G: Normal plots

g3g_normal <- allplots2




nrow(g3a_duplicated) + nrow(g3b_field_selected) + nrow(g3c_no_coords) + nrow(g3d_perimeter) +
  nrow(g3e_point) + nrow(g3f_partial_coords) + nrow(g3g_normal) == nrow(g3_yes_gis)



#Group 6: missing UTM zone
#
#g6_no_zone <- allplots2 %>%
#  filter(is.na(UTM_Zone))
#
#allplots2 <- allplots2 %>%
#  filter(PlotX %notin% g6_no_zone$PlotX)
#
#currently nothing. there are 7 plots in group 1 (no GIS)


