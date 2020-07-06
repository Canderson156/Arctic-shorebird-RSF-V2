#basic modelling







#REPH:june temp

reph_june_temp <- lm(presence_reph ~ june_temp_LCC + poly(june_temp_LCC,2), reph_data)
summary(reph_june_temp)



#REPH:July temp


reph_july_temp <- lm(presence_reph ~ july_temp_LCC, reph_data)
summary(reph_july_temp)



#REPH:landcover 1

reph_lc1 <- lm(presence_reph ~ tussock_graminoid_1, reph_data)
summary(reph_lc1)



#REPH:landcover 2

reph_lc2 <- lm(presence_reph ~ wet_sedge_2, reph_data)
summary(reph_lc2)


#REPH:landcover 3

reph_lc3 <- lm(presence_reph ~ nontussock_graminoid_3, reph_data)
summary(reph_lc3)


#REPH:landcover 2

reph_lc4 <- lm(presence_reph ~ prostrate_shrub_4, reph_data)
summary(reph_lc4)

#REPH:landcover 11
reph_lc11 <- lm(presence_reph ~ wetlands_11, reph_data)
summary(reph_lc11)



#REPH:landcover gram

reph_gram <- lm(presence_reph ~ graminoid_group, reph_data)
summary(reph_gram)



#REPH: temp + landcover

reph_2 <- glm(presence_reph ~ june_temp_LCC + wet_sedge_2 , reph_data, family = binomial)
summary(reph_2)

reph_2 <- glm(presence_reph ~ june_temp_LCC + (june_temp_LCC^2) + wet_sedge_2 + (wet_sedge_2^2), reph_data, family = binomial)
summary(reph_2)

reph_3 <- lm(presence_reph ~ june_temp_LCC * sqrt(wet_sedge_2), reph_data)
summary(reph_3)

