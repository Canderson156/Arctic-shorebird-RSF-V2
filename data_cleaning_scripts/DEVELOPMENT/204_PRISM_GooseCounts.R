### Adding goose data ###########################################################################################################################


geese_sp <- c("CAGO", "SNGO", "ROGO", "CACG", "GWFG")

geese_plots <- included_surveys %>%
  filter(Species %in% geese_sp) %>%
  select(Plot,
         plot_date,
         plot_year,
         total_birds) %>%
  group_by(plot_date) %>% 
  mutate(sum_geese_survey = sum(total_birds)) %>%
  ungroup() %>%
  select(-plot_date, -total_birds) %>%
  distinct() %>%
  group_by(plot_year) %>%
  mutate(mean_geese_year = mean(sum_geese_survey)) %>%
  ungroup() %>%
  select(-plot_year, -sum_geese_survey) %>%
  distinct() %>%
  group_by(Plot) %>% 
  mutate(mean_geese = round(mean(mean_geese_year))) %>%
  ungroup() %>%
  select(-mean_geese_year) %>%
  distinct()


#would I rather add this to the sb object that has individual species rather than summaries for all shorebirds? 
SB <- SB %>%
  merge(geese_plots, all = TRUE) %>%
  mutate(mean_geese = ifelse(is.na(mean_geese), 0, mean_geese))



