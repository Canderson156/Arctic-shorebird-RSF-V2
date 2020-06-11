###function that turns dataframe into sf object



df_to_stPolygons <- function(dataset, long, lat, grouping_var) {
  output <- dataset %>%
    st_as_sf(coords = c(long, lat)) %>%
    group_by_(grouping_var) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  return(output)
}
