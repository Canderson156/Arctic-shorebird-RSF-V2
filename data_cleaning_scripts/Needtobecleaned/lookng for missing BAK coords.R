
# file paths of all the tracks files

filelist <- list.files("E:\\Christine_temp\\Arctic shorebird RSF\\Version 2 - Manuscript\\data\\PRISM\\2008 track files")


subset(ogrDrivers(), grepl("GDB", name))  # I have no idea what this does, got it from this internet

alltracks <- ogrListLayers("E:\\Christine_temp\\Arctic shorebird RSF\\Version 2 - Manuscript\\data\\PRISM\\2008 track files\\13 june.gdb")





filelist <- list.files("E:\\2019 PRISM GIS v2\\")


subset(ogrDrivers(), grepl("GDB", name))  # I have no idea what this does, got it from this internet

alltracks <- ogrListLayers("E:\\2019 PRISM GIS v2\\Region1_Cleaned.gdb")











filegdblist <- paste("E:\\2019 PRISM GIS v2\\", list.files("E:\\2019 PRISM GIS v2"), sep = "")

#re-order so that regions are sequential
filegdblist <- filegdblist[c(1,5:12,2:4)] 


#create a list of all of the objects inseide those geodatabases


subset(ogrDrivers(), grepl("GDB", name))  # I have no idea what this does, got it from this internet

fc_list <- lapply(filegdblist, ogrListLayers)



alltrackspoints <- st_read("E:\\Christine_temp\\Arctic shorebird RSF\\Version 2 - Manuscript\\data\\PRISM\\2008 track files\\all tracks points.shp")

alltrackspoly <- st_read("E:\\Christine_temp\\Arctic shorebird RSF\\Version 2 - Manuscript\\data\\PRISM\\2008 track files\\all tracks polylines.shp")

alltracksgpx <- st_read("E:\\Christine_temp\\Arctic shorebird RSF\\Version 2 - Manuscript\\data\\PRISM\\2008 track files\\all tracks.gpx", "tracks")

alltracksgpx <- alltracksgpx %>%
  filter(not.na(cmt)) %>%
  filter(name != "Arv")
