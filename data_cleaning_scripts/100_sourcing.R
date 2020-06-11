# sources sequences of scripts

# source("data_cleaning_scripts/")

source("data_cleaning_scripts/101_library.R")
source("data_cleaning_scripts/102_PRISM_AbridgedTables.R")
source("data_cleaning_scripts/103_GISshapefiles.R")
source("data_cleaning_scripts/104_Grouping_PRISM_CoordinateStatus.R")
source("data_cleaning_scripts/105_Finalizing_shapefiles_GROUP1.R")
source("data_cleaning_scripts/106_Finalizing_shapefiles_GROUP2.R")
source("data_cleaning_scripts/107_Finalizing_shapefiles_GROUP3.R")
source("data_cleaning_scripts/108_Finalizing_shapefiles__ALL.R")
source("data_cleaning_scripts/109_Finalizing_tables_LongFormat.R")
source("data_cleaning_scripts/110_Finalizing_tables_WideFormat.R")

##confirm if there is anything that shouldn't be earased before using this
rm(list=ls())
