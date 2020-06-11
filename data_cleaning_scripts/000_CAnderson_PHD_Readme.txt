# Christine Anderson PHD Thesis

100 series: Clean PRISM coordinates

	101 - Load packages, functions, coordinate systems, raw PRISM data

	102 - Condensed versions of the full dataset that I use for various analyses
		R objects: bigdata, prism, allplots

	103 - Load raw regional GIS shapefiles from Tyler (2019, some plots missing). 
		Standardize plot names and coordinate system. Merge together. 
		R objects: GIS_shapefiles, Alert_shapefiles

	104 - Organinzing PRISM plots into groups that need to have their coordinates treated differently
		R objects: g0, g1, g2, g2a, g2b, g3

	105 - Finalizing coordinates for group 1 (shapefiles)
		R objects: good_g1

	106 - Finalizing coordinates for group 2 (shapefiles)
		R objects: good_g2, good_g2a, good_g2b

	107 - Finalizing coordinates for group 3 (shapefiles)
		R objects: good_g3_gis, 3 different versions of good_g3a_alert

	108 - Exporting finalized coordinates as shapefiles
		Exported: all_polygons.shp, all_points.shp
		R objects: all_polygons, all_points, all_coords

	109 - Exporting finalized coordinates as csv - long format (one row per coordinate)
		Exported: all_coordsLONG.csv

	110 - Exporting finalized coordinates as csv - wide format (one row per plot)
		Exported: all_coordsWIDE.csv

