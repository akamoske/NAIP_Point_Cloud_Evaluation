#----------------------------------------------------------------------------------------------------------#

# This script was written by Aaron Kamoske - aaron.kamoske@usda.gov - June, 2020

# In order to run the script, change the folder paths to the correct location (e.g. where the .las files are),
# change the projection as needed, and change the output paths.

#----------------------------------------------------------------------------------------------------------#


# load packages
library(lidR)
library(raster)

###### create DEMs from the .las point clouds #####

# list the raster files and then again for just their names so we can use that later
las.files <- list.files("D:/New_Hampshire_Lidar/Lidar_Data/All_Lidar_Tiles/", full.names = TRUE)
las.names <- tools::file_path_sans_ext(list.files("D:/New_Hampshire_Lidar/Lidar_Data/All_Lidar_Tiles/"))

# loop through the files and create a DEM for each, then write to disc
for (i in 1:length(las.files)) {
  las <- readLAS(las.files[i])
  dem <- grid_terrain(las, res = 1, algorithm = tin())
  crs(dem) <- "+proj=utm +zone=19 +ellps=GRS80 +units=m +no_defs"
  writeRaster(dem,
              filename = paste0("D:/New_Hampshire_Lidar/DEMs/Lidar/",
                                las.names[i],
                                ".tif"))
  print(i)
}