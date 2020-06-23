#----------------------------------------------------------------------------------------------------------#

# This script was written by Aaron Kamoske - aaron.kamoske@usda.gov - June, 2020

# In order to run the script, change the folder paths to the correct location (e.g. where the .las files are),
# change the projection as needed, and change the output paths.

#----------------------------------------------------------------------------------------------------------#



# load packages
library(lidR)
library(raster)

# list all the files
naip.files <- list.files("D:/New_Hampshire_Lidar/NAIP_Point_Cloud/All_NAIP_Tiles/", full.names = TRUE)
lidar.files <- list.files("D:/New_Hampshire_Lidar/Lidar_Data/All_Lidar_Tiles/", full.names = TRUE)

# record the names
naip.names <- tools::file_path_sans_ext(list.files("D:/New_Hampshire_Lidar/NAIP_Point_Cloud/All_NAIP_Tiles/"))
lidar.names <- tools::file_path_sans_ext(list.files("D:/New_Hampshire_Lidar/Lidar_Data/All_Lidar_Tiles/"))

# loop through the NAIP files and make hillshades
for (i in 1:length(naip.files)) {
  
  # read in the file
  las <- readLAS(naip.files[i],
                 select = "xyz")
  
  # make a canopy height model
  chm <- grid_canopy(las, 
                     res = 1, 
                     p2r(0.2))
  
  # fix the projection issue
  crs(chm) <- "+proj=utm +zone=19 +ellps=GRS80 +units=m +no_defs"
  
  # make a slope raster
  slope <- terrain(chm, opt = "slope")
  
  # make an aspect raster
  aspect <- terrain(chm, opt = "aspect")
  
  # make a hillshade
  chm.hill <- hillShade(slope,
                        aspect,
                        angle = 40,
                        direction = 270)
  
  # take a look at it
  plot(chm.hill,
       col = grey.colors(100, start = 0, end = 1), legend = F)
  
  # write raster to disc
  writeRaster(chm.hill,
              filename = paste0("D:/New_Hampshire_Lidar/Hillshade/NAIP/", naip.names[i], ".tif"),
              format = "GTiff",
              overwrite = TRUE,
              NAflag = -9999)
  
  # book keeping
  print(i)
}

# loop through the lidar files and make hillshades
for (i in 1:length(lidar.files)) {
  
  # read in the file
  las <- readLAS(lidar.files[i],
                 select = "xyz")
  
  # make a canopy height model
  chm <- grid_canopy(las, 
                     res = 1, 
                     p2r(0.2))
  
  # fix the projection issue
  crs(chm) <- "+proj=utm +zone=19 +ellps=GRS80 +units=m +no_defs"
  
  # make a slope raster
  slope <- terrain(chm, opt = "slope")
  
  # make an aspect raster
  aspect <- terrain(chm, opt = "aspect")
  
  # make a hillshade
  chm.hill <- hillShade(slope,
                        aspect,
                        angle = 40,
                        direction = 270)
  
  # take a look at it
  plot(chm.hill,
       col = grey.colors(100, start = 0, end = 1), legend = F)
  
  # write raster to disc
  writeRaster(chm.hill,
              filename = paste0("D:/New_Hampshire_Lidar/Hillshade/Lidar/", lidar.names[i], ".tif"),
              format = "GTiff",
              overwrite = TRUE,
              NAflag = -9999)
  
  # book keeping
  print(i)
}
