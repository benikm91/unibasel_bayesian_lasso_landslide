# ##########################
#
#    CROP Variable datasets
#
# ##########################

###  Load Packages

#library(sf)
#library(dplyr)

#install.packages('raster')
#library("raster", lib="~/local/R_libs/")
library(raster)

#install.packages('rgdal')
library(rgdal)
#library("rgdal", lib="~/local/R_libs/")

#library(ggplot2)
#library(rgeos)#

#install.packages('RColorBrewer')
#library("RColorBrewer", lib="~/local/R_libs/")
#library(RColorBrewer)

#install.packages('spatstat')
#library("spatstat", lib="~/local/R_libs/")
library(spatstat)
library(spatial)
#library("spatial", lib="~/local/R_libs/")

# ...............................................


#PATH <- "/Volumes/Lauren_Uni"
PATH <- "/scicore/home/alewell/zwelau00/SSGM/R_output_resample_raster_ALL_LAYERS"

# Load Study Site Regions
Arosa <- readOGR(paste(PATH,"/Study_Sites/Outlines/Arosa.shp", sep=''))
Baulmes <- readOGR(paste(PATH,"/Study_Sites/Outlines/Baulmes.shp", sep=''))
Chrauchtal <- readOGR(paste(PATH,"/M3_SSGM/Study_Site_Shapefiles/Outlines/Chrauchtal.shp", sep=''))
Hornbach <- readOGR(paste(PATH,"/Study_Sites/Outlines/Hornbach_Valley.shp", sep=''))
Rappetal <- readOGR(paste(PATH,"/Study_Sites/Outlines/Rappetal.shp", sep=''))
Turbach <- readOGR(paste(PATH,"/Study_Sites/Outlines/Turbach.shp", sep=''))
Urseren <- readOGR(paste(PATH,"/Study_Sites/Outlines/Urseren_Valley.shp", sep=''))
Val_Cluozza <- readOGR(paste(PATH,"/Study_Sites/Outlines/Val_Cluozza.shp", sep=''))
Val_D_Entremont <- readOGR(paste(PATH,"/Study_Sites/Outlines/Val_D_Entremont.shp", sep=''))
Val_Piora <- readOGR(paste(PATH,"/Study_Sites/Outlines/Val_Piora.shp", sep=''))

# Use Aspect_East to match origin, projection, resolution, extent:
Aspect_East <- raster(paste(PATH,"/Static/Aspect_EAST_matched_LV95_CH.tif", sep=''))
CH_border <- raster(paste(PATH,"/CH_border/CH_border_rast_WGS84.tif", sep=''))

# # CRS projection specification LV95
projection_LV95 <- "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs"

# 
CH_border_outline_LV95 <- readOGR(paste(PATH,"/CH_border/CH_outline_LV95.shp", sep=''))
# ................................................ CHELSA PRECIPITATION ....................
# 
#### CHANGE ONLY THIS VARIABLE !
START_YEAR <- 2009 
END_YEAR <- 2013

sum <- 0
Counter <- 0

for (year in START_YEAR:END_YEAR){
  
  Frost_Change_Frequency <- raster(paste(PATH,"/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_FCF_",year,"_V1.2.1.tif", sep=''))
  projection_WGS84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  # # Change crs projection of CHELSA DEC layer
  if (is.na(projection(Frost_Change_Frequency))) {
    crs(Frost_Change_Frequency) <- projection_WGS84
  } else {
    Frost_Change_Frequency <- projectRaster(Frost_Change_Frequency, crs=projection_WGS84)
  }
  projection(Frost_Change_Frequency)
  
  # # crop Chelsa dataset with CH boundary layer with WGS84 projection (this will not crop to exact shape, just extent of rectangle)
  Frost_Change_Frequency_CH <- crop(Frost_Change_Frequency, CH_border, snap='near')
  
  projection_LV95 <- "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs"
  
  #projection(Frost_Change_Frequency)
  #data(Frost_Change_Frequency)
  #origin(Frost_Change_Frequency)
  #extent(Frost_Change_Frequency)
  
  # # Change crs projection of CHELSA DEC layer
  if (is.na(projection(Frost_Change_Frequency_CH))) {
    crs(Frost_Change_Frequency_CH) <- projection_LV95
  } else {
    Frost_Change_Frequency_CH <- projectRaster(Frost_Change_Frequency_CH, crs=projection_LV95)
  }
  projection(Frost_Change_Frequency_CH)
  
  # ................................................ cropping layer to CH boundary ...........................
  # # Crop elevation data by extent of state subset
  Frost_Change_Frequency_CH <- mask(Frost_Change_Frequency_CH, CH_border_outline_LV95)
  Frost_Change_Frequency_CH <- crop(Frost_Change_Frequency_CH, extent(CH_border_outline_LV95))
  
  sum <- sum + Frost_Change_Frequency_CH
  Counter <- Counter + 1
}

Frost_Change_Frequency_CH_mean <- sum/Counter

Frost_Change_Frequency_CH_mean <- resample(Frost_Change_Frequency_CH_mean, Aspect_East, method='bilinear')

#origin(Chelsa_Winter_mean_matched) <- origin (Aspect_East)
origin(Frost_Change_Frequency_CH_mean) <- origin (Aspect_East)

writeRaster(Frost_Change_Frequency_CH_mean, paste(PATH,"/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_LV95_CH_FCF_mean_",START_YEAR,END_YEAR,".tif",sep=''), format="GTiff", overwrite=TRUE)


# 
# 
# # # crop Winter sum dataset with climate regions
# # AROSA
# Frost_Change_Frequency_CH_mean_Arosa <- mask(Frost_Change_Frequency_CH_mean, Arosa)
# Frost_Change_Frequency_CH_mean_Arosa <- crop(Frost_Change_Frequency_CH_mean_Arosa, extent(Arosa))
# 
# writeRaster(Frost_Change_Frequency_CH_mean_Arosa, paste(PATH,"/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_LV95_CH_FCF_mean_Arosa_",START_YEAR,END_YEAR,".tif",sep=''), format="GTiff", overwrite=TRUE)
# 
# FCF_Mean_Arosa <- cellStats(Frost_Change_Frequency_CH_mean_Arosa, "mean")
# 
# # BAULMES
# Frost_Change_Frequency_CH_mean_Baulmes <- mask(Frost_Change_Frequency_CH_mean, Baulmes)
# Frost_Change_Frequency_CH_mean_Baulmes <- crop(Frost_Change_Frequency_CH_mean_Baulmes, extent(Baulmes))
# 
# writeRaster(Frost_Change_Frequency_CH_mean_Baulmes, paste(PATH,"/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_LV95_CH_FCF_mean_Baulmes_",START_YEAR,END_YEAR,".tif",sep=''), format="GTiff", overwrite=TRUE)
# 
# FCF_Mean_Baulmes <- cellStats(Frost_Change_Frequency_CH_mean_Baulmes, "mean")
# 
# # CHRAUCHTAL
# Frost_Change_Frequency_CH_mean_Chrauchtal <- mask(Frost_Change_Frequency_CH_mean, Chrauchtal)
# Frost_Change_Frequency_CH_mean_Chrauchtal <- crop(Frost_Change_Frequency_CH_mean_Chrauchtal, extent(Chrauchtal))
# 
# writeRaster(Frost_Change_Frequency_CH_mean_Chrauchtal, paste(PATH,"/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_LV95_CH_FCF_mean_Chrauchtal_",START_YEAR,END_YEAR,".tif",sep=''), format="GTiff", overwrite=TRUE)
# 
# FCF_Mean_Chrauchtal <- cellStats(Frost_Change_Frequency_CH_mean_Chrauchtal, "mean")
# 
# # HORNBACH
# Frost_Change_Frequency_CH_mean_Hornbach <- mask(Frost_Change_Frequency_CH_mean, Hornbach)
# Frost_Change_Frequency_CH_mean_Hornbach <- crop(Frost_Change_Frequency_CH_mean_Hornbach, extent(Hornbach))
# 
# writeRaster(Frost_Change_Frequency_CH_mean_Hornbach, paste(PATH,"/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_LV95_CH_FCF_mean_Hornbach_",START_YEAR,END_YEAR,".tif",sep=''), format="GTiff", overwrite=TRUE)
# 
# FCF_Mean_Hornbach <- cellStats(Frost_Change_Frequency_CH_mean_Hornbach, "mean")
# 
# # RAPPETAL
# Frost_Change_Frequency_CH_mean_Rappetal <- mask(Frost_Change_Frequency_CH_mean, Rappetal)
# Frost_Change_Frequency_CH_mean_Rappetal <- crop(Frost_Change_Frequency_CH_mean_Rappetal, extent(Rappetal))
# 
# writeRaster(Frost_Change_Frequency_CH_mean_Rappetal, paste(PATH,"/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_LV95_CH_FCF_mean_Rappetal_",START_YEAR,END_YEAR,".tif",sep=''), format="GTiff", overwrite=TRUE)
# 
# FCF_Mean_Rappetal <- cellStats(Frost_Change_Frequency_CH_mean_Rappetal, "mean")
# 
# # TURBACH
# Frost_Change_Frequency_CH_mean_Turbach <- mask(Frost_Change_Frequency_CH_mean, Turbach)
# Frost_Change_Frequency_CH_mean_Turbach <- crop(Frost_Change_Frequency_CH_mean_Turbach, extent(Turbach))
# 
# writeRaster(Frost_Change_Frequency_CH_mean_Turbach, paste(PATH,"/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_LV95_CH_FCF_mean_Turbach_",START_YEAR,END_YEAR,".tif",sep=''), format="GTiff", overwrite=TRUE)
# 
# FCF_Mean_Turbach <- cellStats(Frost_Change_Frequency_CH_mean_Turbach, "mean")
# 
# # URSEREN
# Frost_Change_Frequency_CH_mean_Urseren <- mask(Frost_Change_Frequency_CH_mean, Urseren)
# Frost_Change_Frequency_CH_mean_Urseren <- crop(Frost_Change_Frequency_CH_mean_Urseren, extent(Urseren))
# 
# writeRaster(Frost_Change_Frequency_CH_mean_Urseren, paste(PATH,"/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_LV95_CH_FCF_mean_Urseren_",START_YEAR,END_YEAR,".tif",sep=''), format="GTiff", overwrite=TRUE)
# 
# FCF_Mean_Urseren <- cellStats(Frost_Change_Frequency_CH_mean_Urseren, "mean")
# 
# # VAL CLUOZZA
# Frost_Change_Frequency_CH_mean_Cluozza <- mask(Frost_Change_Frequency_CH_mean, Val_Cluozza)
# Frost_Change_Frequency_CH_mean_Cluozza <- crop(Frost_Change_Frequency_CH_mean_Cluozza, extent(Val_Cluozza))
# 
# writeRaster(Frost_Change_Frequency_CH_mean_Urseren, paste(PATH,"/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_LV95_CH_FCF_mean_Cluozza_",START_YEAR,END_YEAR,".tif",sep=''), format="GTiff", overwrite=TRUE)
# 
# FCF_Mean_Cluozza <- cellStats(Frost_Change_Frequency_CH_mean_Cluozza, "mean")
# 
# # VAL D'ENTREMONT
# Frost_Change_Frequency_CH_mean_Entremont <- mask(Frost_Change_Frequency_CH_mean, Val_D_Entremont)
# Frost_Change_Frequency_CH_mean_Entremont <- crop(Frost_Change_Frequency_CH_mean_Entremont, extent(Val_D_Entremont))
# 
# writeRaster(Frost_Change_Frequency_CH_mean_Entremont, paste(PATH,"/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_LV95_CH_FCF_mean_Entremont_",START_YEAR,END_YEAR,".tif",sep=''), format="GTiff", overwrite=TRUE)
# 
# FCF_Mean_Entremont <- cellStats(Frost_Change_Frequency_CH_mean_Entremont, "mean")
# 
# # VAL PIORA
# Frost_Change_Frequency_CH_mean_Piora <- mask(Frost_Change_Frequency_CH_mean, Val_Piora)
# Frost_Change_Frequency_CH_mean_Piora <- crop(Frost_Change_Frequency_CH_mean_Piora, extent(Val_Piora))
# 
# writeRaster(Frost_Change_Frequency_CH_mean_Piora, paste(PATH,"/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_LV95_CH_FCF_mean_Piora_",START_YEAR,END_YEAR,".tif",sep=''), format="GTiff", overwrite=TRUE)
# 
# FCF_Mean_Piora <- cellStats(Frost_Change_Frequency_CH_mean_Piora, "mean")
# 
# 
# 
# ### MEAN Regions
# output <- matrix(ncol=10, nrow=1)
# 
# output[1,] <- c(FCF_Mean_Arosa, FCF_Mean_Baulmes, FCF_Mean_Chrauchtal, FCF_Mean_Hornbach, FCF_Mean_Rappetal, FCF_Mean_Turbach, FCF_Mean_Urseren, FCF_Mean_Cluozza, FCF_Mean_Entremont, FCF_Mean_Piora)
# 
# colnames(output) <- c( "Arosa", "Baulmes", "Chrauchtal", "Hornbach", "Rappetal", "Turbach", "Urseren", "Val Cluozza", "Val D'Entremont", "Val Piora")
# 
# output_dataframe <- as.data.frame(output)
# 
# 
# 
# write.csv(output_dataframe, file = paste(PATH,"/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_LV95_CH_FCF_studysites_19832013.csv",sep=''))
# write.table(output_dataframe, file=paste(PATH,"/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_LV95_CH_FCF_studysites_19832013.txt",sep=''), append = FALSE, sep = ",", dec = ".")
