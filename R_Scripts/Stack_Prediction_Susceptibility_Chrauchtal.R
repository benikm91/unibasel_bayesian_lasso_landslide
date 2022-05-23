# Build Raster Stack for Susceptibility map predictions

library(rgdal)
#library(ggplot2)
library(spatstat)
library(spatial)
library(sf)
library(raster)
library(dplyr)
library(plyr)
#library(mlr)
#library(parallelMap)
#library(RSAGA)
#library(rgrass7)
#library(pROC)
library(RColorBrewer)
#library(rasterVis)


# 2013 Turbach, Urseren, Val_D_Entremont
# 2014 Arosa, Baulmes, Chrauchtal
# 2015 Hornbach, Rappetal, Val_Cluozza, Val_Piora

# depending on the Location for Precipitation 10Y and 5Y different time frames need to be chose. For other variables from CEHLSA
# (e.g. snow days, there is no possibility to change time frame because dataset only goes to 2013)

set.seed(1)

Site <- "Chrauchtal"
# outline <- readOGR(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/Outlines/",Site,".shp",sep=""))
# plot(outline)

##### add DEM values

elevation <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Elevation/DEM_2m_",Site,".tif",sep=""))
elevation <- scale(elevation)
slope <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Slope/Slope_2m_",Site,".tif",sep=""))
slope <- scale(slope)
aspect <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Aspect/Aspect_2m_",Site,".tif",sep=""))
twi <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/TWI/TWI_2m_",Site,".tif",sep=""))
twi <- scale(twi)
flowacc <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Flow/FlowAcc_2m_",Site,".tif",sep=""))
flowacc <- scale(flowacc)
#flowdir <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Flow/FlowDir_2m_",Site,".tif",sep=""))
curvature_plan <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Curvature/Curvature_plan_2m_",Site,".tif",sep=""))
curvature_plan <- scale(curvature_plan)
curvature_profile <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Curvature/Curvature_profile_2m_",Site,".tif",sep=""))
curvature_profile <- scale(curvature_profile)
#curvature_pp <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Curvature/Curvature_plan_and_profile_2m_",Site,".tif",sep=""))
roughness <- terrain(elevation, opt="roughness")
roughness <- scale(roughness)



# These layers are in CH format and need asjusting to match study site extent and resolution (projection is correct)

dist_t_Roads <- raster("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Distance/CH_Distance_To_Roads_10m.tif")
dist_t_Streams <- raster("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Distance/CH_Distance_To_Streams_10m.tif")

density_Roads <- raster("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Density/CH_Density_Roads_500m.tif")
density_Streams <- raster("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Density/CH_Density_Streams_500m.tif")


# CROP to STUDY SITE
# use elevation as basis for reconfiguration of layers with CH size and wrong spatial resolution

# RESIZE PIXELS to 2m for susceptibility maps

# CHECK and FORCE - origin, extent, projection, data

dist_t_Roads_matched <- resample(dist_t_Roads, elevation, method='bilinear')
dist_t_Roads_matched <- mask(dist_t_Roads_matched, elevation)
dist_t_Roads <- crop(dist_t_Roads_matched, extent(elevation))
dist_t_Roads <- scale(dist_t_Roads)

dist_t_Streams_matched <- resample(dist_t_Streams, elevation, method='bilinear')
dist_t_Streams_matched <- mask(dist_t_Streams_matched, elevation)
dist_t_Streams <- crop(dist_t_Streams_matched, extent(elevation))
dist_t_Streams <- scale(dist_t_Streams)

density_Roads_matched <- resample(density_Roads, elevation, method='bilinear')
density_Roads_matched <- mask(density_Roads_matched, elevation)
density_Roads <- crop(density_Roads_matched, extent(elevation))
density_Roads <- scale(density_Roads)

density_Streams_matched <- resample(density_Streams, elevation, method='bilinear')
density_Streams_matched <- mask(density_Streams_matched, elevation)
density_Streams <- crop(density_Streams_matched, extent(elevation))
density_Streams <- scale(density_Streams)


# CHELSA DATASET
# !!!! ADAPT LOWER to right years as well !!!!

#Chelsa_stack_10Y_20042013 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Stack_CHELSA_LV95_CH_fullsize_20042013_allmonths.tif")
Chelsa_stack_10Y_20052014 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Stack_CHELSA_LV95_CH_fullsize_20052014_allmonths.tif")
#Chelsa_stack_10Y_20062015 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Stack_CHELSA_LV95_CH_fullsize_20062015_allmonths.tif")

#Chelsa_max_10Y_20042013 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Max_CHELSA_LV95_CH_fullsize_20042013_allmonths.tif")
Chelsa_max_10Y_20052014 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Max_CHELSA_LV95_CH_fullsize_20052014_allmonths.tif")
#Chelsa_max_10Y_20062015 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Max_CHELSA_LV95_CH_fullsize_20062015_allmonths.tif")

#names(Chelsa_max_10Y_20042013)[names(Chelsa_max_10Y_20042013)=="Max_CHELSA_LV95_CH_fullsize_20042013_allmonths"] <- "Chelsa_10Y_Max"
#names(Chelsa_max_10Y_20042013)[names(Chelsa_max_10Y_20042013)=="layer"] <- "Chelsa_10Y_Max"
names(Chelsa_max_10Y_20052014)[names(Chelsa_max_10Y_20052014)=="Max_CHELSA_LV95_CH_fullsize_20052014_allmonths"] <- "Chelsa_10Y_Max"
names(Chelsa_max_10Y_20052014)[names(Chelsa_max_10Y_20052014)=="layer"] <- "Chelsa_10Y_Max"
#names(Chelsa_max_10Y_20062015)[names(Chelsa_max_10Y_20062015)=="Max_CHELSA_LV95_CH_fullsize_20062015_allmonths"] <- "Chelsa_10Y_Max"
#names(Chelsa_max_10Y_20062015)[names(Chelsa_max_10Y_20062015)=="layer"] <- "Chelsa_10Y_Max"

#Chelsa_stack_5Y_20092013 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Stack_CHELSA_LV95_CH_fullsize_20092013_allmonths_5Y.tif")
Chelsa_stack_5Y_20102014 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Stack_CHELSA_LV95_CH_fullsize_20102014_allmonths_5Y.tif")
#Chelsa_stack_5Y_20112015 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Stack_CHELSA_LV95_CH_fullsize_20112015_allmonths_5Y.tif")

#Chelsa_max_5Y_20092013 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Max_CHELSA_LV95_CH_fullsize_20092013_allmonths_5Y.tif")
Chelsa_max_5Y_20102014 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Max_CHELSA_LV95_CH_fullsize_20102014_allmonths_5Y.tif")
#Chelsa_max_5Y_20112015 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Max_CHELSA_LV95_CH_fullsize_20112015_allmonths_5Y.tif")

#names(Chelsa_max_5Y_20092013)[names(Chelsa_max_5Y_20092013)=="Max_CHELSA_LV95_CH_fullsize_20092013_allmonths_5Y"] <- "Chelsa_5Y_Max"
#names(Chelsa_max_5Y_20092013)[names(Chelsa_max_5Y_20092013)=="layer"] <- "Chelsa_5Y_Max"
names(Chelsa_max_5Y_20102014)[names(Chelsa_max_5Y_20102014)=="Max_CHELSA_LV95_CH_fullsize_20102014_allmonths_5Y"] <- "Chelsa_5Y_Max"
names(Chelsa_max_5Y_20102014)[names(Chelsa_max_5Y_20102014)=="layer"] <- "Chelsa_5Y_Max"
#names(Chelsa_max_5Y_20112015)[names(Chelsa_max_5Y_20112015)=="Max_CHELSA_LV95_CH_fullsize_20112015_allmonths_5Y"] <- "Chelsa_5Y_Max"
#names(Chelsa_max_5Y_20112015)[names(Chelsa_max_5Y_20112015)=="layer"] <- "Chelsa_5Y_Max"



# CROP to STUDY SITE
# use elevation as basis for reconfiguration of layers with CH size and wrong spatial resolution

# RESIZE PIXELS to 2m for susceptibility maps

# CHECK and FORCE - origin, extent, projection, data


# Chelsa_max_5Y_20092013_matched <- resample(Chelsa_max_5Y_20092013, elevation, method='bilinear')
# Chelsa_max_5Y_20092013_matched <- mask(Chelsa_max_5Y_20092013_matched, elevation)
# Chelsa_max_5Y_20092013 <- crop(Chelsa_max_5Y_20092013_matched, extent(elevation))

Chelsa_max_5Y_20102014_matched <- resample(Chelsa_max_5Y_20102014, elevation, method='bilinear')
Chelsa_max_5Y_20102014_matched <- mask(Chelsa_max_5Y_20102014_matched, elevation)
Chelsa_max_5Y_20102014 <- crop(Chelsa_max_5Y_20102014_matched, extent(elevation))
# # 
# Chelsa_max_5Y_20112015_matched <- resample(Chelsa_max_5Y_20112015, elevation, method='bilinear')
# Chelsa_max_5Y_20112015_matched <- mask(Chelsa_max_5Y_20112015_matched, elevation)
# Chelsa_max_5Y_20112015 <- crop(Chelsa_max_5Y_20112015_matched, extent(elevation))

# Chelsa_max_5Y_20112015 <- scale(Chelsa_max_5Y_20112015)
Chelsa_max_5Y_20102014 <- scale(Chelsa_max_5Y_20102014)
#Chelsa_max_5Y_20092013 <- scale(Chelsa_max_5Y_20092013)

# CROP to STUDY SITE
# use elevation as basis for reconfiguration of layers with CH size and wrong spatial resolution

# RESIZE PIXELS to 2m for susceptibility maps

# CHECK and FORCE - origin, extent, projection, data

# Chelsa_max_10Y_20042013_matched <- resample(Chelsa_max_10Y_20042013, elevation, method='bilinear')
# Chelsa_max_10Y_20042013_matched <- mask(Chelsa_max_10Y_20042013_matched, elevation)
# Chelsa_max_10Y_20042013 <- crop(Chelsa_max_10Y_20042013_matched, extent(elevation))

Chelsa_max_10Y_20052014_matched <- resample(Chelsa_max_10Y_20052014, elevation, method='bilinear')
Chelsa_max_10Y_20052014_matched <- mask(Chelsa_max_10Y_20052014_matched, elevation)
Chelsa_max_10Y_20052014 <- crop(Chelsa_max_10Y_20052014_matched, extent(elevation))

# Chelsa_max_10Y_20062015_matched <- resample(Chelsa_max_10Y_20062015, elevation, method='bilinear')
# Chelsa_max_10Y_20062015_matched <- mask(Chelsa_max_10Y_20062015_matched, elevation)
# Chelsa_max_10Y_20062015 <- crop(Chelsa_max_10Y_20062015_matched, extent(elevation))

#Chelsa_max_10Y_20062015 <- scale(Chelsa_max_10Y_20062015)
Chelsa_max_10Y_20052014 <- scale(Chelsa_max_10Y_20052014)
#Chelsa_max_10Y_20042013 <- scale(Chelsa_max_10Y_20042013)


snow_days <- raster("/Volumes/Lauren_Uni/M3_SSGM/Explanatory_Variables/Snow_Days_y/CHELSA_LV95_CH_snow_days_mean_20092013.tif")
snow_cover_days <- raster("/Volumes/Lauren_Uni/M3_SSGM/Explanatory_Variables/Snow_Cover_Days_y/CHELSA_LV95_CH_scd_mean_20092013.tif")
grow_season_length <- raster("/Volumes/Lauren_Uni/M3_SSGM/Explanatory_Variables/Growing_Season_Length_y/CHELSA_LV95_CH_gsl_mean_20092013.tif")
frost_ch_freq <- raster("/Volumes/Lauren_Uni/M3_SSGM/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_LV95_CH_fcf_mean_20092013.tif")

# CROP to STUDY SITE
# use elevation as basis for reconfiguration of layers with CH size and wrong spatial resolution

# RESIZE PIXELS to 2m for susceptibility maps

# CHECK and FORCE - origin, extent, projection, data

snow_days_matched <- resample(snow_days, elevation, method='bilinear')
snow_days_matched <- mask(snow_days_matched, elevation)
snow_days <- crop(snow_days_matched, extent(elevation))
names(snow_days)[names(snow_days)=="CHELSA_LV95_CH_snow_days_mean_20092013"] <- "Snow_Days"
snow_days <- scale(snow_days)

snow_cover_days_matched <- resample(snow_cover_days, elevation, method='bilinear')
snow_cover_days_matched <- mask(snow_cover_days_matched, elevation)
snow_cover_days <- crop(snow_cover_days_matched, extent(elevation))
names(snow_cover_days)[names(snow_cover_days)=="CHELSA_LV95_CH_scd_mean_20092013"] <- "Snow_Cover_Days"
snow_cover_days <- scale(snow_cover_days)

grow_season_length_matched <- resample(grow_season_length, elevation, method='bilinear')
grow_season_length_matched <- mask(grow_season_length_matched, elevation)
grow_season_length <- crop(grow_season_length_matched, extent(elevation))
names(grow_season_length)[names(grow_season_length)=="CHELSA_LV95_CH_gsl_mean_20092013"] <- "Grow_Season_Length"
grow_season_length <- scale(grow_season_length)

frost_ch_freq_matched <- resample(frost_ch_freq, elevation, method='bilinear')
frost_ch_freq_matched <- mask(frost_ch_freq_matched, elevation)
frost_ch_freq <- crop(frost_ch_freq_matched, extent(elevation))
names(frost_ch_freq)[names(frost_ch_freq)=="CHELSA_LV95_CH_fcf_mean_20092013"] <- "Frost_Change_Freq"
frost_ch_freq <- scale(frost_ch_freq)

# Aspect

breaks <- c(0, 22.5,67.5,112.5, 157.5, 202.5, 247.5, 292.5, 337.5, Inf)
rc <- cut(aspect, breaks=breaks)
values(rc)[values(rc)==9] <- 1

aspect_cat <- rc

aspect_1 <- ratify(aspect_cat)==1
aspect_2 <- ratify(aspect_cat)==2
aspect_3 <- ratify(aspect_cat)==3
aspect_4 <- ratify(aspect_cat)==4
aspect_5 <- ratify(aspect_cat)==5
aspect_6 <- ratify(aspect_cat)==6
aspect_7 <- ratify(aspect_cat)==7
aspect_8 <- ratify(aspect_cat)==8


# Geology: Gesteinsklasse 
gesteinsklasse <- readOGR(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/Gesteinsklasse/",Site,"_Gesteinsklasse.shp", sep=""))

#ext <- extent(gesteinsklasse)
ext <- extent(elevation)
r <- raster(ext, res=2)


gesteinsklasse_raster <- rasterize(gesteinsklasse, r, field="GESTEINKL")
# ratify raster
gestk <- ratify(gesteinsklasse_raster)
# Create levels
rat <- levels(gestk)[[1]]
rat$gesteinsklasse <- gesteinsklasse$GESTEINKL
levels(gestk) <- rat


projection_LV95 <- "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs"
# # Change crs projection of CHELSA DEC layer
if (is.na(projection(gestk))) {
  crs(gestk) <- projection_LV95
} else {
  gestk <- projectRaster(gestk, crs=projection_LV95)
}
projection(gestk)
origin(gestk) <- origin (elevation)
gestk <- mask(gestk, elevation)
gestk <- crop(gestk, extent(elevation))
gestk


gestk_1 <- ratify(gestk)==1
gestk_2 <- ratify(gestk)==2
#gestk_3 <- ratify(gestk)==3
#gestk_4 <- ratify(gestk)==4

# 
# projection(gestk)
# data(gestk)
# origin(gestk)
# extent(gestk)
# 
# projection(elevation)
# data(elevation)
# origin(elevation)
# extent(elevation)
# 


raster_stack <- stack(elevation, slope, twi, flowacc, 
                      curvature_plan, curvature_profile, roughness, 
                      dist_t_Roads, dist_t_Streams, 
                      density_Roads, density_Streams, 
                      # Chelsa_max_5Y_20092013, 
                      # Chelsa_max_10Y_20042013,
                      Chelsa_max_5Y_20102014,
                      Chelsa_max_10Y_20052014,
                      # Chelsa_max_5Y_20112015,
                      # Chelsa_max_10Y_20062015,
                      snow_days,
                      snow_cover_days,
                      grow_season_length,
                      frost_ch_freq,
                      gestk_1, gestk_2, #gestk_3, gestk_4,
                      aspect_1, aspect_2, aspect_3, aspect_4, aspect_5, aspect_6, aspect_7, aspect_8)

writeRaster(raster_stack, paste("/Volumes/Lauren_Uni/M3_SSGM/Stack_all_Predictors_",Site,".tif",sep=''), format="GTiff", overwrite=TRUE)





# 
# 
# 
# 
# # hcl.colors(n, palette = "viridis", alpha = NULL, rev = FALSE, fixup = TRUE)
# 
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Elevation.pdf", width = 7, height = 5)
# plot(raster_stack$DEM_2m_Val_Piora, main="Elevation", col=hcl.colors(20, palette='ag_GrnYl'))
# dev.off()
# 
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Slope.pdf", width = 7, height = 5)
# plot(raster_stack$Slope_2m_Val_Piora, main="Slope", col=hcl.colors(40, palette='ag_GrnYl'))
# dev.off()
# 
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_TWI.pdf", width = 7, height = 5)
# plot(raster_stack$TWI_2m_Val_Piora, main="TWI", col=hcl.colors(10, palette='ag_GrnYl'))
# dev.off()
# 
# # Flow Accumulation
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Flow_Acc.pdf", width = 7, height = 5)
# min <- 0
# max <- 15000
# Flow_Acc <- raster_stack$FlowAcc_2m_Val_Piora
# #Flow_Acc[raster_stack$FlowAcc_2m_Val_Piora <= min] <- NA
# Flow_Acc[raster_stack$FlowAcc_2m_Val_Piora >= max] <- NA
# plot(Flow_Acc, main="Flow Accumulation", col=hcl.colors(5, palette='ag_GrnYl', rev=TRUE))
# dev.off()
# 
# # Curvature Plan
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Curv_plan.pdf", width = 7, height = 5)
# min <- -50
# max <- 50
# Curvature_plan <- raster_stack$Curvature_plan_2m_Val_Piora
# Curvature_plan[raster_stack$Curvature_plan_2m_Val_Piora <= min] <- NA
# Curvature_plan[raster_stack$Curvature_plan_2m_Val_Piora >= max] <- NA
# plot(Curvature_plan, main="Curvature (plan)", col=hcl.colors(10, palette='ag_GrnYl'))
# dev.off()
# 
# 
# 
# # Curvature Profile 
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Curv_profile.pdf", width = 7, height = 5)
# min <- -50
# max <- 50
# Curvature_profile <- raster_stack$Curvature_profile_2m_Val_Piora
# Curvature_profile[raster_stack$Curvature_profile_2m_Val_Piora <= min] <- NA
# Curvature_profile[raster_stack$Curvature_profile_2m_Val_Piora >= max] <- NA
# plot(Curvature_profile, main="Curvature (profile)", col=hcl.colors(10, palette='ag_GrnYl', rev=TRUE))
# dev.off()
# 
# # Roughness 
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Roughness.pdf", width = 7, height = 5)
# min <- 0
# max <- 25
# Rough_ness <- raster_stack$roughness
# #Rough_ness[raster_stack$roughness <= min] <- NA
# Rough_ness[raster_stack$roughness >= max] <- NA
# plot(Rough_ness, main="Roughness", col=hcl.colors(20, palette='ag_GrnYl'))
# dev.off()
# 
# 
# # Distance to Roads 
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Dist_to_Roads.pdf", width = 7, height = 5)
# plot(raster_stack$CH_Distance_To_Roads_10m, main="Distnace to Roads", col=hcl.colors(100, palette='ag_GrnYl'))
# dev.off()
# 
# # Distance to Streams 
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Dist_to_Streams.pdf", width = 7, height = 5)
# plot(raster_stack$CH_Distance_To_Streams_10m, main="Distnace to Streams", col=hcl.colors(100, palette='ag_GrnYl'))
# dev.off()
# 
# # Road Density
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Road_Density.pdf", width = 7, height = 5)
# plot(raster_stack$CH_Density_Roads_500m, main="Road Density", col=hcl.colors(50, palette='ag_GrnYl'))
# dev.off()
# 
# # Distance to Streams 
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Stream_Density.pdf", width = 7, height = 5)
# plot(raster_stack$CH_Density_Streams_500m, main="Stream Density", col=hcl.colors(50, palette='ag_GrnYl'))
# dev.off()
# 
# # Precip 5 Year Max
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Precip_5Y_Max.pdf", width = 7, height = 5)
# plot(raster_stack$Chelsa_5Y_Max, main="Precipitation 5-Year Max.", col=hcl.colors(50, palette='ag_GrnYl'))
# dev.off()
# 
# # Precip 5 Year Max
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Precip_10Y_Max.pdf", width = 7, height = 5)
# plot(raster_stack$Chelsa_10Y_Max, main="Precipitation 10-Year Max.", col=hcl.colors(50, palette='ag_GrnYl'))
# dev.off()
# 
# # Snow Days
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Snow_Days.pdf", width = 7, height = 5)
# plot(raster_stack$Snow_Days, main="Snow Days", col=hcl.colors(50, palette='ag_GrnYl'))
# dev.off()
# 
# # Snow Cover Days
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Snow_Cover_Days.pdf", width = 7, height = 5)
# plot(raster_stack$Snow_Cover_Days, main="Snow Cover Days", col=hcl.colors(50, palette='ag_GrnYl'))
# dev.off()
# 
# # Growing Season Length
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Growing_Season_Length.pdf", width = 7, height = 5)
# plot(raster_stack$Grow_Season_Length, main="Growing Season Length", col=hcl.colors(50, palette='ag_GrnYl'))
# dev.off()
# 
# # Frost Change Frequency
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Frost_Change_Freq.pdf", width = 7, height = 5)
# plot(raster_stack$Frost_Change_Freq, main="Frost Change Frequency", col=hcl.colors(50, palette='ag_GrnYl'))
# dev.off()
# 
# # Aspect
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Aspect.pdf", width = 7, height = 5)
# plot(raster_stack$layer.1, main="Aspect", col=hcl.colors(8, palette='ag_GrnYl'))
# dev.off()
# 
# # Geology
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora_layers_Geology.pdf", width = 7, height = 5)
# plot(raster_stack$layer.2, main="Geology", legend=FALSE, col=hcl.colors(3, palette='ag_GrnYl'))
# legend(x = "bottomleft", bg="white", legend = c("Metamorphic", "Igneous", "Sedimentary"), 
#        fill = c("#FDE333", "#00BE7D", "#003761"))
# dev.off()
# 
# 
# 
# 
