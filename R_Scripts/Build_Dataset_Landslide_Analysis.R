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

# 2013 Turbach, Urseren, Val_D_Entremont
# 2014 Arosa, Baulmes, Chrauchtal
# 2015 Hornbach, Rappetal, Val_Cluozza, Val_Piora

# depending on the Location for Precipitation 10Y and 5Y different time frames need to be chose. For other vartiables from CEHLSA
# (e.g. snow days, there is no possibility to change time frame because dataset only goes to 2013

set.seed(1)

Site <- "Chrauchtal"


# Load Txt Files with Points for Landslides and Non-Landslides
LSL_pts <- read.delim(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/",Site,"_Landslide_Points_4sqm.txt",sep=""), header = T, sep=",")
N_LSL_pts <-read.delim(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/",Site,"_Random_Points_nonLS_4sqm.txt",sep=""), header = T, sep=",")
lsl <- bind_rows(LSL_pts,N_LSL_pts)


head(lsl)
lsl <- lsl[, -(1)]
## - only if "CID" is in dataset
# Baulmes, Hornbach
#lsl <- lsl[, -(4)]


# # not necessary to do this, but to double check it maches Geosegmentation Tutorial
# # select non-landslide points
# non_pts = filter(landslides, lslpts == 0)
# # select landslide points
# lsl_pts = filter(landslides, lslpts == 1)
# lsl = bind_rows(non_pts, lsl_pts)


# CHELSA DATASET
# !!!! ADAPT LOWER to right years as well !!!!

Chelsa_stack_10Y_20042013 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Stack_CHELSA_LV95_CH_fullsize_20042013_allmonths.tif")
#Chelsa_stack_10Y_20052014 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Stack_CHELSA_LV95_CH_fullsize_20052014_allmonths.tif")
#Chelsa_stack_10Y_20062015 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Stack_CHELSA_LV95_CH_fullsize_20062015_allmonths.tif")

Chelsa_max_10Y_20042013 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Max_CHELSA_LV95_CH_fullsize_20042013_allmonths.tif")
#Chelsa_max_10Y_20052014 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Max_CHELSA_LV95_CH_fullsize_20052014_allmonths.tif")
#Chelsa_max_10Y_20062015 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Max_CHELSA_LV95_CH_fullsize_20062015_allmonths.tif")

names(Chelsa_max_10Y_20042013)[names(Chelsa_max_10Y_20042013)=="layer"] <- "Chelsa_10Y_Max"
#names(Chelsa_max_10Y_20052014)[names(Chelsa_max_10Y_20052014)=="layer"] <- "Chelsa_10Y_Max"
#names(Chelsa_max_10Y_20062015)[names(Chelsa_max_10Y_20062015)=="layer"] <- "Chelsa_10Y_Max"

Chelsa_stack_5Y_20092013 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Stack_CHELSA_LV95_CH_fullsize_20092013_allmonths_5Y.tif")
#Chelsa_stack_5Y_20102014 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Stack_CHELSA_LV95_CH_fullsize_20102014_allmonths_5Y.tif")
#Chelsa_stack_5Y_20112015 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Stack_CHELSA_LV95_CH_fullsize_20112015_allmonths_5Y.tif")

Chelsa_max_5Y_20092013 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Max_CHELSA_LV95_CH_fullsize_20092013_allmonths_5Y.tif")
#Chelsa_max_5Y_20102014 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Max_CHELSA_LV95_CH_fullsize_20102014_allmonths_5Y.tif")
#Chelsa_max_5Y_20112015 <- stack("/Volumes/Lauren_Uni/M3_SSGM/Chelsa_Precipitation_Statistic_Evaluation/Max_CHELSA_LV95_CH_fullsize_20112015_allmonths_5Y.tif")

names(Chelsa_max_5Y_20092013)[names(Chelsa_max_5Y_20092013)=="layer"] <- "Chelsa_5Y_Max"
#names(Chelsa_max_5Y_20102014)[names(Chelsa_max_5Y_20102014)=="layer"] <- "Chelsa_5Y_Max"
#names(Chelsa_max_5Y_20112015)[names(Chelsa_max_5Y_20112015)=="layer"] <- "Chelsa_5Y_Max"


##### add DEM values

elevation <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Elevation/DEM_2m_",Site,".tif",sep=""))
slope <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Slope/Slope_2m_",Site,".tif",sep=""))
aspect <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Aspect/Aspect_2m_",Site,".tif",sep=""))
twi <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/TWI/TWI_2m_",Site,".tif",sep=""))
flowacc <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Flow/FlowAcc_2m_",Site,".tif",sep=""))
flowdir <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Flow/FlowDir_2m_",Site,".tif",sep=""))
curvature_plan <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Curvature/Curvature_plan_2m_",Site,".tif",sep=""))
curvature_profile <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Curvature/Curvature_profile_2m_",Site,".tif",sep=""))
#curvature_pp <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Curvature/Curvature_plan_and_profile_2m_",Site,".tif",sep=""))
roughness <- terrain(elevation, opt="roughness")

eastness <- sin(aspect)
northness <- cos(aspect)


dist_t_Roads <- raster("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Distance/CH_Distance_To_Roads_10m.tif")
dist_t_Streams <- raster("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Distance/CH_Distance_To_Streams_10m.tif")

density_Roads <- raster("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Density/CH_Density_Roads_500m.tif")
density_Streams <- raster("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Density/CH_Density_Streams_500m.tif")



# extract values for points using x and y points: column 2(x) and 3(y)
elev_extracted <- raster::extract(elevation, lsl[,1:2], method='simple')
lsl$elevation <- elev_extracted

slope_extracted <- raster::extract(slope, lsl[,1:2], method='simple')
lsl$slope <- slope_extracted

aspect_extracted <- raster::extract(aspect, lsl[,1:2], method='simple')
lsl$aspect <- aspect_extracted

twi_extracted <- raster::extract(twi, lsl[,1:2], method='simple')
lsl$twi <- twi_extracted

flowacc_extracted <- raster::extract(flowacc, lsl[,1:2], method='simple')
lsl$flowacc <- flowacc_extracted

flowdir_extracted <- raster::extract(flowdir, lsl[,1:2], method='simple')
lsl$flowdir <- flowdir_extracted

curvature_plan_extracted <- raster::extract(curvature_plan, lsl[,1:2], method='simple')
lsl$curvature_plan <- curvature_plan_extracted

curvature_profile_extracted <- raster::extract(curvature_profile, lsl[,1:2], method='simple')
lsl$curvature_profile <- curvature_profile_extracted

#curvature_pp_extracted <- raster::extract(curvature_pp, lsl[,1:2], method='simple')
#lsl$curvature_pp <- curvature_pp_extracted

# eastness_extracted <- raster::extract(eastness, lsl[,1:2], method='simple')
# lsl$eastness <- eastness_extracted
# 
# northness_extracted <- raster::extract(northness, lsl[,1:2], method='simple')
# lsl$northness <- northness_extracted

roughness_extracted <- raster::extract(roughness, lsl[,1:2], method='simple')
lsl$roughness <- roughness_extracted

dist_t_Roads_extracted <- raster::extract(dist_t_Roads, lsl[,1:2], method='simple')
lsl$dist_t_Roads <- dist_t_Roads_extracted

dist_t_Streams_extracted <- raster::extract(dist_t_Streams, lsl[,1:2], method='simple')
lsl$dist_t_Streams <- dist_t_Streams_extracted

density_Roads_extracted <- raster::extract(density_Roads, lsl[,1:2], method='simple')
lsl$density_Roads <- density_Roads_extracted

density_Streams_extracted <- raster::extract(density_Streams, lsl[,1:2], method='simple')
lsl$density_Streams <- density_Streams_extracted




# add SSGM values
ssgm <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/SSGM_study_sites/",Site,"_SSGM.tif", sep=""))

ssgm_extracted <- raster::extract(ssgm, lsl[,1:2], method='simple')
lsl$ssgm <- ssgm_extracted

lsl$ssgm[is.na(lsl$ssgm)] <- 0

# add veg. roughness
veg_rough <- raster("/Volumes/Lauren_Uni/SSGM_PROJECT/R_output_resample_raster_ALL_LAYERS/Semi_Static/Veg_Roughness_SSGM_06.tif")

veg_rough_extracted <- raster::extract(veg_rough, lsl[,1:2], method='simple')
lsl$veg_rough <- veg_rough_extracted
lsl$veg_rough[is.na(lsl$veg_rough)] <- 0

# 30 Year Winter precipitation sum

winter_P_30Y<- raster("/Volumes/Lauren_Uni/SSGM_PROJECT/SSGM_OUTPUT_Winterprecip/CHELSAcruts_LV95_CH_WINTER_BASELINE_fullsize19852015.tif")
winter_P_30Y_extracted <- raster::extract(winter_P_30Y, lsl[,1:2], method='simple')
lsl$winter_P_30Y <- winter_P_30Y_extracted

lsl$winter_P_30Y[is.na(lsl$winter_P_30Y)] <- 0
##### add CHELSA derivatives

# ?snow_water <- raster("/Volumes/Lauren_Uni/M3_SSGM/Explanatory_Variables/Snow_Water_y/CHELSA_LV95_CH_snow_water_mean_20092013.tif")
# ?snow_water_eq <- raster("/Volumes/Lauren_Uni/M3_SSGM/Explanatory_Variables/Snow_Water_Equivalent_y/CHELSA_LV95_CH_swe_mean_20092013.tif")

snow_days <- raster("/Volumes/Lauren_Uni/M3_SSGM/Explanatory_Variables/Snow_Days_y/CHELSA_LV95_CH_snow_days_mean_20092013.tif")
snow_cover_days <- raster("/Volumes/Lauren_Uni/M3_SSGM/Explanatory_Variables/Snow_Cover_Days_y/CHELSA_LV95_CH_scd_mean_20092013.tif")
grow_season_length <- raster("/Volumes/Lauren_Uni/M3_SSGM/Explanatory_Variables/Growing_Season_Length_y/CHELSA_LV95_CH_gsl_mean_20092013.tif")
frost_ch_freq <- raster("/Volumes/Lauren_Uni/M3_SSGM/Explanatory_Variables/Frost_Change_Frequency_y/CHELSA_LV95_CH_fcf_mean_20092013.tif")
# ?first_day_grow_season <- raster("/Volumes/Lauren_Uni/M3_SSGM/Explanatory_Variables/First_Day_Growing_Season_y/CHELSA_LV95_CH_fgd_mean_20092013.tif")
# ?last_day_grow_season <- raster("/Volumes/Lauren_Uni/M3_SSGM/Explanatory_Variables/Last_Day_Growing_Season_y/CHELSA_LV95_CH_lgd_mean_20092013.tif")

#data <- raster("/PATH/data.tif")



# # - 2013
extract_max_value_10Y <- raster::extract(Chelsa_max_10Y_20042013, lsl[,1:2], method='simple')
lsl$CHELSA_max_value10Y <- as.numeric(extract_max_value_10Y)

extract_max_value_5Y <- raster::extract(Chelsa_max_5Y_20092013, lsl[,1:2], method='simple')
lsl$CHELSA_max_value5Y <- as.numeric(extract_max_value_5Y)

# # - 2014
# extract_max_value_10Y <- raster::extract(Chelsa_max_10Y_20052014, lsl[,1:2], method='simple')
# lsl$CHELSA_max_value10Y <- as.numeric(extract_max_value_10Y)
# 
# extract_max_value_5Y <- raster::extract(Chelsa_max_5Y_20102014, lsl[,1:2], method='simple')
# lsl$CHELSA_max_value5Y <- as.numeric(extract_max_value_5Y)

# # - 2015
# extract_max_value_10Y <- raster::extract(Chelsa_max_10Y_20062015, lsl[,1:2], method='simple')
# lsl$CHELSA_max_value10Y <- as.numeric(extract_max_value_10Y)
# 
# extract_max_value_5Y <- raster::extract(Chelsa_max_5Y_20112015, lsl[,1:2], method='simple')
# lsl$CHELSA_max_value5Y <- as.numeric(extract_max_value_5Y)




snow_days_extracted <- raster::extract(snow_days, lsl[,1:2], method='simple')
lsl$snow_days <- snow_days_extracted

snow_cover_days_extracted <- raster::extract(snow_cover_days, lsl[,1:2], method='simple')
lsl$snow_cover_days <- snow_cover_days_extracted

grow_season_length_extracted <- raster::extract(grow_season_length, lsl[,1:2], method='simple')
lsl$grow_season_length <- grow_season_length_extracted

frost_ch_freq_extracted <- raster::extract(frost_ch_freq, lsl[,1:2], method='simple')
lsl$frost_ch_freq <- frost_ch_freq_extracted

#elev_extracted <- raster::extract(elevation, lsl[,1:2], method='simple')
#lsl$elevation <- elev_extracted





##### add Categorial Datasets

#lithology <- readOGR(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/Lithology/",Site,"_Lithology.shp", sep=""))
# geology <- readOGR(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/Geology/",Site,"_Geology.shp", sep=""))
# 
# ext <- extent(geology)
# r <- raster(ext, data=2)
# geology_raster <- rasterize(geology, r, field="GEOL_F")
# # ratify raster
# geo <- ratify(geology_raster)
# # Create levels
# rat <- levels(geo)[[1]]
# rat$geology <- geology$GEOL_F
# levels(geo) <- rat
# #rasterVis::levelplot(geo)
# geology_extracted <- raster::extract(geo, lsl[,1:2], method='simple')
# geology_extracted <- factorValues(geo, geology_extracted)
# lsl$geology <- geology_extracted
# 
# lsl$geology <- lsl$geology$geology

lithology <- readOGR(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/Lithology/",Site,"_Lithology.shp", sep=""))

ext <- extent(lithology)
r <- raster(ext, res=2)
lithology_raster <- rasterize(lithology, r, field="LITHO")
# ratify raster
litho <- ratify(lithology_raster)
# Create levels
rat <- levels(litho)[[1]]
rat$lithology <- lithology$LITHO
levels(litho) <- rat
#rasterVis::levelplot(litho)
lithology_extracted <- raster::extract(litho, lsl[,1:2], method='simple')
lithology_extracted <- factorValues(litho, lithology_extracted)
lsl$lithology <- lithology_extracted

lsl$lithology <- lsl$lithology$lithology




gesteinsklasse <- readOGR(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/Gesteinsklasse/",Site,"_Gesteinsklasse.shp", sep=""))

ext <- extent(gesteinsklasse)
r <- raster(ext, res=2)
gesteinsklasse_raster <- rasterize(gesteinsklasse, r, field="GESTEINKL")
# ratify raster
gestk <- ratify(gesteinsklasse_raster)
# Create levels
rat <- levels(gestk)[[1]]
rat$gesteinsklasse <- gesteinsklasse$GESTEINKL
levels(gestk) <- rat
#rasterVis::levelplot(gestk)
gesteinsklasse_extracted <- raster::extract(gestk, lsl[,1:2], method='simple')
gesteinsklasse_extracted <- factorValues(gestk, gesteinsklasse_extracted)
lsl$gesteinsklasse <- gesteinsklasse_extracted

lsl$gesteinsklasse <- lsl$gesteinsklasse$gesteinsklasse




# add corine landcover information
corine <- raster("/Volumes/Lauren_Uni/CORINE/u2018_clc2012_v2020_20u1_raster100m/DATA/CORINE_CH_2012.tif")

corine_extracted <- raster::extract(corine, lsl[,1:2], method='simple')
lsl$corine <- as.factor(corine_extracted)

# make aspect as factor (section N, NE...)


lsl$aspect_factor = lsl$aspect


lsl$aspect_factor[lsl$aspect <= 22.5] <- "N"
lsl$aspect_factor[lsl$aspect > 337.5] <- "N"
lsl$aspect_factor[lsl$aspect > 22.5 & lsl$aspect <= 67.5] <- "NE"
lsl$aspect_factor[lsl$aspect > 67.5 & lsl$aspect <= 112.5] <- "E"
lsl$aspect_factor[lsl$aspect > 112.5 & lsl$aspect <= 157.5] <- "SE"
lsl$aspect_factor[lsl$aspect > 157.5 & lsl$aspect <= 202.5] <- "S"
lsl$aspect_factor[lsl$aspect > 202.5 & lsl$aspect <= 247.5] <- "SW"
lsl$aspect_factor[lsl$aspect > 247.5 & lsl$aspect <= 292.5] <- "W"
lsl$aspect_factor[lsl$aspect > 292.5 & lsl$aspect <= 337.5] <- "NW"
lsl$aspect_factor <- as.factor(lsl$aspect_factor)


str(lsl)

# - reminder, that dataset is still sorted by "0" first and then all "1"
# - to mix dataset, use "sample" and resort dataset by mixed samples 
rows <- sample(nrow(lsl))
lsl <- lsl[rows, ]

#  export of variables for model development:
save(lsl, file = paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/",Site,".rda", sep=""))





# --------------------------------------------

# standardize continuous values
# copy original dataset
lsl_std <- lsl

mean_CHELSA_max_value10Y <- mean(na.omit(lsl_std$CHELSA_max_value10Y))
sd_CHELSA_max_value10Y <- sd(na.omit(lsl_std$CHELSA_max_value10Y))
lsl_std$CHELSA_max_value10Y <- (lsl_std$CHELSA_max_value10Y - mean_CHELSA_max_value10Y) / sd_CHELSA_max_value10Y

mean_CHELSA_max_value5Y <- mean(na.omit(lsl_std$CHELSA_max_value5Y))
sd_CHELSA_max_value5Y <- sd(na.omit(lsl_std$CHELSA_max_value5Y))
lsl_std$CHELSA_max_value5Y <- (lsl_std$CHELSA_max_value5Y - mean_CHELSA_max_value5Y) / sd_CHELSA_max_value5Y

mean_elevation <- mean(na.omit(lsl_std$elevation))
sd_elevation <- sd(na.omit(lsl_std$elevation))
lsl_std$elevation <- (lsl_std$elevation - mean_elevation) / sd_elevation

mean_slope <- mean(na.omit(lsl_std$slope))
sd_slope <- sd(na.omit(lsl_std$slope))
lsl_std$slope <- (lsl_std$slope - mean_slope) / sd_slope

mean_aspect <- mean(na.omit(lsl_std$aspect))
sd_aspect <- sd(na.omit(lsl_std$aspect))
lsl_std$aspect <- (lsl_std$aspect - mean_aspect) / sd_aspect

mean_twi <- mean(na.omit(lsl_std$twi))
sd_twi <- sd(na.omit(lsl_std$twi))
lsl_std$twi <- (lsl_std$twi - mean_twi) / sd_twi

mean_flowacc <- mean(na.omit(lsl_std$flowacc))
sd_flowacc <- sd(na.omit(lsl_std$flowacc))
lsl_std$flowacc <- (lsl_std$flowacc - mean_flowacc) / sd_flowacc

mean_flowdir <- mean(na.omit(lsl_std$flowdir))
sd_flowdir <- sd(na.omit(lsl_std$flowdir))
lsl_std$flowdir <- (lsl_std$flowdir - mean_flowdir) / sd_flowdir

mean_curvature_plan <- mean(na.omit(lsl_std$curvature_plan))
sd_curvature_plan <- sd(na.omit(lsl_std$curvature_plan))
lsl_std$curvature_plan <- (lsl_std$curvature_plan - mean_curvature_plan) / sd_curvature_plan

mean_curvature_profile <- mean(na.omit(lsl_std$curvature_profile))
sd_curvature_profile <- sd(na.omit(lsl_std$curvature_profile))
lsl_std$curvature_profile <- (lsl_std$curvature_profile - mean_curvature_profile) / sd_curvature_profile

mean_roughness <- mean(na.omit(lsl_std$roughness))
sd_roughness <- sd(na.omit(lsl_std$roughness))
lsl_std$roughness <- (lsl_std$roughness - mean_roughness) / sd_roughness

mean_snow_days <- mean(na.omit(lsl_std$snow_days))
sd_snow_days <- sd(na.omit(lsl_std$snow_days))
lsl_std$snow_days <- (lsl_std$snow_days - mean_snow_days) / sd_snow_days

mean_snow_cover_days <- mean(na.omit(lsl_std$snow_cover_days))
sd_snow_cover_days <- sd(na.omit(lsl_std$snow_cover_days))
lsl_std$snow_cover_days <- (lsl_std$snow_cover_days - mean_snow_cover_days) / sd_snow_cover_days

mean_grow_season_length <- mean(na.omit(lsl_std$grow_season_length))
sd_grow_season_length <- sd(na.omit(lsl_std$grow_season_length))
lsl_std$grow_season_length <- (lsl_std$grow_season_length - mean_grow_season_length) / sd_grow_season_length

mean_frost_ch_freq <- mean(na.omit(lsl_std$frost_ch_freq))
sd_frost_ch_freq <- sd(na.omit(lsl_std$frost_ch_freq))
lsl_std$frost_ch_freq <- (lsl_std$frost_ch_freq - mean_frost_ch_freq) / sd_frost_ch_freq

mean_ssgm <- mean(na.omit(lsl_std$ssgm))
sd_ssgm <- sd(na.omit(lsl_std$ssgm))
lsl_std$ssgm <- (lsl_std$ssgm - mean_ssgm) / sd_ssgm

mean_veg_rough <- mean(na.omit(lsl_std$veg_rough))
sd_veg_rough <- sd(na.omit(lsl_std$veg_rough))
lsl_std$veg_rough <- (lsl_std$veg_rough - mean_veg_rough) / sd_veg_rough

mean_winter_P_30Y <- mean(na.omit(lsl_std$winter_P_30Y))
sd_winter_P_30Y <- sd(na.omit(lsl_std$winter_P_30Y))
lsl_std$winter_P_30Y <- (lsl_std$winter_P_30Y - mean_winter_P_30Y) / sd_winter_P_30Y

mean_dist_t_Roads <- mean(na.omit(lsl_std$dist_t_Roads))
sd_dist_t_Roads <- sd(na.omit(lsl_std$dist_t_Roads))
lsl_std$dist_t_Roads <- (lsl_std$dist_t_Roads - mean_dist_t_Roads) / sd_dist_t_Roads
lsl_std$dist_t_Roads[is.na(lsl_std$dist_t_Roads)] <- 0

mean_dist_t_Streams <- mean(na.omit(lsl_std$dist_t_Streams))
sd_dist_t_Streams <- sd(na.omit(lsl_std$dist_t_Streams))
lsl_std$dist_t_Streams <- (lsl_std$dist_t_Streams - mean_dist_t_Streams) / sd_dist_t_Streams
lsl_std$dist_t_Streams[is.na(lsl_std$dist_t_Streams)] <- 0

mean_density_Roads <- mean(na.omit(lsl_std$density_Roads))
sd_density_Roads <- sd(na.omit(lsl_std$density_Roads))
lsl_std$density_Roads <- (lsl_std$density_Roads - mean_density_Roads) / sd_density_Roads
lsl_std$density_Roads[is.na(lsl_std$density_Roads)] <- 0

mean_density_Streams <- mean(na.omit(lsl_std$density_Streams))
sd_density_Streams <- sd(na.omit(lsl_std$density_Streams))
lsl_std$density_Streams <- (lsl_std$density_Streams - mean_density_Streams) / sd_density_Streams
lsl_std$density_Streams[is.na(lsl_std$density_Streams)] <- 0


#shapiro.test(sample(lsl$aspect, 3000))

# keep these the same, as they are factors (Categorial)
# geology 
# corine

# already centered around zero
# eastness
# northness

str(lsl_std)
plot(lsl_std$lithology)
plot(lsl_std$gesteinsklasse)

#  export of variables for model development:
save(lsl_std, file = paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/",Site,"_standardized.rda", sep=""))


