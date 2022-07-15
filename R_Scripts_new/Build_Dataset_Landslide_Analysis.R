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
library("stringr")
library("tidyr")

# 2013 Turbach, Urseren, Val_D_Entremont
# 2014 Arosa, Baulmes, Chrauchtal
# 2015 Hornbach, Rappetal, Val_Cluozza, Val_Piora

# depending on the Location for Precipitation 10Y and 5Y different time frames need to be chose. For other vartiables from CEHLSA
# (e.g. snow days, there is no possibility to change time frame because dataset only goes to 2013

standardize <- function(data) {
  return (data - mean(na.omit(data))) / sd(na.omit(data))
}

set.seed(1)

Sites <- c(
  "Arosa"=2014
  # "Baulmes"=2014,
  # "Chrauchtal"=2014,
  # "Hornbach"=2015,
  # "Rappetal"=2015,
  # "Turbach"=2013,
  # "Urseren"=2013,
  # "Val_Cluozza"=2015,
  # "Val_D_Entremont"=2013,
  # "Val_Piora"=2015
)

out_dir <- "/Users/beni/Documents/BayianLasso/data/data/out_R_new"

data_dir <- "/Users/beni/Documents/BayianLasso/data/data";
explanatory_variables_dir <- file.path(data_dir, "Explanatory_Variables_Datasets")
distance_to_roads_dir <- file.path(explanatory_variables_dir, "Distance_To_Roads")
distance_to_streams_dir <- file.path(explanatory_variables_dir, "Distance_To_Streams")
density_roads_dir <- file.path(explanatory_variables_dir, "Density_Roads")
density_streams_dir <- file.path(explanatory_variables_dir, "Density_Streams")
snow_days_dir <- file.path(explanatory_variables_dir, "Snow_Days")
snow_cover_days_dir <- file.path(explanatory_variables_dir, "Snow_Cover_Days")
grow_season_length_dir <- file.path(explanatory_variables_dir, "Growing_Season_Length")
frost_ch_freq_dir <- file.path(explanatory_variables_dir, "Frost_Change_Frequency")

lithology_dir <- file.path(explanatory_variables_dir, "Geology_Lithologie")
gesteinsklasse_dir <- file.path(explanatory_variables_dir, "Geology_Gesteinsklasse")

for (Site in names(Sites)) {
  year <- Sites[Site]

  LSL_pts <- read.delim(file.path(data_dir, "Landslide_Points_Studysites", str_interp("${Site}_Landslide_Points_4sqm.txt")))
  N_LSL_pts <- read.delim(file.path(data_dir, "Landslide_Points_Studysites", str_interp("${Site}_Random_Points_nonLS_4sqm.txt")))
  lsl <- bind_rows(LSL_pts, N_LSL_pts) %>%
    separate("FID.x.y.lslpts", c("id", "x","y","lslpts"), sep = ",", remove=FALSE)

  lsl$x <- as.numeric(as.character(lsl$x))
  lsl$y <- as.numeric(as.character(lsl$y))

  ##### CHELSA

  data_dir_10Y <- file.path(data_dir, "Explanatory_Variables_Datasets", "Precip_10Y")
  data_dir_5Y <- file.path(data_dir, "Explanatory_Variables_Datasets", "Precip_5Y")

  Chelsa_stack_10Y <- stack(file.path(data_dir_10Y, str_interp("Stack_CHELSA_LV95_CH_fullsize_${year-9}${year}_allmonths.tif")))
  # Chelsa_max_10Y <- stack(file.path(data_dir_10Y, str_interp("Max_CHELSA_LV95_CH_fullsize_{year-9}${year}_allmonths.tif")))
  # names(Chelsa_max_10Y)[names(Chelsa_max_10Y)=="layer"] <- "Chelsa_10Y_Max"

  Chelsa_stack_5Y <- stack(file.path(data_dir_5Y, str_interp("Stack_CHELSA_LV95_CH_fullsize_${year-4}${year}_allmonths_5Y.tif")))
  # Chelsa_max_5Y <- stack(file.path(data_dir_5Y, str_interp("Max_CHELSA_LV95_CH_fullsize_${year-4}${year}_allmonths_5Y.tif")))
  # names(Chelsa_max_5Y)[names(Chelsa_max_5Y)=="layer"] <- "Chelsa_5Y_Max"

  # lsl$CHELSA_max_value10Y <- as.numeric(raster::extract(Chelsa_max_10Y, lsl[c('x', 'y')], method='simple'))
  # lsl$CHELSA_max_value5Y <- as.numeric(raster::extract(Chelsa_max_5Y, lsl[c('x', 'y')], method='simple'))

  ##### SwissALTI

  elevation <- raster(file.path(explanatory_variables_dir, "Elevation", str_interp("DEM_2m_${Site}.tif")))
  roughness <- terrain(elevation, opt="roughness")
  slope <- raster(file.path(explanatory_variables_dir, "Slope", str_interp("Slope_2m_${Site}.tif")))
  aspect <- raster(file.path(explanatory_variables_dir, "Aspect", str_interp("Aspect_2m_${Site}.tif")))
  twi <- raster(file.path(explanatory_variables_dir, "Topo_Wetness_Index", str_interp("TWI_2m_${Site}.tif")))
  flowacc <- raster(file.path(explanatory_variables_dir, "Flow_Accumulation", str_interp("FlowAcc_2m_${Site}.tif")))
  flowdir <- raster(file.path(explanatory_variables_dir, "Flow_Direction", str_interp("FlowDir_2m_${Site}.tif")))
  curvature_plan <- raster(file.path(explanatory_variables_dir, "Curvature", str_interp("Curvature_plan_2m_${Site}.tif")))
  curvature_profile <- raster(file.path(explanatory_variables_dir, "Curvature", str_interp("Curvature_profile_2m_${Site}.tif")))
  #curvature_pp <- raster(file.path(explanatory_variables_dir, "Curvature", str_interp("Curvature_plan_and_profile_2m_${Site}.tif"))

  eastness <- sin(aspect)
  northness <- cos(aspect)


  dist_t_Roads <- raster(file.path(distance_to_roads_dir, "CH_Distance_To_Roads_10m.tif"))
  dist_t_Streams <- raster(file.path(distance_to_streams_dir, "CH_Distance_To_Streams_10m.tif"))

  density_Roads <- raster(file.path(density_roads_dir, "CH_Density_Roads_500m.tif"))
  density_Streams <- raster(file.path(density_streams_dir, "CH_Density_Streams_500m.tif"))

  # extract values for points using x and y points: column 2(x) and 3(y)
  lsl$elevation <- raster::extract(elevation, lsl[c('x', 'y')], method='simple')
  lsl$roughness <- raster::extract(roughness, lsl[c('x', 'y')], method='simple')
  lsl$slope <- raster::extract(slope, lsl[c('x', 'y')], method='simple')
  lsl$aspect <- raster::extract(aspect, lsl[c('x', 'y')], method='simple')
  lsl$twi <- raster::extract(twi, lsl[c('x', 'y')], method='simple')
  lsl$flowacc <- raster::extract(flowacc, lsl[c('x', 'y')], method='simple')
  lsl$flowdir <- raster::extract(flowdir, lsl[c('x', 'y')], method='simple')
  lsl$curvature_plan <- raster::extract(curvature_plan, lsl[c('x', 'y')], method='simple')
  lsl$curvature_profile <- raster::extract(curvature_profile, lsl[c('x', 'y')], method='simple')

  #curvature_pp_extracted <- raster::extract(curvature_pp, lsl[c('x', 'y')], method='simple')
  #lsl$curvature_pp <- curvature_pp_extracted

  # eastness_extracted <- raster::extract(eastness, lsl[c('x', 'y')], method='simple')
  # lsl$eastness <- eastness_extracted
  #
  # northness_extracted <- raster::extract(northness, lsl[c('x', 'y')], method='simple')
  # lsl$northness <- northness_extracted

  ##### SwissTLM

  lsl$dist_t_Roads <- raster::extract(dist_t_Roads, lsl[c('x', 'y')], method='simple')
  lsl$dist_t_Streams <- raster::extract(dist_t_Streams, lsl[c('x', 'y')], method='simple')
  lsl$density_Roads <- raster::extract(density_Roads, lsl[c('x', 'y')], method='simple')
  lsl$density_Streams <- raster::extract(density_Streams, lsl[c('x', 'y')], method='simple')

  # # TODO add SSGM values
  # ssgm <- raster(paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/SSGM_study_sites/",Site,"_SSGM.tif", sep=""))
  #
  # ssgm_extracted <- raster::extract(ssgm, lsl[c('x', 'y')], method='simple')
  # lsl$ssgm <- ssgm_extracted
  #
  # lsl$ssgm[is.na(lsl$ssgm)] <- 0
  #
  # # add veg. roughness
  # veg_rough <- raster("/Volumes/Lauren_Uni/SSGM_PROJECT/R_output_resample_raster_ALL_LAYERS/Semi_Static/Veg_Roughness_SSGM_06.tif")
  #
  # veg_rough_extracted <- raster::extract(veg_rough, lsl[c('x', 'y')], method='simple')
  # lsl$veg_rough <- veg_rough_extracted
  # lsl$veg_rough[is.na(lsl$veg_rough)] <- 0
  #
  # # 30 Year Winter precipitation sum
  #
  # winter_P_30Y<- raster("/Volumes/Lauren_Uni/SSGM_PROJECT/SSGM_OUTPUT_Winterprecip/CHELSAcruts_LV95_CH_WINTER_BASELINE_fullsize19852015.tif")
  # winter_P_30Y_extracted <- raster::extract(winter_P_30Y, lsl[c('x', 'y')], method='simple')
  # lsl$winter_P_30Y <- winter_P_30Y_extracted
  #
  # lsl$winter_P_30Y[is.na(lsl$winter_P_30Y)] <- 0

  ##### add CHELSA derivatives

  snow_days <- raster(file.path(snow_days_dir, "CHELSA_LV95_CH_snow_days_mean_20092013.tif"))
  snow_cover_days <- raster(file.path(snow_cover_days_dir, "CHELSA_LV95_CH_scd_mean_20092013.tif"))
  grow_season_length <- raster(file.path(grow_season_length_dir, "CHELSA_LV95_CH_gsl_mean_20092013.tif"))
  frost_ch_freq <- raster(file.path(frost_ch_freq_dir, "CHELSA_LV95_CH_fcf_mean_20092013.tif"))

  lsl$snow_days <- raster::extract(snow_days, lsl[c('x', 'y')], method='simple')
  lsl$snow_cover_days <- raster::extract(snow_cover_days, lsl[c('x', 'y')], method='simple')
  lsl$grow_season_length <- raster::extract(grow_season_length, lsl[c('x', 'y')], method='simple')
  lsl$frost_ch_freq <- raster::extract(frost_ch_freq, lsl[c('x', 'y')], method='simple')

  ##### add Categorial Datasets

    field <- "LITHO"
    lithology <- readOGR(file.path(lithology_dir, str_interp("${Site}_Lithology.shp")))
    lithology$field <- factor(lithology[[field]])

    lithology_raster <- rasterize(lithology, raster(extent(lithology), res=2), field="field")
    # ratify raster
    litho <- ratify(lithology_raster)

    print(litho)

    rat <- levels(litho)[[1]]
    rat[[field]] <- levels(lithology$field)
    levels(litho) <- rat

    print(litho)

    # Create levels
    # rat <- levels(litho)[[1]]
    # rat$lithology <- lithology$LITHO
    # levels(litho) <- rat
    #rasterVis::levelplot(litho)
    lithology_extracted <- raster::extract(litho, lsl[c('x', 'y')], method='simple')

    print(lithology_extracted)

    lithology_extracted <- factorValues(litho, lithology_extracted)
    lsl$lithology <- lithology_extracted
    lsl$lithology <- lsl$lithology$lithology



  gesteinsklasse <- readOGR(file.path(gesteinsklasse_dir, str_interp("${Site}_Gesteinsklasse.shp")))

  gestk <- ratify(
    rasterize(
      gesteinsklasse,
      raster(extent(gesteinsklasse), res=2),
      field="GESTEINKL"
    )
  )

  rat <- levels(gestk)[[1]]
  rat$gesteinsklasse <- gesteinsklasse$GESTEINKL
  levels(gestk) <- rat

  gesteinsklasse_extracted <- raster::extract(gestk, lsl[c('x', 'y')], method='simple')
  gesteinsklasse_extracted <- factorValues(gestk, gesteinsklasse_extracted)

  lsl$gesteinsklasse <- gesteinsklasse_extracted
  lsl$gesteinsklasse <- lsl$gesteinsklasse$gesteinsklasse

  # TODO # add corine landcover information
  # corine <- raster("/Volumes/Lauren_Uni/CORINE/u2018_clc2012_v2020_20u1_raster100m/DATA/CORINE_CH_2012.tif")
  #
  # corine_extracted <- raster::extract(corine, lsl[c('x', 'y')], method='simple')
  # lsl$corine <- as.factor(corine_extracted)

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

  dir.create(file.path(out_dir, "All_Sites_Points"), showWarnings = FALSE)

  #  export of variables
  save(lsl, file = file.path(out_dir, "All_Sites_Points", str_interp("${Site}.rda")))

  plot(lsl$lithology)
  plot(lsl$gesteinsklasse)

  # --------------------------------------------
  # standardize continuous values
  # --------------------------------------------

  # copy original dataset
  lsl_std <- lsl

  # lsl_std$CHELSA_max_value10Y <- standardize(lsl_std$CHELSA_max_value10Y)
  # lsl_std$CHELSA_max_value5Y <- standardize(lsl_std$CHELSA_max_value5Y)
  lsl_std$elevation <- standardize(lsl_std$elevation)
  lsl_std$slope <- standardize(lsl_std$slope)
  lsl_std$aspect <- standardize(lsl_std$aspect)
  lsl_std$twi <- standardize(lsl_std$twi)
  lsl_std$flowacc <- standardize(lsl_std$flowacc)
  lsl_std$flowdir <- standardize(lsl_std$flowdir)
  lsl_std$curvature_plan <- standardize(lsl_std$curvature_plan)
  lsl_std$curvature_profile <- standardize(lsl_std$curvature_profile)
  lsl_std$roughness <- standardize(lsl_std$roughness)
  lsl_std$snow_days <- standardize(lsl_std$snow_days)
  lsl_std$snow_cover_days <- standardize(lsl_std$snow_cover_days)
  lsl_std$grow_season_length <- standardize(lsl_std$grow_season_length)
  lsl_std$frost_ch_freq <- standardize(lsl_std$frost_ch_freq)
  # lsl_std$ssgm <- standardize(lsl_std$ssgm)
  # lsl_std$veg_rough <- standardize(lsl_std$veg_rough)
  lsl_std$winter_P_30Y <- standardize(lsl_std$winter_P_30Y)
  lsl_std$dist_t_Roads <- standardize(lsl_std$dist_t_Roads)
  lsl_std$dist_t_Streams <- standardize(lsl_std$dist_t_Streams)
  lsl_std$density_Roads <- standardize(lsl_std$density_Roads)
  lsl_std$density_Streams <- standardize(lsl_std$density_Streams)

  # Handle missing values: TODO maybe not here? So we can try differen (e.g. dropping)

  lsl_std$dist_t_Roads[is.na(lsl_std$dist_t_Roads)] <- 0
  lsl_std$dist_t_Streams[is.na(lsl_std$dist_t_Streams)] <- 0
  lsl_std$density_Roads[is.na(lsl_std$density_Roads)] <- 0
  lsl_std$density_Streams[is.na(lsl_std$density_Streams)] <- 0

  str(lsl_std)

  #  export of variables for model development:
  save(lsl_std, file = file.path(out_dir, "All_Sites_Points", str_interp("${Site}_standardized.rda")))

}
