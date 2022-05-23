

library(plyr)

load ('/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/Arosa.rda')
lsl_Arosa <- subset(lsl, select=-c(aspect,corine,ssgm,veg_rough,winter_P_30Y)) 
lsl_Arosa$Site <- as.factor("Arosa")
levels(lsl_Arosa$lithology)[10] <- "Moraene"

load ('/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/Baulmes.rda')
lsl_Baulmes <- subset(lsl, select=-c(aspect,corine,ssgm,veg_rough,winter_P_30Y)) 
lsl_Baulmes$Site <- as.factor("Baulmes")
levels(lsl_Baulmes$lithology)[3] <- "Moraene"

load ('/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/Chrauchtal.rda')
lsl_Chrauchtal <- subset(lsl, select=-c(aspect,corine,ssgm,veg_rough,winter_P_30Y)) 
lsl_Chrauchtal$Site <- as.factor("Chrauchtal")

load ('/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/Hornbach.rda')
lsl_Hornbach <- subset(lsl, select=-c(aspect,corine,ssgm,veg_rough,winter_P_30Y)) 
lsl_Hornbach$Site <- as.factor("Hornbach")

load ('/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/Rappetal.rda')
lsl_Rappetal <- subset(lsl, select=-c(aspect,corine,ssgm,veg_rough,winter_P_30Y)) 
lsl_Rappetal$Site <- as.factor("Rappetal")

load ('/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/Turbach.rda')
lsl_Turbach <- subset(lsl, select=-c(aspect,corine,ssgm,veg_rough,winter_P_30Y)) 
lsl_Turbach$Site <- as.factor("Turbach")
levels(lsl_Turbach$lithology)[3] <- "Moraene"

load ('/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/Urseren.rda')
lsl_Urseren <- subset(lsl, select=-c(aspect,corine,ssgm,veg_rough,winter_P_30Y)) 
lsl_Urseren$Site <- as.factor("Urseren")
levels(lsl_Urseren$lithology)[9] <- "Moraene"

load ('/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/Val_Cluozza.rda')
lsl_Val_Cluozza <- subset(lsl, select=-c(aspect,corine,ssgm,veg_rough,winter_P_30Y)) 
lsl_Val_Cluozza$Site <- as.factor("Val_Cluozza")
levels(lsl_Val_Cluozza$lithology)[4] <- "Moraene"

load ('/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/Val_D_Entremont.rda')
lsl_Val_D_Entremont <- subset(lsl, select=-c(aspect,corine,ssgm,veg_rough,winter_P_30Y)) 
lsl_Val_D_Entremont$Site <- as.factor("Val_D_Entremont")
levels(lsl_Val_D_Entremont$lithology)[4] <- "Moraene"

load ('/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/Val_Piora.rda')
lsl_Val_Piora <- subset(lsl, select=-c(aspect,corine,ssgm,veg_rough,winter_P_30Y)) 
lsl_Val_Piora$Site <- as.factor("Val_Piora")


lsl_total <- rbind(lsl_Arosa, lsl_Baulmes, lsl_Chrauchtal, lsl_Hornbach, lsl_Rappetal, lsl_Turbach, lsl_Urseren, lsl_Val_Cluozza, lsl_Val_D_Entremont, lsl_Val_Piora)


lsl_std <- lsl_total

mean_CHELSA_max_value10Y <- mean(na.omit(lsl_std$CHELSA_max_value10Y))
sd_CHELSA_max_value10Y <- sd(na.omit(lsl_std$CHELSA_max_value10Y))
lsl_std$CHELSA_max_value10Y[is.na(lsl_std$CHELSA_max_value10Y)] <- mean_CHELSA_max_value10Y
lsl_std$CHELSA_max_value10Y <- (lsl_std$CHELSA_max_value10Y - mean_CHELSA_max_value10Y) / sd_CHELSA_max_value10Y


mean_CHELSA_max_value5Y <- mean(na.omit(lsl_std$CHELSA_max_value5Y))
sd_CHELSA_max_value5Y <- sd(na.omit(lsl_std$CHELSA_max_value5Y))
lsl_std$CHELSA_max_value5Y[is.na(lsl_std$CHELSA_max_value5Y)] <- mean_CHELSA_max_value5Y
lsl_std$CHELSA_max_value5Y <- (lsl_std$CHELSA_max_value5Y - mean_CHELSA_max_value5Y) / sd_CHELSA_max_value5Y

mean_elevation <- mean(na.omit(lsl_std$elevation))
sd_elevation <- sd(na.omit(lsl_std$elevation))
lsl_std$elevation[is.na(lsl_std$elevation)] <- mean_elevation
lsl_std$elevation <- (lsl_std$elevation - mean_elevation) / sd_elevation

mean_slope <- mean(na.omit(lsl_std$slope))
sd_slope <- sd(na.omit(lsl_std$slope))
lsl_std$slope[is.na(lsl_std$slope)] <- mean_slope
lsl_std$slope <- (lsl_std$slope - mean_slope) / sd_slope

mean_twi <- mean(na.omit(lsl_std$twi))
sd_twi <- sd(na.omit(lsl_std$twi))
lsl_std$twi[is.na(lsl_std$twi)] <- mean_twi
lsl_std$twi <- (lsl_std$twi - mean_twi) / sd_twi

mean_flowacc <- mean(na.omit(lsl_std$flowacc))
sd_flowacc <- sd(na.omit(lsl_std$flowacc))
lsl_std$flowacc[is.na(lsl_std$flowacc)] <- mean_flowacc
lsl_std$flowacc <- (lsl_std$flowacc - mean_flowacc) / sd_flowacc

mean_flowdir <- mean(na.omit(lsl_std$flowdir))
sd_flowdir <- sd(na.omit(lsl_std$flowdir))
lsl_std$flowdir[is.na(lsl_std$flowdir)] <- mean_flowdir
lsl_std$flowdir <- (lsl_std$flowdir - mean_flowdir) / sd_flowdir

mean_curvature_plan <- mean(na.omit(lsl_std$curvature_plan))
sd_curvature_plan <- sd(na.omit(lsl_std$curvature_plan))
lsl_std$curvature_plan[is.na(lsl_std$curvature_plan)] <- mean_curvature_plan
lsl_std$curvature_plan <- (lsl_std$curvature_plan - mean_curvature_plan) / sd_curvature_plan

mean_curvature_profile <- mean(na.omit(lsl_std$curvature_profile))
sd_curvature_profile <- sd(na.omit(lsl_std$curvature_profile))
lsl_std$curvature_profile[is.na(lsl_std$curvature_profile)] <- mean_curvature_profile
lsl_std$curvature_profile <- (lsl_std$curvature_profile - mean_curvature_profile) / sd_curvature_profile

mean_roughness <- mean(na.omit(lsl_std$roughness))
sd_roughness <- sd(na.omit(lsl_std$roughness))
lsl_std$roughness[is.na(lsl_std$roughness)] <- mean_roughness
lsl_std$roughness <- (lsl_std$roughness - mean_roughness) / sd_roughness

mean_snow_days <- mean(na.omit(lsl_std$snow_days))
sd_snow_days <- sd(na.omit(lsl_std$snow_days))
lsl_std$snow_days[is.na(lsl_std$snow_days)] <- mean_snow_days
lsl_std$snow_days <- (lsl_std$snow_days - mean_snow_days) / sd_snow_days

mean_snow_cover_days <- mean(na.omit(lsl_std$snow_cover_days))
sd_snow_cover_days <- sd(na.omit(lsl_std$snow_cover_days))
lsl_std$snow_cover_days[is.na(lsl_std$snow_cover_days)] <- mean_snow_cover_days
lsl_std$snow_cover_days <- (lsl_std$snow_cover_days - mean_snow_cover_days) / sd_snow_cover_days

mean_grow_season_length <- mean(na.omit(lsl_std$grow_season_length))
sd_grow_season_length <- sd(na.omit(lsl_std$grow_season_length))
lsl_std$grow_season_length[is.na(lsl_std$grow_season_length)] <- mean_grow_season_length
lsl_std$grow_season_length <- (lsl_std$grow_season_length - mean_grow_season_length) / sd_grow_season_length

mean_frost_ch_freq <- mean(na.omit(lsl_std$frost_ch_freq))
sd_frost_ch_freq <- sd(na.omit(lsl_std$frost_ch_freq))
lsl_std$frost_ch_freq[is.na(lsl_std$frost_ch_freq)] <- mean_frost_ch_freq
lsl_std$frost_ch_freq <- (lsl_std$frost_ch_freq - mean_frost_ch_freq) / sd_frost_ch_freq
# 
# mean_ssgm <- mean(na.omit(lsl_std$ssgm))
# sd_ssgm <- sd(na.omit(lsl_std$ssgm))
# lsl_std$ssgm[is.na(lsl_std$ssgm)] <- mean_ssgm
# lsl_std$ssgm <- (lsl_std$ssgm - mean_ssgm) / sd_ssgm
# 
# mean_veg_rough <- mean(na.omit(lsl_std$veg_rough))
# sd_veg_rough <- sd(na.omit(lsl_std$veg_rough))
# lsl_std$veg_rough[is.na(lsl_std$veg_rough)] <- 0
# lsl_std$veg_rough <- (lsl_std$veg_rough - mean_veg_rough) / sd_veg_rough
# 
# mean_winter_P_30Y <- mean(na.omit(lsl_std$winter_P_30Y))
# sd_winter_P_30Y <- sd(na.omit(lsl_std$winter_P_30Y))
# lsl_std$winter_P_30Y[is.na(lsl_std$winter_P_30Y)] <- mean_winter_P_30Y
# lsl_std$winter_P_30Y <- (lsl_std$winter_P_30Y - mean_winter_P_30Y) / sd_winter_P_30Y


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

sum(is.na(lsl_std))

#lsl_std$geology[is.na(lsl_std$geology)] <- "jo"
lsl_std$lithology[is.na(lsl_std$lithology)] <- "Sedimentgesteine"
lsl_std$gesteinsklasse[is.na(lsl_std$gesteinsklasse)] <- "Sedimentgesteine"
lsl_std$aspect_factor[is.na(lsl_std$aspect_factor)] <- "SE"
sum(is.na(lsl_std))
plot(lsl_std$Site)
#lsl_std <- na.omit(lsl_std)

save(lsl_std, file = paste("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/All_Sites_standardized.rda", sep=""))


