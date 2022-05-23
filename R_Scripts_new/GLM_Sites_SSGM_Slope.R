#*****************************     model with only ssgm

# - load packages
library('raster')
library('spatial')
library('glmnet')
library('lmtest')
library('reshape')
library("dplyr")
#library("ggbiplot")
#library("grplasso")
#library("gglasso")
library("glm")
library("caret")
library("parallel") # for parallel computing?
library("blockCV") # spatial crossvalidation
library("shiny")
library("rgdal")
library("sf")
library("ggplot2")
library("data.table")
#library("sperrorest")

# ******** Load Dataset

# ******** CHECK GEOLOGY ********************************************

Site <- "Arosa"
#Arosa <- load ('/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/Arosa.rda')
#Arosa <- load ('/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/Arosa_standardized.rda')
load ('/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Geosegmentation/All_Sites_Points/Arosa_standardized.rda')
str(lsl_std)
#lsl_std <- subset(lsl_std, select=-c(CID)) 

# - make sure experiments turn out the same by setting seed 
#set.seed(666)
#set.seed(1)

# - create vector that will be filled in the Loop
vector.glm <- vector()
vector.obs <- vector()

# - rename dataset to lsl (match normal dataset. lsl_std contains standardized coefficients).
# - remove aspect as a "continious variable" 
lsl <- subset(lsl_std, select=c(x, y, lslpts, ssgm)) 


outline <- readOGR("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/Outlines/Arosa.shp")
elevation <- raster("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/DEM/Elevation/DEM_2m_Arosa.tif")
pa_data <- sf::st_as_sf(lsl, coords = c("x", "y"), crs = raster::crs(elevation))

# - nice plot overview of variables
#plot(pa_data, max.plot = 21)

# - make sure experiments turn out the same by setting seed 
#set.seed(1)
k.boot=20

for (j in 1:k.boot ) {

  set.seed(j)
  print(j)
  
  # spatial blocking by specified range and random assignment
  sb1 <- spatialBlock(speciesData = pa_data,
                      species = "lslpts",
                      theRange = 1000,
                      rasterLayer = elevation,
                      k = 5,
                      selection = "random",
                      iteration = 10,
                      showBlocks = FALSE)
  
  # 
  # # spatial blocking by specified range and random assignment
  # sb1 <- spatialBlock(speciesData = pa_data,
  #                     species = "lslpts",
  #                     theRange = 1000,
  #                     rasterLayer = elevation,
  #                     k = 5,
  #                     selection = "random",
  #                     iteration = 10)
  # dev.off()
  
  # - visualize folds
  #foldExplorer(sb1, elevation, pa_data)
  
  cvID <- sb1$foldID
  
  # - merge data together with ID subset
  lsl.data.ID <-  cbind(lsl,cvID)
  
  # - subset is shorter than full data set, so many have NA filled. Replace NA with zero so these variables 
  # - are not removed later by na.omit
  
  lsl.data.ID[is.na(lsl.data.ID)] <- 0
  
  str(lsl.data.ID)
  
  n.folds <- 5
  
  for (i in 1:n.folds) {

    print(i)
    
    ## - choose only data without ID=xx for training data set
    lsl.data.train <- subset(lsl.data.ID, cvID != i, select=-c(x,y,cvID)) 
    
    ## - choose only data with ID=xx for test data set
    lsl.data.test <- subset(lsl.data.ID, cvID == i, select=-c(x,y,cvID))
    
    # - create glm model with Widmer Index as only response variable
    mod.ssgm <- glm(lslpts ~ ssgm, family = binomial, data = lsl.data.train)
    
    # ****   PREDICTING PROCESS (TEST)   ****
    
    fcst.model.1.glm.fold <- predict(mod.ssgm, newdata=subset(lsl.data.test, select=c(ssgm)), type="response")

    coef.glm <- mod.ssgm$coefficients
    #coef.glm[ coef.glm != 0 ]
    
    # TODO solve decimal point issue
    beta <- as.numeric(coef.glm)
    
    #beta <- format(round(beta, 8),scientific=FALSE)
    #signif(beta, digits = 8)
    coefficients <- data.frame(names(coef.glm), beta)
    
    names(coefficients) <- c("name", "beta")
    #coefficients <- format(round(coefficients, digits=8, nsmall=8),scientific=FALSE)
    
    assign(paste("model.1.only.ssgm.coeffiecients_glm_",i,"_",j,sep=""),  coefficients)
    # - write coefficients to output file
    sink(paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/",Site,"/",Site,".model.1.only.ssgm.coeffiecients_glm_",i,"_",j,".txt",sep=""))
    cat("============= coeffiecients ================\n")
    print(coefficients)
    sink()
    
    
    # save predictions and observations of folds, so for later evaluation of predicitons the right observations match (due to randomizing k-fold process)
    vector.glm <- append(vector.glm, fcst.model.1.glm.fold)
    vector.obs <- append(vector.obs, lsl.data.test$lslpts)
  }
  
  
  
  # rename vectors (security step: to not loose what loop has done if what follows is wrong)
  
  glm <- vector.glm
  obs.glm <- vector.obs
  
  #save data set as .rda file 
  save(glm, file=paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/",Site,"/",Site,".model.1.only.ssgm.glm_",j,".rda", sep=""))
  save(obs.glm, file=paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/",Site,"/",Site,".model.1.only.ssgm.lslpts.obs_",j,".rda", sep=""))
  
}

