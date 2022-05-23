

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
library("grpreg")
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
Site <- "All"

load ('/Users/beni/Documents/BayianLasso/data/data/Predictors_rda_Studysites/All_Sites_standardized.rda')

str(lsl_std)
lsl_std <- subset(lsl_std, select=-c(Site, flowdir, lithology)) 
str(lsl_std)

set.seed(1)
rows <- sample(nrow(lsl_std))
lsl <- lsl_std[rows, ]



# - create vector that will be filled in the Loop
vector.cv.grpreg <- vector()
vector.obs <- vector()


# **** Option 2: BLOCK Cross Validation ****
# - Simple random selection of training and testing 
# - folds in the structured environment leads to an underestimation 
# - of error in the evaluation of spatial predictions and may result 
# - in inappropriate model selection (Telford and Birks, 2009; Roberts et al., 2017)

#outline <- readOGR("/Volumes/Lauren_Uni/M3_SSGM/Study_Site_Shapefiles/Outlines/Hornbach_Valley.shp")
outline <- raster("/Volumes/Lauren_Uni/SSGM_PROJECT/CH_border/CH_outline_LV95.tif")
pa_data <- sf::st_as_sf(lsl, coords = c("x", "y"), crs = raster::crs(outline))
#plot(pa_data$geology)
# - nice plot overview of variables
#plot(pa_data, max.plot = 26)

# - make sure experiments turn out the same by setting seed 
#set.seed(1)
k.boot=20

for (j in 1:k.boot ) {

  set.seed(j)
  print(j)

  # spatial blocking by specified range and random assignment
  sb1 <- spatialBlock(speciesData = pa_data,
                      species = "lslpts",
                      theRange = 2000,
                      #rasterLayer = outline,
                      k = 5,
                      selection = "random",
                      iteration = 10,
                      showBlocks = FALSE)
  
  # # 
  # # # spatial blocking by specified range and random assignment
  # sb1 <- spatialBlock(speciesData = pa_data,
  #                      species = "lslpts",
  #                      theRange = 2000,
  #                      #rasterLayer = outline,
  #                      k = 5,
  #                      selection = "random",
  #                      iteration = 10)
  #  dev.off()
  # 
  # - visualize folds
  # foldExplorer(sb1, outline, pa_data)
  
  cvID <- sb1$foldID
  
  # - merge data together with ID subset
  lsl.data.ID <-  cbind(lsl,cvID)
  
  str(lsl.data.ID)
  
  n.folds <- 5
  
  for (i in 1:n.folds) {
    
    print(i)
    
    ## - choose only data without ID=xx for training data set
    lsl.data.train <- subset(lsl.data.ID, cvID != i, select=-c(x,y,cvID)) 
    
    ## - choose only data with ID=xx for test data set
    lsl.data.test <- subset(lsl.data.ID, cvID == i, select=-c(x,y,cvID))
    
    # - define groups: dummy coding of a factor is treated as group
    # - alternative
    # - find factors
    l.covar <- names(lsl[, 4:ncol(lsl)])
    l.factors <- names(lsl[l.covar])[ 
      t.f <- unlist( lapply(lsl[l.covar], is.factor) ) ]
    l.numeric <-  names(t.f[ !t.f ])
    # create a vector that labels the groups with the same number  
    groups <- c( 1:length(l.numeric), 
                 unlist( 
                   sapply(1:length(l.factors), function(n){
                     rep(n+length(l.numeric), nlevels(lsl[, l.factors[n]]))
                   }) 
                 ) 
    )
    
    
    # - creating x and dealing with DUMMY VARIABLES (categorial factors)
    # - this code snippet includes all dummy variables
    contr.Dummy <- function(contrasts, ...){
      conT <- contr.treatment(contrasts=FALSE, ...)
      conT
    }
    options(contrasts=c(ordered='contr.Dummy', unordered='contr.Dummy'))
    
    
    # - trim off the first column: with TRAIN dataset
    x <- model.matrix(lslpts~., data=lsl.data.train)[,-1]
    
    # - leaving only the predictors
    y <- as.numeric(lsl.data.train$lslpts)
    
    # ****   TRAINING PROCESS    ****
    
    # - cross-validate grpreg model with TRAIN dataset
    fit.grpreg <- cv.grpreg(X = x, 
                            y = y,
                            group=groups,
                            #penalty="grLasso",
                            penalty="grLasso",
                            family="binomial",
                            nfolds=10,
                            nlambda=100,
                            alpha=1,
                            returnY = TRUE)
    
    # - choose optimal lambda: CV minimum error + 1 SE (see glmnet)
    l.se <- fit.grpreg$cvse[ fit.grpreg$min ] + fit.grpreg$cve[ fit.grpreg$min ]
    idx.se <- min( which( fit.grpreg$cve < l.se ) ) - 1
    lambda.min <- fit.grpreg$lambda.min
    # - get the non-zero coefficients:
    #coeffiecients.glmnet <- coef(glmnet.glmnet, s=lambda.min)
    coef.grpreg <- fit.grpreg$fit$beta[, idx.se ]
    #coef.grpreg[ coef.grpreg != 0 ]
    
    # TODO solve decimal point issue
    beta <- as.numeric(coef.grpreg)
    
    #beta <- format(round(beta, 8),scientific=FALSE)
    #signif(beta, digits = 8)
    coefficients <- data.frame(names(coef.grpreg), beta)
    
    names(coefficients) <- c("name", "beta")
    #coefficients <- format(round(coefficients, digits=8, nsmall=8),scientific=FALSE)
    
    assign(paste("model.1.all.sites.coeffiecients_cv_grpreg_grLasso_Gestkl_",i,"_",j,sep=""),  coefficients)
    
    # - write coefficients to output file
#sink(paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/",Site,"/",Site,".model.1.all.sites.coeffiecients_cv_grpreg_grLasso_Gestkl_",i,"_",j,".txt",sep=""))
    cat("============= coeffiecients ================\n")
    print(coefficients)
    cat("============= lambda.min ================\n")
    print(lambda.min)
    cat("============= lambda.1se ================\n")
    print(fit.grpreg$lambda[idx.se])
#sink()
    
    
    # ****   PREDICTING PROCESS (TEST)   ****
    
    # **** predict() models with TEST dataset
    # trim off the first column
    newx.matrix = model.matrix(lslpts~., lsl.data.test)[,-1] 
    
    fcst.model.1.cv.grpreg.fold <- predict(fit.grpreg,
                                           X=newx.matrix,
                                           lambda=fit.grpreg$lambda[idx.se],
                                           type="response")
    # fcst.model.1.cv.grpreg.fold <- predict(fit.grpreg,
    #                                        X=newx.matrix,
    #                                        lambda=lambda.min,
    #                                        type="response")
    # 
    assign(paste("fcst.model.1.cv.all.sites.grpreg_grLasso_Gestkl.fold",i,"_",j,sep=""), fcst.model.1.cv.grpreg.fold)
    # 
    # vector.cv.grpreg <- append(vector.cv.grpreg, fcst.model.1.cv.grpreg.fold)
    
    
    # get CV predictions
    lasso.cv.pred <- fit.grpreg$Y[,idx.se]
    ## ----lasso-plot-cv,echo=FALSE,fig.width=7,fig.height=4.5, fig.align='center', out.width='0.8\\textwidth',fig.cap = "Cross validation error plotted against the tuning parameter lambda. The dashed line indicates lambda at minimal error, the dotted darkgrey line is the optimal lambda with minimal error + 1 SE."----
    plot(fit.grpreg)
    abline( h = l.se, col = "grey", lty = "dotted")
    abline( v = log( fit.grpreg$lambda[ idx.se ]), col = "grey30", lty = "dotted")
    
    
    assign(paste("model.1.all.sites.cv.grpreg_grLasso_Gestkl.fold",i,"_",j,sep=""), fit.grpreg)
    
    # - save lambda plot to .pdf file
    pdf(paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/",Site,"/",Site,"_model_1_all_sites_cv_grpreg_grLasso_Gestkl_fold_",i,"_",j,".pdf",sep=""))
    plot(fit.grpreg)
    abline( h = l.se, col = "grey", lty = "dotted")
    abline( v = log( fit.grpreg$lambda[ idx.se ]), col = "grey30", lty = "dotted")
    dev.off()
    
    # save predictions and observations of folds, so for later evaluation of predicitons the right observations match (due to randomizing k-fold process)
    vector.cv.grpreg <- append(vector.cv.grpreg, fcst.model.1.cv.grpreg.fold)
    vector.obs <- append(vector.obs, lsl.data.test$lslpts)
  }
  
  
  
  # rename vectors (security step: to not loose what loop has done if what follows is wrong)
  
  cv.grpreg <- vector.cv.grpreg
  obs.grpreg <- vector.obs
  
  #save data set as .rda file 
#save(cv.grpreg, file=paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/",Site,"/",Site,".model.1.all.sites.cv.grpreg_grLasso_Gestkl_",j,".rda", sep=""))
#save(obs.grpreg, file=paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/",Site,"/",Site,".model.1.all.sites.lslpts.observations_Gestkl_",j,".rda", sep=""))
  
}

