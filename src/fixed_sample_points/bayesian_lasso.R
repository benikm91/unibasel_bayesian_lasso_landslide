# Reimplement R stuff for python dataset => For comparing with python solution

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
library("stringr")
library(MBSGS)

standardize <- function(data) {
  return(data - mean(na.omit(data))) / sd(na.omit(data))
}

mode <- function(x) { # thanks to https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

k.boot <- 20 # How many grids to generate
n.folds <- 5 # How many groups to make per grid
n.folds.hyperparameter <- 10 # Number of folds to find hyper parameter per model.

Sites <- c(
  "Arosa" = 2014,
  "Baulmes" = 2014,
  "Chrauchtal" = 2014,
  "Hornbach" = 2015,
  "Rappetal" = 2015,
  "Turbach" = 2013,
  "Urseren" = 2013,
  "Val_Cluozza" = 2015,
  # "Val_D_Entremont" = 2013,
  "Val_Piora" = 2015
)

for (Site in names(Sites)) {

  print(str_interp("Process ${Site}"))

  current_dir <- "/Users/beni/Documents/BayianLasso/data/data/src/fixed_sample_points"
  data_dir <- file.path(current_dir, "out")
  out_dir <- file.path(current_dir, "out", "lasso_fixed")
  path <- file.path(data_dir, str_interp("${Site}_geo_df.feather"))
  lsl <- arrow::read_feather(path)

  lsl$aspect_factor <- lsl$aspect

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

  lsl$lslpts <- lsl$landslide

  lsl$gesteinsklasse <- as.factor(lsl$gesteinsklasse)

  label <- c('lslpts')
  numeric_features <- c(
    'elevation',
    'slope',
    'curvature_plan',
    'curvature_profile',
    'roughness',
    'flowacc',
    'twi',  # Topographic wetness index
    'distance_to_roads',
    'distance_to_streams',
    'density_roads',
    'density_streams',
    'flowdir',
    'max_precip_5Y',
    'max_precip_10Y',
    'stack_precip_5Y',
    'stack_precip_10Y',
    'snow_days',
    'snow_cover_days',
    'grow_season_length',
    'frost_ch_freq'
  )
  features <- c(
    numeric_features,
    'gesteinsklasse',
    'aspect_factor'
  )

  lsl <- subset(lsl, select = c(label, 'x', 'y', features))

  lsl <- lsl %>% mutate_at(numeric_features, scale)

  outline <- readOGR(str_interp("/Users/beni/Documents/BayianLasso/data/data/Outlines_Studysites/${Site}.shp"))
  elevation <- raster(str_interp("/Users/beni/Documents/BayianLasso/data/data/Explanatory_Variables_Datasets/Elevation/DEM_2m_${Site}.tif"))
  pa_data <- sf::st_as_sf(lsl, coords = c("x", "y"), crs = raster::crs(elevation))
  # plot(pa_data$gesteinsklasse)
  # - nice plot overview of variables
  plot(pa_data, max.plot = 21)

  for (j in 1:k.boot) {

    # - create vector that will be filled in the Loop
    vector.cv.grpreg <- vector()
    vector.obs <- vector()

    set.seed(j)
    print(j)

    # spatial blocking by specified range and random assignment
    sb1 <- spatialBlock(speciesData = pa_data,
                        species = "lslpts",
                        theRange = 1000,
                        rasterLayer = elevation,
                        k = n.folds,
                        selection = "random",
                        iteration = 10,
                        showBlocks = FALSE)

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
    # foldExplorer(sb1, elevation, pa_data)

    cvID <- sb1$foldID

    # - merge data together with ID subset
    lsl.data.ID <- cbind(lsl, cvID)

    #plot(sort(cvID), col=cvID, pch=19, type='b', main='Block Cross-Validation ID' )

    # - subset is shorter than full data set, so many have NA filled. Replace NA with zero so these variables
    # - are not removed later by na.omit
    #plot(lsl.data.ID$geology)

    # take larges amount of geology to fill NA if there are any

    #lsl.data.ID$lithology[is.na(lsl.data.ID$lithology)] <- "Sedimentgesteine"
    if ("Lockergesteine" %in% unique(lsl.data.ID$gesteinsklasse)) {
      lsl.data.ID$gesteinsklasse[is.na(lsl.data.ID$gesteinsklasse)] <- "Lockergesteine"
    } else {
      lsl.data.ID$gesteinsklasse[is.na(lsl.data.ID$gesteinsklasse)] <- "Sedimentgesteine"
    }
    lsl.data.ID$aspect_factor[is.na(lsl.data.ID$aspect_factor)] <- "E"

    lsl.data.ID[is.na(lsl.data.ID)] <- 0

    str(lsl.data.ID)

    for (i in 1:n.folds) {

      print(i)

      ## - choose only data without ID=xx for training data set
      lsl.data.train <- subset(lsl.data.ID, cvID != i, select = -c(x, y, cvID))

      ## - choose only data with ID=xx for test data set
      lsl.data.test <- subset(lsl.data.ID, cvID == i, select = -c(x, y, cvID))

      # - define groups: dummy coding of a factor is treated as group
      # - alternative
      # - find factors
      l.covar <- features
      l.factors <- names(lsl[l.covar])[
        t.f <- unlist(lapply(lsl[l.covar], is.factor))]
      l.numeric <- names(t.f[!t.f])
      # create a vector that labels the groups with the same number
      groups <- c(
        1:length(l.numeric),
        unlist(
          sapply(1:length(l.factors), function(n) {
            rep(n + length(l.numeric), nlevels(lsl[[l.factors[n]]]))
          })
        )
      )

      # - creating x and dealing with DUMMY VARIABLES (categorial factors)
      # - this code snippet includes all dummy variables
      contr.Dummy <- function(contrasts, ...) {
        conT <- contr.treatment(contrasts = FALSE, ...)
        conT
      }

      options(contrasts = c(ordered = 'contr.Dummy', unordered = 'contr.Dummy'))


      # - trim off the first column: with TRAIN dataset
      x <- model.matrix(lslpts ~ ., data = lsl.data.train)[, -1]

      # - leaving only the predictors
      y <- as.numeric(lsl.data.train$lslpts)

      # ****   TRAINING PROCESS    ****

      # - cross-validate grpreg model with TRAIN dataset
      fit.grpreg <- cv.grpreg(
        X = x,
        y = y,
        group = groups,
        #penalty="grLasso",
        penalty = "grLasso",
        family = "binomial",
        nfolds = n.folds.hyperparameter,
        nlambda = 100,
        alpha = 1, # alpha=1 => No l2-regularization
        returnY = TRUE
      )

      # - choose optimal lambda: CV minimum error + 1 SE (see glmnet)
      l.se <- fit.grpreg$cve[fit.grpreg$min] + fit.grpreg$cvse[fit.grpreg$min]

      print("fit.grpreg$min")
      print(fit.grpreg$min)

      print("fit.grpreg$cve[fit.grpreg$min]")
      print(fit.grpreg$cve[fit.grpreg$min])

      print("fit.grpreg$cvse[fit.grpreg$min]")
      print(fit.grpreg$cvse[fit.grpreg$min])

      print("l.se")
      print(l.se)

      print("fit.grpreg$cve")
      print(fit.grpreg$cve)

      # idx.se <- min(which(fit.grpreg$cve < l.se)) - 1 # TODO Beni: Why here -1? We take the first that's bigger than CV minimum error + 1 SE, not the max that is still within CV minimum error + 1 SE...
      idx.se <- min(which(fit.grpreg$cve < l.se))
      lambda.min <- fit.grpreg$lambda.min
      # - get the non-zero coefficients:
      #coeffiecients.glmnet <- coef(glmnet.glmnet, s=lambda.min)
      coef.grpreg <- fit.grpreg$fit$beta[, idx.se]
      #coef.grpreg[ coef.grpreg != 0 ]

      print("fit.grpreg$fit$beta")
      print(fit.grpreg$fit$beta)

      print("coef.grpreg")
      print(coef.grpreg)

      print("idx.se")
      print(idx.se)

      # TODO solve decimal point issue
      beta <- as.numeric(coef.grpreg)

      #beta <- format(round(beta, 8),scientific=FALSE)
      #signif(beta, digits = 8)
      coefficients <- data.frame(names(coef.grpreg), beta)

      print("names(coefficients)")
      print(names(coefficients))

      names(coefficients) <- c("name", "beta")
      #coefficients <- format(round(coefficients, digits=8, nsmall=8),scientific=FALSE)

      assign(paste("model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_", i, "_", j, sep = ""), coefficients)

      # - write coefficients to output file
      sink(paste(out_dir, "/", Site, ".model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_", i, "_", j, ".txt", sep = ""))
      cat("============= coeffiecients ================\n")
      print(coefficients)
      cat("============= lambda.min ================\n")
      print(lambda.min)
      cat("============= lambda.1se ================\n")
      print(fit.grpreg$lambda[idx.se])
      sink()


      # ****   PREDICTING PROCESS (TEST)   ****

      # **** predict() models with TEST dataset
      # trim off the first column
      newx.matrix = model.matrix(lslpts ~ ., lsl.data.test)[, -1]

      fcst.model.1.cv.grpreg.fold <- predict(
        fit.grpreg,  # TODO Beni: why here fit grpreg? why not select idx.se model?
        X = newx.matrix,
        lambda = fit.grpreg$lambda[idx.se],
        type = "response"
      )

      # fcst.model.1.cv.grpreg.fold <- predict(fit.grpreg,
      #                                        X=newx.matrix,
      #                                        lambda=lambda.min,
      #                                        type="response")
      #
      assign(paste("fcst.model.1.Gestkl.cv.grpreg_grLasso.fold", i, "_", j, sep = ""), fcst.model.1.cv.grpreg.fold)
      #
      # vector.cv.grpreg <- append(vector.cv.grpreg, fcst.model.1.cv.grpreg.fold)


      # get CV predictions
      lasso.cv.pred <- fit.grpreg$Y[, idx.se]
      ## ----lasso-plot-cv,echo=FALSE,fig.width=7,fig.height=4.5, fig.align='center', out.width='0.8\\textwidth',fig.cap = "Cross validation error plotted against the tuning parameter lambda. The dashed line indicates lambda at minimal error, the dotted darkgrey line is the optimal lambda with minimal error + 1 SE."----
      plot(fit.grpreg)
      abline(h = l.se, col = "grey", lty = "dotted")
      abline(v = log(fit.grpreg$lambda[idx.se]), col = "grey30", lty = "dotted")


      assign(paste("model.1.Gestkl.cv.grpreg_grLasso.fold", i, "_", j, sep = ""), fit.grpreg)

      # - save lambda plot to .pdf file
      pdf(paste(out_dir, "/", Site, "_model_1_Gestkl_cv_grpreg_grLasso_fold_", i, "_", j, ".pdf", sep = ""))
      plot(fit.grpreg)
      abline(h = l.se, col = "grey", lty = "dotted")
      abline(v = log(fit.grpreg$lambda[idx.se]), col = "grey30", lty = "dotted")
      dev.off()

      # save predictions and observations of folds, so for later evaluation of predicitons the right observations match (due to randomizing k-fold process)
      vector.cv.grpreg <- append(vector.cv.grpreg, fcst.model.1.cv.grpreg.fold)
      vector.obs <- append(vector.obs, lsl.data.test$lslpts)
    }

    # rename vectors (security step: to not loose what loop has done if what follows is wrong)

    # TODO Beni: vector.cv.grpreg does not get reinitialized in for loop!
    cv.grpreg <- vector.cv.grpreg
    obs.grpreg <- vector.obs

    #save data set as .rda file
    save(cv.grpreg, file = paste(out_dir, "/", Site, ".model.1.Gestkl.cv.grpreg_grLasso_", j, ".rda", sep = ""))
    save(obs.grpreg, file = paste(out_dir, "/", Site, ".model.1.Gestkl.lslpts.observations_", j, ".rda", sep = ""))

    dev.off()
  }
}

