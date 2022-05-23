# - load packages

library('ggplot2')
# - load packages
library('lmtest')
library('reshape')
library('gplots')
library('ROCR')

out_dir <- "/Users/beni/Documents/BayianLasso/data/data/src/fixed_sample_points/out"

print("Start BrierScore Bootstraping")

# -------- Arosa
k.boot=20

grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste0(out_dir, '/Arosa.model.1.Gestkl.cv.grpreg_grLasso_', j, '.rda'))
  grpreg.model.Arosa <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Arosa)
  load (paste0(out_dir, '/Arosa.model.1.Gestkl.lslpts.observations_', j, '.rda'))
  obs.Arosa <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Arosa)
}

grpreg.model.Arosa <- grpreg.model
obs.Arosa <- obs

# -------- Baulmes
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste0(out_dir, '/Baulmes.model.1.Gestkl.cv.grpreg_grLasso_', j, '.rda'))
  grpreg.model.Baulmes <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Baulmes)
  load (paste0(out_dir, '/Baulmes.model.1.Gestkl.lslpts.observations_', j, '.rda'))
  obs.Baulmes <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Baulmes)
}

grpreg.model.Baulmes <- grpreg.model
obs.Baulmes <- obs



# -------- Chrauchtal
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste0(out_dir, '/Chrauchtal.model.1.Gestkl.cv.grpreg_grLasso_', j, '.rda'))
  grpreg.model.Chrauchtal <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Chrauchtal)
  load (paste0(out_dir, '/Chrauchtal.model.1.Gestkl.lslpts.observations_', j, '.rda'))
  obs.Chrauchtal <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Chrauchtal)
}

grpreg.model.Chrauchtal <- grpreg.model
obs.Chrauchtal <- obs



# -------- Hornbach
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste0(out_dir, '/Hornbach.model.1.Gestkl.cv.grpreg_grLasso_', j, '.rda'))
  grpreg.model.Hornbach <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Hornbach)
  load (paste0(out_dir, '/Hornbach.model.1.Gestkl.lslpts.observations_', j, '.rda'))
  obs.Hornbach <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Hornbach)
}

grpreg.model.Hornbach <- grpreg.model
obs.Hornbach <- obs



# -------- Rappetal
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste0(out_dir, '/Rappetal.model.1.Gestkl.cv.grpreg_grLasso_', j, '.rda'))
  grpreg.model.Rappetal <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Rappetal)
  load (paste0(out_dir, '/Rappetal.model.1.Gestkl.lslpts.observations_', j, '.rda'))
  obs.Rappetal <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Rappetal)
}

grpreg.model.Rappetal <- grpreg.model
obs.Rappetal <- obs



# -------- Turbach
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste0(out_dir, '/Turbach.model.1.Gestkl.cv.grpreg_grLasso_', j, '.rda'))
  grpreg.model.Turbach <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Turbach)
  load (paste0(out_dir, '/Turbach.model.1.Gestkl.lslpts.observations_', j, '.rda'))
  obs.Turbach <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Turbach)
}

grpreg.model.Turbach <- grpreg.model
obs.Turbach <- obs



# -------- Urseren
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste0(out_dir, '/Urseren.model.1.Gestkl.cv.grpreg_grLasso_', j, '.rda'))
  grpreg.model.Urseren <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Urseren)
  load (paste0(out_dir, '/Urseren.model.1.Gestkl.lslpts.observations_', j, '.rda'))
  obs.Urseren <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Urseren)
}

grpreg.model.Urseren <- grpreg.model
obs.Urseren <- obs



# -------- Val Cluozza Lambda.min
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste0(out_dir, '/Val_Cluozza.model.1.Gestkl.cv.grpreg_grLasso_', j, '.rda'))
  grpreg.model.Val_Cluozza <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Val_Cluozza)
  load (paste0(out_dir, '/Val_Cluozza.model.1.Gestkl.lslpts.observations_', j, '.rda'))
  obs.Val_Cluozza <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Val_Cluozza)
}

grpreg.model.Val_Cluozza <- grpreg.model
obs.Val_Cluozza <- obs

# -------- Val Cluozza Lambda.1SE
# grpreg.model <- vector()
# obs <- vector()
#
# for ( j in 1:k.boot ) {
#   load (paste('/out_dir, '.model.2.cv.grpreg_grLasso_',j,'.rda', sep=""))
#   grpreg.model.Val_Cluozza <- as.numeric(cv.grpreg)
#   grpreg.model <- c(grpreg.model, grpreg.model.Val_Cluozza)
#   load (paste('/out_dir, '.model.2.lslpts.observations_',j,'.rda', sep=""))
#   obs.Val_Cluozza <- as.numeric(obs.grpreg)
#   obs <- c(obs, obs.Val_Cluozza)
# }
#
# grpreg.model.Val_Cluozza <- grpreg.model
# obs.Val_Cluozza <- obs


# -------- Val d'Entremont
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  # load (paste(out_dir, '/Val_D_Entremont.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  load (paste0(out_dir, '/Val_Piora.model.1.Gestkl.cv.grpreg_grLasso_', j, '.rda'))
  grpreg.model.Val_D_Entremont <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Val_D_Entremont)
  # load (paste(out_dir, '/Val_D_Entremont.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  load (paste0(out_dir, '/Val_Piora.model.1.Gestkl.lslpts.observations_', j, '.rda'))
  obs.Val_D_Entremont <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Val_D_Entremont)
}

grpreg.model.Val_D_Entremont <- grpreg.model
obs.Val_D_Entremont <- obs



# -------- Val Piora
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste0(out_dir, '/Val_Piora.model.1.Gestkl.cv.grpreg_grLasso_', j, '.rda'))
  grpreg.model.Val_Piora <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Val_Piora)
  load (paste0(out_dir, '/Val_Piora.model.1.Gestkl.lslpts.observations_', j, '.rda'))
  obs.Val_Piora <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Val_Piora)
}

grpreg.model.Val_Piora <- grpreg.model
obs.Val_Piora <- obs





sink(paste0(out_dir, "/BrierScore/Brier_Scores_grlasso_no_bootstrap_Gesteinsklasse.txt"))

cat(sprintf("Brier Score for Arosa: %10.5f\n", mean( ( obs.Arosa - grpreg.model.Arosa )^2)))
cat(sprintf("Brier Score for Baulmes: %10.5f\n", mean( ( obs.Baulmes - grpreg.model.Baulmes )^2)))
cat(sprintf("Brier Score for Chrauchtal: %10.5f\n", mean( ( obs.Chrauchtal - grpreg.model.Chrauchtal )^2)))
cat(sprintf("Brier Score for Hornbach: %10.5f\n", mean( ( obs.Hornbach - grpreg.model.Hornbach )^2)))
cat(sprintf("Brier Score for Rappetal: %10.5f\n", mean( ( obs.Rappetal - grpreg.model.Rappetal )^2)))
cat(sprintf("Brier Score for Turbach: %10.5f\n", mean( ( obs.Turbach - grpreg.model.Turbach )^2)))
cat(sprintf("Brier Score for Urseren: %10.5f\n", mean( ( obs.Urseren - grpreg.model.Urseren )^2)))
cat(sprintf("Brier Score for Val Cluozza: %10.5f\n", mean( ( obs.Val_Cluozza - grpreg.model.Val_Cluozza )^2)))
cat(sprintf("Brier Score for Val d'Entremont: %10.5f\n", mean( ( obs.Val_D_Entremont - grpreg.model.Val_D_Entremont )^2)))
cat(sprintf("Brier Score for Val Piora: %10.5f\n", mean( ( obs.Val_Piora - grpreg.model.Val_Piora )^2)))

sink()








# CALCULATER BRIER SCORE

# - Setting kboot to 200. Values between about 200 and 500 seem
#   to be good enough, but if it is fast enough you can also do
#   that even more often (result will get more stable).

kboot <- 500
set.seed(666)

# -------------------------------------------------------------------
# - Correct: "replacae=TRUE"
# -------------------------------------------------------------------

boot.correct  <- matrix( NA, nrow=kboot, ncol=10,
                         dimnames=list(NULL,c("grpreg.Arosa",
                                              "grpreg.Baulmes",
                                              "grpreg.Chrauchtal",
                                              "grpreg.Hornbach",
                                              "grpreg.Rappetal",
                                              "grpreg.Turbach",
                                              "grpreg.Urseren",
                                              "grpreg.Val.Cluozza",
                                              "grpreg.Val.D.Entremont",
                                              "grpreg.Val.Piora") ))


# - Looping kboot times, pick THE SAME random elements for BOTH
#   (or all) vectors and store the mean values into boot.correct.
#   "idx" will be the random sample.


for ( i in 1:kboot ) {

  n <- length(obs.Arosa)
  idx <- sample(1:n,n,replace=TRUE)
  boot.correct[i,"grpreg.Arosa"] <- mean( ( obs.Arosa[idx] - grpreg.model.Arosa[idx] )^2)

  n <- length(obs.Baulmes)
  idx <- sample(1:n,n,replace=TRUE)
  boot.correct[i,"grpreg.Baulmes"] <- mean( ( obs.Baulmes[idx] - grpreg.model.Baulmes[idx] )^2)

  n <- length(obs.Chrauchtal)
  idx <- sample(1:n,n,replace=TRUE)
  boot.correct[i,"grpreg.Chrauchtal"] <- mean( ( obs.Chrauchtal[idx] - grpreg.model.Chrauchtal[idx] )^2)

  n <- length(obs.Hornbach)
  idx <- sample(1:n,n,replace=TRUE)
  boot.correct[i,"grpreg.Hornbach"] <- mean( ( obs.Hornbach[idx] - grpreg.model.Hornbach[idx] )^2)

  n <- length(obs.Rappetal)
  idx <- sample(1:n,n,replace=TRUE)
  boot.correct[i,"grpreg.Rappetal"] <- mean( ( obs.Rappetal[idx] - grpreg.model.Rappetal[idx] )^2)

  n <- length(obs.Turbach)
  idx <- sample(1:n,n,replace=TRUE)
  boot.correct[i,"grpreg.Turbach"] <- mean( ( obs.Turbach[idx] - grpreg.model.Turbach[idx] )^2)

  n <- length(obs.Urseren)
  idx <- sample(1:n,n,replace=TRUE)
  boot.correct[i,"grpreg.Urseren"] <- mean( ( obs.Urseren[idx] - grpreg.model.Urseren[idx] )^2)

  n <- length(obs.Val_Cluozza)
  idx <- sample(1:n,n,replace=TRUE)
  boot.correct[i,"grpreg.Val.Cluozza"] <- mean( ( obs.Val_Cluozza[idx] - grpreg.model.Val_Cluozza[idx] )^2)

  n <- length(obs.Val_D_Entremont)
  idx <- sample(1:n,n,replace=TRUE)
  boot.correct[i,"grpreg.Val.D.Entremont"] <- mean( ( obs.Val_D_Entremont[idx] - grpreg.model.Val_D_Entremont[idx] )^2)

  n <- length(obs.Val_Piora)
  idx <- sample(1:n,n,replace=TRUE)
  boot.correct[i,"grpreg.Val.Piora"] <- mean( ( obs.Val_Piora[idx] - grpreg.model.Val_Piora[idx] )^2)

}



# PLOT BRIER SCORE

lab <- c("Arosa","Baulmes","Chrauchtal","Hornbachtal","Rappetal","Turbachtal","Urserental","Val Cluozza","Val d'Entremont","Val Piora")

pdf(file= paste0(out_dir, "/BrierScore/bootstrapped_BS_all_Sites_Gesteinsklasse.pdf"))

op <- par(mar=c(6.4,4.5,2,1))

boxplot( boot.correct,
         pars=list(outcol="black"), #main="Brier Scores for Study Sites",
         t="l", xaxt="n", xlab="", ylab="Brier Score", cex.axis=1.3, cex.lab=1.3)

axis(1.3, at=c(1:10), labels=FALSE)

# abline(v=c(1.5, 3.5),col="gray45", lty=1 )

text(x=1:10, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]),
     labels=lab, srt=45, adj=1, xpd=TRUE, cex=1.3)

par(op)

dev.off()


sink(paste0(out_dir, "/BrierScore/Brier_Scores_grlasso_bootstrap_Gesteinsklasse.txt"))

cat(sprintf("Brier Score for Arosa: %10.5f\n", median(boot.correct[,1])))
cat(sprintf("Brier Score for Baulmes: %10.5f\n", median(boot.correct[,2])))
cat(sprintf("Brier Score for Chrauchtal: %10.5f\n", median(boot.correct[,3])))
cat(sprintf("Brier Score for Hornbach: %10.5f\n", median(boot.correct[,4])))
cat(sprintf("Brier Score for Rappetal: %10.5f\n", median(boot.correct[,5])))
cat(sprintf("Brier Score for Turbach: %10.5f\n", median(boot.correct[,6])))
cat(sprintf("Brier Score for Urseren: %10.5f\n", median(boot.correct[,7])))
cat(sprintf("Brier Score for Val Cluozza: %10.5f\n", median(boot.correct[,8])))
cat(sprintf("Brier Score for Val d'Entremont: %10.5f\n", median(boot.correct[,9])))
cat(sprintf("Brier Score for Val Piora: %10.5f\n", median(boot.correct[,10])))

sink()

print("Done BrierScore Bootstraping")
