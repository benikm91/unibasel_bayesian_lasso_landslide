# - load packages

library('ggplot2')
# - load packages
library('lmtest')
library('reshape')
library('gplots')
library('ROCR')


# -------- Arosa
k.boot=20

grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Arosa/Arosa.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  grpreg.model.Arosa <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Arosa)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Arosa/Arosa.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Arosa <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Arosa)
}

grpreg.model.Arosa <- grpreg.model
obs.Arosa <- obs

# -------- Baulmes
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Baulmes/Baulmes.model.2.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  grpreg.model.Baulmes <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Baulmes)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Baulmes/Baulmes.model.2.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Baulmes <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Baulmes)
}

grpreg.model.Baulmes <- grpreg.model
obs.Baulmes <- obs



# -------- Chrauchtal
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Chrauchtal/Chrauchtal.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  grpreg.model.Chrauchtal <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Chrauchtal)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Chrauchtal/Chrauchtal.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Chrauchtal <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Chrauchtal)
}

grpreg.model.Chrauchtal <- grpreg.model
obs.Chrauchtal <- obs



# -------- Hornbach
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Hornbach/Hornbach.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  grpreg.model.Hornbach <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Hornbach)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Hornbach/Hornbach.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Hornbach <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Hornbach)
}

grpreg.model.Hornbach <- grpreg.model
obs.Hornbach <- obs



# -------- Rappetal
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Rappetal/Rappetal.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  grpreg.model.Rappetal <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Rappetal)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Rappetal/Rappetal.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Rappetal <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Rappetal)
}

grpreg.model.Rappetal <- grpreg.model
obs.Rappetal <- obs



# -------- Turbach
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Turbach/Turbach.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  grpreg.model.Turbach <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Turbach)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Turbach/Turbach.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Turbach <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Turbach)
}

grpreg.model.Turbach <- grpreg.model
obs.Turbach <- obs



# -------- Urseren
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Urseren/Urseren.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  grpreg.model.Urseren <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Urseren)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Urseren/Urseren.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Urseren <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Urseren)
}

grpreg.model.Urseren <- grpreg.model
obs.Urseren <- obs



# -------- Val Cluozza Lambda.min
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Cluozza/Val_Cluozza.model.2.Gestk.cv.grpreg_grLasso_',j,'.rda', sep=""))
  grpreg.model.Val_Cluozza <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Val_Cluozza)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Cluozza/Val_Cluozza.model.2.Gestk.lslpts.observations_',j,'.rda', sep=""))
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
#   load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Cluozza/Val_Cluozza.model.2.cv.grpreg_grLasso_',j,'.rda', sep=""))
#   grpreg.model.Val_Cluozza <- as.numeric(cv.grpreg)
#   grpreg.model <- c(grpreg.model, grpreg.model.Val_Cluozza)
#   load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Cluozza/Val_Cluozza.model.2.lslpts.observations_',j,'.rda', sep=""))
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
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_D_Entremont/Val_D_Entremont.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  grpreg.model.Val_D_Entremont <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Val_D_Entremont)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_D_Entremont/Val_D_Entremont.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Val_D_Entremont <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Val_D_Entremont)
}

grpreg.model.Val_D_Entremont <- grpreg.model
obs.Val_D_Entremont <- obs



# -------- Val Piora
grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora/Val_Piora.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  grpreg.model.Val_Piora <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.Val_Piora)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora/Val_Piora.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Val_Piora <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.Val_Piora)
}

grpreg.model.Val_Piora <- grpreg.model
obs.Val_Piora <- obs





sink("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/Brier_Scores_grlasso_no_bootstrap_Gesteinsklasse.txt")

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

pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/bootstrapped_BS_all_Sites_Gesteinsklasse.pdf")

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


sink("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/Brier_Scores_grlasso_bootstrap_Gesteinsklasse.txt")

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




# ************************* performance scores ************************* 
# color for ROC curve 
colors <- c("#EBE06E","#BDDA66","#87D26F","#3EC77F","#00BA8F","#00A99C",
            "#0095A4","#007CA4","#005D9D","#22288E")

# ******** AROSA

predictions.model <- vector()
observations <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Arosa/Arosa.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  predictions.model.Arosa <- as.numeric(cv.grpreg)
  predictions.model <- c(predictions.model, predictions.model.Arosa)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Arosa/Arosa.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Arosa <- as.numeric(obs.grpreg)
  observations <- c(observations, obs.Arosa)
}


labels.Arosa <- as.data.frame(observations)
predictions.model.Arosa <- as.data.frame(predictions.model)

pred.model.Arosa <- prediction(predictions.model.Arosa, labels.Arosa)
perf.pred.model.Arosa <- performance(pred.model.Arosa, measure = "tpr", x.measure = "fpr") 

# - Confusion Matrix
CM.model.Arosa <- table(predictions.model.Arosa  >= 0.5 , observations)
Arosa <- as.vector(CM.model.Arosa)

# - Calculate Rates/Ratios
Bias.Arosa <- (Arosa[2]+Arosa[4])/(Arosa[3]+Arosa[4])
HR.Arosa <- Arosa[4]/(Arosa[4]+Arosa[3])
FARate.Arosa <- Arosa[2]/(Arosa[2]+Arosa[1])
FARatio.Arosa <- Arosa[2]/(Arosa[2]+Arosa[4])
PC.Arosa <- (Arosa[1]+Arosa[4])/(Arosa[1]+Arosa[2]+Arosa[3]+Arosa[4])

# - AUC / ROC curve
AUC.Arosa <- performance(pred.model.Arosa, measure = "auc")
auc.arosa <- round(as.numeric(AUC.Arosa@y.values),3)   
pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Arosa_Gesteinsklasse.pdf")
plot(perf.pred.model.Arosa, 
     colorize=TRUE, 
     colorize.palette=colors, 
     colorkey=FALSE, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.5, 1), 
     yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
abline(a=0, b= 1, lty=3, lwd=2)
legend('bottom', c(paste("AUC =", auc.arosa)), bty='n', cex=1.6)
dev.off()  



# ******** BAULMES

predictions.model <- vector()
observations <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Baulmes/Baulmes.model.2.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  predictions.model.Baulmes <- as.numeric(cv.grpreg)
  predictions.model <- c(predictions.model, predictions.model.Baulmes)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Baulmes/Baulmes.model.2.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Baulmes <- as.numeric(obs.grpreg)
  observations <- c(observations, obs.Baulmes)
}


labels.Baulmes <- as.data.frame(observations)
predictions.model.Baulmes <- as.data.frame(predictions.model)

pred.model.Baulmes <- prediction(predictions.model.Baulmes, labels.Baulmes)
perf.pred.model.Baulmes <- performance(pred.model.Baulmes, measure = "tpr", x.measure = "fpr") 

# - Confusion Matrix
CM.model.Baulmes <- table(predictions.model.Baulmes  >= 0.5 , observations)
Baulmes <- as.vector(CM.model.Baulmes)

# - Calculate Rates/Ratios
Bias.Baulmes <- (Baulmes[2]+Baulmes[4])/(Baulmes[3]+Baulmes[4])
HR.Baulmes <- Baulmes[4]/(Baulmes[4]+Baulmes[3])
FARate.Baulmes <- Baulmes[2]/(Baulmes[2]+Baulmes[1])
FARatio.Baulmes <- Baulmes[2]/(Baulmes[2]+Baulmes[4])
PC.Baulmes <- (Baulmes[1]+Baulmes[4])/(Baulmes[1]+Baulmes[2]+Baulmes[3]+Baulmes[4])

# - AUC / ROC curve
AUC.Baulmes <- performance(pred.model.Baulmes, measure = "auc")
auc.baulmes <- round(as.numeric(AUC.Baulmes@y.values),3)   
pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Baulmes_Gesteinsklasse.pdf")
plot(perf.pred.model.Baulmes, 
     colorize=TRUE, 
     colorize.palette=colors, 
     colorkey=FALSE, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.5, 1), 
     yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
abline(a=0, b= 1, lty=3, lwd=2)
legend('bottom', c(paste("AUC =", auc.baulmes)), bty='n', cex=1.6)
dev.off() 




# ******** CHRAUCHTAL

predictions.model <- vector()
observations <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Chrauchtal/Chrauchtal.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  predictions.model.Chrauchtal <- as.numeric(cv.grpreg)
  predictions.model <- c(predictions.model, predictions.model.Chrauchtal)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Chrauchtal/Chrauchtal.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Chrauchtal <- as.numeric(obs.grpreg)
  observations <- c(observations, obs.Chrauchtal)
}


labels.Chrauchtal <- as.data.frame(observations)
predictions.model.Chrauchtal <- as.data.frame(predictions.model)

pred.model.Chrauchtal <- prediction(predictions.model.Chrauchtal, labels.Chrauchtal)
perf.pred.model.Chrauchtal <- performance(pred.model.Chrauchtal, measure = "tpr", x.measure = "fpr") 

# - Confusion Matrix
CM.model.Chrauchtal <- table(predictions.model.Chrauchtal  >= 0.5 , observations)
Chrauchtal <- as.vector(CM.model.Chrauchtal)

# - Calculate Rates/Ratios
Bias.Chrauchtal <- (Chrauchtal[2]+Chrauchtal[4])/(Chrauchtal[3]+Chrauchtal[4])
HR.Chrauchtal <- Chrauchtal[4]/(Chrauchtal[4]+Chrauchtal[3])
FARate.Chrauchtal <- Chrauchtal[2]/(Chrauchtal[2]+Chrauchtal[1])
FARatio.Chrauchtal <- Chrauchtal[2]/(Chrauchtal[2]+Chrauchtal[4])
PC.Chrauchtal <- (Chrauchtal[1]+Chrauchtal[4])/(Chrauchtal[1]+Chrauchtal[2]+Chrauchtal[3]+Chrauchtal[4])

# - AUC / ROC curve
AUC.Chrauchtal <- performance(pred.model.Chrauchtal, measure = "auc")
auc.chrauchtal <- round(as.numeric(AUC.Chrauchtal@y.values),3)   
pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Chrauchtal_Gesteinsklasse.pdf")
plot(perf.pred.model.Chrauchtal, 
     colorize=TRUE, 
     colorize.palette=colors, 
     colorkey=FALSE, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.5, 1), 
     yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
abline(a=0, b= 1, lty=3, lwd=2)
legend('bottom', c(paste("AUC =", auc.chrauchtal)), bty='n', cex=1.6)
dev.off()  




# ******** HORNBACH

predictions.model <- vector()
observations <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Hornbach/Hornbach.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  predictions.model.Hornbach <- as.numeric(cv.grpreg)
  predictions.model <- c(predictions.model, predictions.model.Hornbach)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Hornbach/Hornbach.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Hornbach <- as.numeric(obs.grpreg)
  observations <- c(observations, obs.Hornbach)
}


labels.Hornbach <- as.data.frame(observations)
predictions.model.Hornbach <- as.data.frame(predictions.model)

pred.model.Hornbach <- prediction(predictions.model.Hornbach, labels.Hornbach)
perf.pred.model.Hornbach <- performance(pred.model.Hornbach, measure = "tpr", x.measure = "fpr") 

# - Confusion Matrix
CM.model.Hornbach <- table(predictions.model.Hornbach  >= 0.5 , observations)
Hornbach <- as.vector(CM.model.Hornbach)

# - Calculate Rates/Ratios
Bias.Hornbach <- (Hornbach[2]+Hornbach[4])/(Hornbach[3]+Hornbach[4])
HR.Hornbach <- Hornbach[4]/(Hornbach[4]+Hornbach[3])
FARate.Hornbach <- Hornbach[2]/(Hornbach[2]+Hornbach[1])
FARatio.Hornbach <- Hornbach[2]/(Hornbach[2]+Hornbach[4])
PC.Hornbach <- (Hornbach[1]+Hornbach[4])/(Hornbach[1]+Hornbach[2]+Hornbach[3]+Hornbach[4])

# - AUC / ROC curve
AUC.Hornbach <- performance(pred.model.Hornbach, measure = "auc")
auc.hornbach <- round(as.numeric(AUC.Hornbach@y.values),3)   
pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Hornbach_Gesteinsklasse.pdf")
plot(perf.pred.model.Hornbach, 
     colorize=TRUE, 
     colorize.palette=colors, 
     colorkey=FALSE, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.5, 1), 
     yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
abline(a=0, b= 1, lty=3, lwd=2)
legend('bottom', c(paste("AUC =", auc.hornbach)), bty='n', cex=1.6)
dev.off()  



# ******** RAPPETAL

predictions.model <- vector()
observations <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Rappetal/Rappetal.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  predictions.model.Rappetal <- as.numeric(cv.grpreg)
  predictions.model <- c(predictions.model, predictions.model.Rappetal)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Rappetal/Rappetal.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Rappetal <- as.numeric(obs.grpreg)
  observations <- c(observations, obs.Rappetal)
}


labels.Rappetal <- as.data.frame(observations)
predictions.model.Rappetal <- as.data.frame(predictions.model)

pred.model.Rappetal <- prediction(predictions.model.Rappetal, labels.Rappetal)
perf.pred.model.Rappetal <- performance(pred.model.Rappetal, measure = "tpr", x.measure = "fpr") 

# - Confusion Matrix
CM.model.Rappetal <- table(predictions.model.Rappetal  >= 0.5 , observations)
Rappetal <- as.vector(CM.model.Rappetal)

# - Calculate Rates/Ratios
Bias.Rappetal <- (Rappetal[2]+Rappetal[4])/(Rappetal[3]+Rappetal[4])
HR.Rappetal <- Rappetal[4]/(Rappetal[4]+Rappetal[3])
FARate.Rappetal <- Rappetal[2]/(Rappetal[2]+Rappetal[1])
FARatio.Rappetal <- Rappetal[2]/(Rappetal[2]+Rappetal[4])
PC.Rappetal <- (Rappetal[1]+Rappetal[4])/(Rappetal[1]+Rappetal[2]+Rappetal[3]+Rappetal[4])

# - AUC / ROC curve
AUC.Rappetal <- performance(pred.model.Rappetal, measure = "auc")
auc.rappetal <- round(as.numeric(AUC.Rappetal@y.values),3)   
pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Rappetal_Gesteinsklasse.pdf")
plot(perf.pred.model.Rappetal, 
     colorize=TRUE, 
     colorize.palette=colors, 
     colorkey=FALSE, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.5, 1), 
     yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
abline(a=0, b= 1, lty=3, lwd=2)
legend('bottom', c(paste("AUC =", auc.rappetal)), bty='n', cex=1.6)
dev.off()  



# ******** TURBACH

predictions.model <- vector()
observations <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Turbach/Turbach.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  predictions.model.Turbach <- as.numeric(cv.grpreg)
  predictions.model <- c(predictions.model, predictions.model.Turbach)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Turbach/Turbach.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Turbach <- as.numeric(obs.grpreg)
  observations <- c(observations, obs.Turbach)
}


labels.Turbach <- as.data.frame(observations)
predictions.model.Turbach <- as.data.frame(predictions.model)

pred.model.Turbach <- prediction(predictions.model.Turbach, labels.Turbach)
perf.pred.model.Turbach <- performance(pred.model.Turbach, measure = "tpr", x.measure = "fpr") 

# - Confusion Matrix
CM.model.Turbach <- table(predictions.model.Turbach  >= 0.5 , observations)
Turbach <- as.vector(CM.model.Turbach)

# - Calculate Rates/Ratios
Bias.Turbach <- (Turbach[2]+Turbach[4])/(Turbach[3]+Turbach[4])
HR.Turbach <- Turbach[4]/(Turbach[4]+Turbach[3])
FARate.Turbach <- Turbach[2]/(Turbach[2]+Turbach[1])
FARatio.Turbach <- Turbach[2]/(Turbach[2]+Turbach[4])
PC.Turbach <- (Turbach[1]+Turbach[4])/(Turbach[1]+Turbach[2]+Turbach[3]+Turbach[4])

# - AUC / ROC curve
AUC.Turbach <- performance(pred.model.Turbach, measure = "auc")
auc.turbach <- round(as.numeric(AUC.Turbach@y.values),3)   
pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Turbach_Gesteinsklasse.pdf")
plot(perf.pred.model.Turbach, 
     colorize=TRUE, 
     colorize.palette=colors, 
     colorkey=FALSE, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.5, 1), 
     yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
abline(a=0, b= 1, lty=3, lwd=2)
legend('bottom', c(paste("AUC =", auc.turbach)), bty='n', cex=1.6)
dev.off()  



# ******** URSEREN

predictions.model <- vector()
observations <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Urseren/Urseren.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  predictions.model.Urseren <- as.numeric(cv.grpreg)
  predictions.model <- c(predictions.model, predictions.model.Urseren)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Urseren/Urseren.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Urseren <- as.numeric(obs.grpreg)
  observations <- c(observations, obs.Urseren)
}


labels.Urseren <- as.data.frame(observations)
predictions.model.Urseren <- as.data.frame(predictions.model)

pred.model.Urseren <- prediction(predictions.model.Urseren, labels.Urseren)
perf.pred.model.Urseren <- performance(pred.model.Urseren, measure = "tpr", x.measure = "fpr") 

# - Confusion Matrix
CM.model.Urseren <- table(predictions.model.Urseren  >= 0.5 , observations)
Urseren <- as.vector(CM.model.Urseren)

# - Calculate Rates/Ratios
Bias.Urseren <- (Urseren[2]+Urseren[4])/(Urseren[3]+Urseren[4])
HR.Urseren <- Urseren[4]/(Urseren[4]+Urseren[3])
FARate.Urseren <- Urseren[2]/(Urseren[2]+Urseren[1])
FARatio.Urseren <- Urseren[2]/(Urseren[2]+Urseren[4])
PC.Urseren <- (Urseren[1]+Urseren[4])/(Urseren[1]+Urseren[2]+Urseren[3]+Urseren[4])

# - AUC / ROC curve
AUC.Urseren <- performance(pred.model.Urseren, measure = "auc")
auc.urseren <- round(as.numeric(AUC.Urseren@y.values),3)   
pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Urseren_Gesteinsklasse.pdf")
plot(perf.pred.model.Urseren, 
     colorize=TRUE, 
     colorize.palette=colors, 
     colorkey=FALSE, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.5, 1), 
     yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
abline(a=0, b= 1, lty=3, lwd=2)
legend('bottom', c(paste("AUC =", auc.urseren)), bty='n', cex=1.6)
dev.off()  



# VAL CLUOZZA
predictions.model <- vector()
observations <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Cluozza/Val_Cluozza.model.2.Gestk.cv.grpreg_grLasso_',j,'.rda', sep=""))
  predictions.model.Val.Cluozza <- as.numeric(cv.grpreg)
  predictions.model <- c(predictions.model, predictions.model.Val.Cluozza)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Cluozza/Val_Cluozza.model.2.Gestk.lslpts.observations_',j,'.rda', sep=""))
  obs.Val.Cluozza <- as.numeric(obs.grpreg)
  observations <- c(observations, obs.Val.Cluozza)
}


labels.Val.Cluozza <- as.data.frame(observations)
predictions.model.Val.Cluozza <- as.data.frame(predictions.model)

pred.model.Val.Cluozza <- prediction(predictions.model.Val.Cluozza, labels.Val.Cluozza)
perf.pred.model.Val.Cluozza <- performance(pred.model.Val.Cluozza, measure = "tpr", x.measure = "fpr") 

# - Confusion Matrix
CM.model.Val.Cluozza <- table(predictions.model.Val.Cluozza  >= 0.5 , observations)
Val.Cluozza <- as.vector(CM.model.Val.Cluozza)

# - Calculate Rates/Ratios
Bias.Val.Cluozza <- (Val.Cluozza[2]+Val.Cluozza[4])/(Val.Cluozza[3]+Val.Cluozza[4])
HR.Val.Cluozza <- Val.Cluozza[4]/(Val.Cluozza[4]+Val.Cluozza[3])
FARate.Val.Cluozza <- Val.Cluozza[2]/(Val.Cluozza[2]+Val.Cluozza[1])
FARatio.Val.Cluozza <- Val.Cluozza[2]/(Val.Cluozza[2]+Val.Cluozza[4])
PC.Val.Cluozza <- (Val.Cluozza[1]+Val.Cluozza[4])/(Val.Cluozza[1]+Val.Cluozza[2]+Val.Cluozza[3]+Val.Cluozza[4])

# - AUC / ROC curve
AUC.Val.Cluozza <- performance(pred.model.Val.Cluozza, measure = "auc")
auc.val.cluozza <- round(as.numeric(AUC.Val.Cluozza@y.values),3)   
pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Val_Cluozza_Gesteinsklasse.pdf")
plot(perf.pred.model.Val.Cluozza, 
     colorize=TRUE, 
     colorize.palette=colors, 
     colorkey=FALSE, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.5, 1), 
     yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
abline(a=0, b= 1, lty=3, lwd=2)
legend('bottom', c(paste("AUC =", auc.val.cluozza)), bty='n', cex=1.6)
dev.off()  






# VAL D'ENTREMONT
predictions.model <- vector()
observations <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_D_Entremont/Val_D_Entremont.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  predictions.model.Val.D.Entremont <- as.numeric(cv.grpreg)
  predictions.model <- c(predictions.model, predictions.model.Val.D.Entremont)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_D_Entremont/Val_D_Entremont.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Val.D.Entremont <- as.numeric(obs.grpreg)
  observations <- c(observations, obs.Val.D.Entremont)
}


labels.Val.D.Entremont <- as.data.frame(observations)
predictions.model.Val.D.Entremont <- as.data.frame(predictions.model)

pred.model.Val.D.Entremont <- prediction(predictions.model.Val.D.Entremont, labels.Val.D.Entremont)
perf.pred.model.Val.D.Entremont <- performance(pred.model.Val.D.Entremont, measure = "tpr", x.measure = "fpr") 

# - Confusion Matrix
CM.model.Val.D.Entremont <- table(predictions.model.Val.D.Entremont  >= 0.5 , observations)
Val.D.Entremont <- as.vector(CM.model.Val.D.Entremont)

# - Calculate Rates/Ratios
Bias.Val.D.Entremont <- (Val.D.Entremont[2]+Val.D.Entremont[4])/(Val.D.Entremont[3]+Val.D.Entremont[4])
HR.Val.D.Entremont <- Val.D.Entremont[4]/(Val.D.Entremont[4]+Val.D.Entremont[3])
FARate.Val.D.Entremont <- Val.D.Entremont[2]/(Val.D.Entremont[2]+Val.D.Entremont[1])
FARatio.Val.D.Entremont <- Val.D.Entremont[2]/(Val.D.Entremont[2]+Val.D.Entremont[4])
PC.Val.D.Entremont <- (Val.D.Entremont[1]+Val.D.Entremont[4])/(Val.D.Entremont[1]+Val.D.Entremont[2]+Val.D.Entremont[3]+Val.D.Entremont[4])

# - AUC / ROC curve
AUC.Val.D.Entremont <- performance(pred.model.Val.D.Entremont, measure = "auc")
auc.val.d.entremont <- round(as.numeric(AUC.Val.D.Entremont@y.values),3)   
pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Val_D_Entremont_Gesteinsklasse.pdf")
plot(perf.pred.model.Val.D.Entremont, 
     colorize=TRUE, 
     colorize.palette=colors, 
     colorkey=FALSE, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.5, 1), 
     yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
abline(a=0, b= 1, lty=3, lwd=2)
legend('bottom', c(paste("AUC =", auc.val.d.entremont)), bty='n', cex=1.6)
dev.off()  




# VAL PIORA
predictions.model <- vector()
observations <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora/Val_Piora.model.1.Gestkl.cv.grpreg_grLasso_',j,'.rda', sep=""))
  predictions.model.Val.Piora <- as.numeric(cv.grpreg)
  predictions.model <- c(predictions.model, predictions.model.Val.Piora)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora/Val_Piora.model.1.Gestkl.lslpts.observations_',j,'.rda', sep=""))
  obs.Val.Piora <- as.numeric(obs.grpreg)
  observations <- c(observations, obs.Val.Piora)
}


labels.Val.Piora <- as.data.frame(observations)
predictions.model.Val.Piora <- as.data.frame(predictions.model)

pred.model.Val.Piora <- prediction(predictions.model.Val.Piora, labels.Val.Piora)
perf.pred.model.Val.Piora <- performance(pred.model.Val.Piora, measure = "tpr", x.measure = "fpr") 

# - Confusion Matrix
CM.model.Val.Piora <- table(predictions.model.Val.Piora  >= 0.5 , observations)
Val.Piora <- as.vector(CM.model.Val.Piora)

# - Calculate Rates/Ratios
Bias.Val.Piora <- (Val.Piora[2]+Val.Piora[4])/(Val.Piora[3]+Val.Piora[4])
HR.Val.Piora <- Val.Piora[4]/(Val.Piora[4]+Val.Piora[3])
FARate.Val.Piora <- Val.Piora[2]/(Val.Piora[2]+Val.Piora[1])
FARatio.Val.Piora <- Val.Piora[2]/(Val.Piora[2]+Val.Piora[4])
PC.Val.Piora <- (Val.Piora[1]+Val.Piora[4])/(Val.Piora[1]+Val.Piora[2]+Val.Piora[3]+Val.Piora[4])

# - AUC / ROC curve
AUC.Val.Piora <- performance(pred.model.Val.Piora, measure = "auc")
auc.val.piora <- round(as.numeric(AUC.Val.Piora@y.values),3)   
pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Val_Piora_Gesteinsklasse.pdf")
plot(perf.pred.model.Val.Piora, 
     colorize=TRUE, 
     colorize.palette=colors, 
     colorkey=FALSE, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.5, 1), 
     yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
abline(a=0, b= 1, lty=3, lwd=2)
legend('bottom', c(paste("AUC =", auc.val.piora)), bty='n', cex=1.6)
dev.off()  

# Plot all sites in one graph; ROC & AUC


colors=c("#003B01", "#005251", "#005D7B", "#3F5DA1", "#9353B6", "#CB4FB0", "#EB6492", "#F98C5E", "#F8BD00", "#F0E400")

labels=c(paste("Arosa, AUC =",auc.arosa), paste("Baulmes, AUC =",auc.baulmes), paste("Chrauchtal, AUC =",auc.chrauchtal), paste("Hornbach, AUC =",auc.hornbach),
         paste("Rappetal, AUC =",auc.rappetal), paste("Turbach, AUC =",auc.turbach), paste("Urseren, AUC =",auc.urseren),paste("Val Cluozza, AUC =",auc.val.cluozza),
         paste("Val d'Entremont, AUC =",auc.val.d.entremont), paste("Val Piora, AUC =",auc.val.piora))


pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_All_in_One_Plot_Gesteinsklasse.pdf")
par(cex.axis=1.3)
plot(perf.pred.model.Arosa, col=colors[1], lwd=2.8, cex.lab=1.3)
plot(perf.pred.model.Baulmes, add = TRUE, col=colors[2], lwd=2.8)
plot(perf.pred.model.Chrauchtal, add = TRUE, col=colors[3], lwd=2.8)
plot(perf.pred.model.Hornbach, add = TRUE, col=colors[4], lwd=2.8)
plot(perf.pred.model.Rappetal, add = TRUE, col=colors[5], lwd=2.8)
plot(perf.pred.model.Turbach, add = TRUE, col=colors[6], lwd=2.8)
plot(perf.pred.model.Urseren, add = TRUE, col=colors[7], lwd=2.8)
plot(perf.pred.model.Val.Cluozza, add = TRUE, col=colors[8], lwd=2.8)
plot(perf.pred.model.Val.D.Entremont, add = TRUE, col=colors[9], lwd=2.8)
plot(perf.pred.model.Val.Piora, add = TRUE, col=colors[10], lwd=2.8)
legend('bottomright', labels, col=colors, bty="n", lty = 1, lwd=3, cex=1.1)
abline(a=0, b= 1, lty=3, lwd=1)
text(x= 0.185, y= 0.7 , labels = "A", cex = 0.9, col = "#003B01", font=2)
text(x= 0.46, y= 0.76 , labels = "B", cex = 0.9, col = "#005251", font=2)
text(x= 0.42, y= 0.86 , labels = "C", cex = 0.9, col = "#005D7B", font=2)
text(x= 0.7, y= 0.93 , labels = "H", cex = 0.9, col = "#3F5DA1", font=2)
text(x= 0.38, y= 0.895 , labels = "R", cex = 0.9, col = "#9353B6", font=2)
text(x= 0.46, y= 0.93 , labels = "T", cex = 0.9, col = "#CB4FB0", font=2)
text(x= 0.25, y= 0.88 , labels = "U", cex = 0.9, col = "#EB6492", font=2)
text(x= 0.5, y=0.858 , labels = "VC", cex = 0.9, col = "#F98C5E", font=2)
text(x= 0.25, y= 0.62 , labels = "VE", cex = 0.9, col = "#F8BD00", font=2)
text(x=0.25 , y=0.816 , labels = "VP", cex = 0.9, col = "#DFD553", font=2)
dev.off()  






# - Print all scores to txt file of all 10 sites

sink("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/Contingency_Table_results_Gesteinsklasse.txt")

cat("=========  Contingency Matrix with 0.5 Probability threshold ================\n")
cat("=========  Bias = (total forecast yes) / (total observed yes)  B >= 1, overforecast; B <= 1, underforecast \n")
cat("=========  Hit Rate =============\n")
cat("=========  FARate (False Alarm Ratio) ============\n")
cat("=========  FARatio  (False Alarm Rate) ============\n")
cat("=========  PC / Accuracy ================\n")

cat("=========  Arosa ================\n")
print(CM.model.Arosa)
print(Bias.Arosa)
print(HR.Arosa)
print(FARate.Arosa)
print(FARatio.Arosa)
print(PC.Arosa)

cat("=============  Baulmes ================\n")
print(CM.model.Baulmes)
print(Bias.Baulmes)
print(HR.Baulmes)
print(FARate.Baulmes)
print(FARatio.Baulmes)
print(PC.Baulmes)


cat("=============  Chrauchtal ================\n")
print(CM.model.Chrauchtal)
print(Bias.Chrauchtal)
print(HR.Chrauchtal)
print(FARate.Chrauchtal)
print(FARatio.Chrauchtal)
print(PC.Chrauchtal)

cat("=============  Hornbach ================\n")
print(CM.model.Hornbach)
print(Bias.Hornbach)
print(HR.Hornbach)
print(FARate.Hornbach)
print(FARatio.Hornbach)
print(PC.Hornbach)

cat("=============  Rappetal ================\n")
print(CM.model.Rappetal)
print(Bias.Rappetal)
print(HR.Rappetal)
print(FARate.Rappetal)
print(FARatio.Rappetal)
print(PC.Rappetal)

cat("=============  Turbach ================\n")
print(CM.model.Turbach)
print(Bias.Turbach)
print(HR.Turbach)
print(FARate.Turbach)
print(FARatio.Turbach)
print(PC.Turbach)

cat("=============  Urseren ================\n")
print(CM.model.Urseren)
print(Bias.Urseren)
print(HR.Urseren)
print(FARate.Urseren)
print(FARatio.Urseren)
print(PC.Urseren)

cat("=============  Val Cluozza ================\n")
print(CM.model.Val.Cluozza)
print(Bias.Val.Cluozza)
print(HR.Val.Cluozza)
print(FARate.Val.Cluozza)
print(FARatio.Val.Cluozza)
print(PC.Val.Cluozza)

cat("=============  Val d'Entremont ================\n")
print(CM.model.Val.D.Entremont)
print(Bias.Val.D.Entremont)
print(HR.Val.D.Entremont)
print(FARate.Val.D.Entremont)
print(FARatio.Val.D.Entremont)
print(PC.Val.D.Entremont)

cat("=============  Val Piora ================\n")
print(CM.model.Val.Piora)
print(Bias.Val.Piora)
print(HR.Val.Piora)
print(FARate.Val.Piora)
print(FARatio.Val.Piora)
print(PC.Val.Piora)

sink()

