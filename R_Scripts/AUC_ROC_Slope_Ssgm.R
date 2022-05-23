# - load packages

library('ggplot2')
# - load packages
library('lmtest')
library('reshape')
library('gplots')
library('ROCR')



# ************************* performance scores ************************* 
# color for ROC curve 
#colors <- c("#EBE06E","#BDDA66","#87D26F","#3EC77F","#00BA8F","#00A99C","#0095A4","#007CA4","#005D9D","#22288E")

# ******** AROSA
# SLOPE
predictions.model.slope <- vector()
observations.slope <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Arosa/Arosa.model.1.only.slope.glm_',j,'.rda', sep=""))
  predictions.model.slope.Arosa <- as.numeric(glm)
  predictions.model.slope <- c(predictions.model.slope, predictions.model.slope.Arosa)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Arosa/Arosa.model.1.only.slope.lslpts.obs_',j,'.rda', sep=""))
  obs.slope.Arosa <- as.numeric(obs.glm)
  observations.slope <- c(observations.slope, obs.slope.Arosa)
}


labels.slope.Arosa <- as.data.frame(observations.slope)
predictions.model.slope.Arosa <- as.data.frame(predictions.model.slope)

pred.model.slope.Arosa <- prediction(predictions.model.slope.Arosa, labels.slope.Arosa)
perf.pred.model.slope.Arosa <- performance(pred.model.slope.Arosa, measure = "tpr", x.measure = "fpr") 


# - AUC / ROC curve
AUC.Arosa.slope <- performance(pred.model.slope.Arosa, measure = "auc")
auc.slope.Arosa <- round(as.numeric(AUC.Arosa.slope@y.values),3)   






# SSGM
predictions.model.ssgm <- vector()
observations.ssgm <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Arosa/Arosa.model.1.only.ssgm.glm_',j,'.rda', sep=""))
  predictions.model.ssgm.Arosa <- as.numeric(glm)
  predictions.model.ssgm <- c(predictions.model.ssgm, predictions.model.ssgm.Arosa)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Arosa/Arosa.model.1.only.ssgm.lslpts.obs_',j,'.rda', sep=""))
  obs.ssgm.Arosa <- as.numeric(obs.glm)
  observations.ssgm <- c(observations.ssgm, obs.ssgm.Arosa)
}


labels.ssgm.Arosa <- as.data.frame(observations.ssgm)
predictions.model.ssgm.Arosa <- as.data.frame(predictions.model.ssgm)

pred.model.ssgm.Arosa <- prediction(predictions.model.ssgm.Arosa, labels.ssgm.Arosa)
perf.pred.model.ssgm.Arosa <- performance(pred.model.ssgm.Arosa, measure = "tpr", x.measure = "fpr") 

# - AUC / ROC curve
AUC.Arosa.ssgm <- performance(pred.model.ssgm.Arosa, measure = "auc")
auc.ssgm.Arosa <- round(as.numeric(AUC.Arosa.ssgm@y.values),3)
# 
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Arosa_onlySsgm.pdf")
# par(mfrow=c(1,1))
# plot(perf.pred.model.ssgm.Arosa, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.ssgm.Arosa)), bty='n', cex=1.6)
# dev.off()  


# ******** BAULMES
# SLOPE
predictions.model.slope <- vector()
observations.slope <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Baulmes/Baulmes.model.1.only.slope.glm_',j,'.rda', sep=""))
  predictions.model.slope.Baulmes <- as.numeric(glm)
  predictions.model.slope <- c(predictions.model.slope, predictions.model.slope.Baulmes)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Baulmes/Baulmes.model.1.only.slope.lslpts.obs_',j,'.rda', sep=""))
  obs.slope.Baulmes <- as.numeric(obs.glm)
  observations.slope <- c(observations.slope, obs.slope.Baulmes)
}


labels.slope.Baulmes <- as.data.frame(observations.slope)
predictions.model.slope.Baulmes <- as.data.frame(predictions.model.slope)

pred.model.slope.Baulmes <- prediction(predictions.model.slope.Baulmes, labels.slope.Baulmes)
perf.pred.model.slope.Baulmes <- performance(pred.model.slope.Baulmes, measure = "tpr", x.measure = "fpr") 

# # - AUC / ROC curve
 AUC.Baulmes.slope <- performance(pred.model.slope.Baulmes, measure = "auc")
 auc.slope.Baulmes <- round(as.numeric(AUC.Baulmes.slope@y.values),3)   

# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Baulmes_onlySlope.pdf")
# plot(perf.pred.model.slope.Baulmes, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.slope)), bty='n', cex=1.6)
# dev.off()  

# SSGM
predictions.model.ssgm <- vector()
observations.ssgm <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Baulmes/Baulmes.model.1.only.ssgm.glm_',j,'.rda', sep=""))
  predictions.model.ssgm.Baulmes <- as.numeric(glm)
  predictions.model.ssgm <- c(predictions.model.ssgm, predictions.model.ssgm.Baulmes)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Baulmes/Baulmes.model.1.only.ssgm.lslpts.obs_',j,'.rda', sep=""))
  obs.ssgm.Baulmes <- as.numeric(obs.glm)
  observations.ssgm <- c(observations.ssgm, obs.ssgm.Baulmes)
}


labels.ssgm.Baulmes <- as.data.frame(observations.ssgm)
predictions.model.ssgm.Baulmes <- as.data.frame(predictions.model.ssgm)

pred.model.ssgm.Baulmes <- prediction(predictions.model.ssgm.Baulmes, labels.ssgm.Baulmes)
perf.pred.model.ssgm.Baulmes <- performance(pred.model.ssgm.Baulmes, measure = "tpr", x.measure = "fpr") 

# # - AUC / ROC curve
AUC.Baulmes.ssgm <- performance(pred.model.ssgm.Baulmes, measure = "auc")
auc.ssgm.Baulmes <- round(as.numeric(AUC.Baulmes.ssgm@y.values),3)

# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Baulmes_onlySsgm.pdf")
# par(mfrow=c(1,1))
# plot(perf.pred.model.ssgm.Baulmes, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.ssgm)), bty='n', cex=1.6)
# dev.off()  


# ******** CHRAUCHTAL
# SLOPE
predictions.model.slope <- vector()
observations.slope <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Chrauchtal/Chrauchtal.model.1.only.slope.glm_',j,'.rda', sep=""))
  predictions.model.slope.Chrauchtal <- as.numeric(glm)
  predictions.model.slope <- c(predictions.model.slope, predictions.model.slope.Chrauchtal)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Chrauchtal/Chrauchtal.model.1.only.slope.lslpts.obs_',j,'.rda', sep=""))
  obs.slope.Chrauchtal <- as.numeric(obs.glm)
  observations.slope <- c(observations.slope, obs.slope.Chrauchtal)
}


labels.slope.Chrauchtal <- as.data.frame(observations.slope)
predictions.model.slope.Chrauchtal <- as.data.frame(predictions.model.slope)

pred.model.slope.Chrauchtal <- prediction(predictions.model.slope.Chrauchtal, labels.slope.Chrauchtal)
perf.pred.model.slope.Chrauchtal<- performance(pred.model.slope.Chrauchtal, measure = "tpr", x.measure = "fpr") 
# 
# # - AUC / ROC curve
 AUC.Chrauchtal.slope <- performance(pred.model.slope.Chrauchtal, measure = "auc")
 auc.slope.Chrauchtal <- round(as.numeric(AUC.Chrauchtal.slope@y.values),3)   

# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Chrauchtal_onlySlope.pdf")
# plot(perf.pred.model.slope.Chrauchtal, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.slope)), bty='n', cex=1.6)
# dev.off()  

# SSGM
predictions.model.ssgm <- vector()
observations.ssgm <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Chrauchtal/Chrauchtal.model.1.only.ssgm.glm_',j,'.rda', sep=""))
  predictions.model.ssgm.Chrauchtal <- as.numeric(glm)
  predictions.model.ssgm <- c(predictions.model.ssgm, predictions.model.ssgm.Chrauchtal)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Chrauchtal/Chrauchtal.model.1.only.ssgm.lslpts.obs_',j,'.rda', sep=""))
  obs.ssgm.Chrauchtal <- as.numeric(obs.glm)
  observations.ssgm <- c(observations.ssgm, obs.ssgm.Chrauchtal)
}


labels.ssgm.Chrauchtal <- as.data.frame(observations.ssgm)
predictions.model.ssgm.Chrauchtal <- as.data.frame(predictions.model.ssgm)

pred.model.ssgm.Chrauchtal <- prediction(predictions.model.ssgm.Chrauchtal, labels.ssgm.Chrauchtal)
perf.pred.model.ssgm.Chrauchtal <- performance(pred.model.ssgm.Chrauchtal, measure = "tpr", x.measure = "fpr") 
# 
# # - AUC / ROC curve
AUC.Chrauchtal.ssgm <- performance(pred.model.ssgm.Chrauchtal, measure = "auc")
auc.ssgm.Chrauchtal <- round(as.numeric(AUC.Chrauchtal.ssgm@y.values),3) 

# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Chrauchtal_onlySsgm.pdf")
# par(mfrow=c(1,1))
# plot(perf.pred.model.ssgm.Chrauchtal, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.ssgm)), bty='n', cex=1.6)
# dev.off()  



# ******** HORNBACH
# SLOPE
predictions.model.slope <- vector()
observations.slope <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Hornbach/Hornbach.model.1.only.slope.glm_',j,'.rda', sep=""))
  predictions.model.slope.Hornbach <- as.numeric(glm)
  predictions.model.slope <- c(predictions.model.slope, predictions.model.slope.Hornbach)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Hornbach/Hornbach.model.1.only.slope.lslpts.obs_',j,'.rda', sep=""))
  obs.slope.Hornbach <- as.numeric(obs.glm)
  observations.slope <- c(observations.slope, obs.slope.Hornbach)
}


labels.slope.Hornbach <- as.data.frame(observations.slope)
predictions.model.slope.Hornbach <- as.data.frame(predictions.model.slope)

pred.model.slope.Hornbach <- prediction(predictions.model.slope.Hornbach, labels.slope.Hornbach)
perf.pred.model.slope.Hornbach <- performance(pred.model.slope.Hornbach, measure = "tpr", x.measure = "fpr") 

# # - AUC / ROC curve
AUC.Hornbach.slope <- performance(pred.model.slope.Hornbach, measure = "auc")
auc.slope.Hornbach <- round(as.numeric(AUC.Hornbach.slope@y.values),3)   

# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Hornbach_onlySlope.pdf")
# plot(perf.pred.model.slope.Hornbach, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.slope)), bty='n', cex=1.6)
# dev.off()  

# SSGM
predictions.model.ssgm <- vector()
observations.ssgm <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Hornbach/Hornbach.model.1.only.ssgm.glm_',j,'.rda', sep=""))
  predictions.model.ssgm.Hornbach <- as.numeric(glm)
  predictions.model.ssgm <- c(predictions.model.ssgm, predictions.model.ssgm.Hornbach)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Hornbach/Hornbach.model.1.only.ssgm.lslpts.obs_',j,'.rda', sep=""))
  obs.ssgm.Hornbach <- as.numeric(obs.glm)
  observations.ssgm <- c(observations.ssgm, obs.ssgm.Hornbach)
}


labels.ssgm.Hornbach <- as.data.frame(observations.ssgm)
predictions.model.ssgm.Hornbach <- as.data.frame(predictions.model.ssgm)

pred.model.ssgm.Hornbach <- prediction(predictions.model.ssgm.Hornbach, labels.ssgm.Hornbach)
perf.pred.model.ssgm.Hornbach <- performance(pred.model.ssgm.Hornbach, measure = "tpr", x.measure = "fpr") 

# # - AUC / ROC curve
AUC.Hornbach.ssgm <- performance(pred.model.ssgm.Hornbach, measure = "auc")
auc.ssgm.Hornbach <- round(as.numeric(AUC.Hornbach.ssgm@y.values),3)

# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Hornbach_onlySsgm.pdf")
# par(mfrow=c(1,1))
# plot(perf.pred.model.ssgm.Hornbach, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.ssgm)), bty='n', cex=1.6)
# dev.off()  



# ******** RAPPETAL

# SLOPE
predictions.model.slope <- vector()
observations.slope <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Rappetal/Rappetal.model.1.only.slope.glm_',j,'.rda', sep=""))
  predictions.model.slope.Rappetal <- as.numeric(glm)
  predictions.model.slope <- c(predictions.model.slope, predictions.model.slope.Rappetal)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Rappetal/Rappetal.model.1.only.slope.lslpts.obs_',j,'.rda', sep=""))
  obs.slope.Rappetal <- as.numeric(obs.glm)
  observations.slope <- c(observations.slope, obs.slope.Rappetal)
}


labels.slope.Rappetal <- as.data.frame(observations.slope)
predictions.model.slope.Rappetal <- as.data.frame(predictions.model.slope)

pred.model.slope.Rappetal <- prediction(predictions.model.slope.Rappetal, labels.slope.Rappetal)
perf.pred.model.slope.Rappetal <- performance(pred.model.slope.Rappetal, measure = "tpr", x.measure = "fpr") 

# # - AUC / ROC curve
AUC.Rappetal.slope <- performance(pred.model.slope.Rappetal, measure = "auc")
auc.slope.Rappetal <- round(as.numeric(AUC.Rappetal.slope@y.values),3)
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Rappetal_onlySlope.pdf")
# plot(perf.pred.model.slope.Rappetal, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.slope)), bty='n', cex=1.6)
# dev.off()  

# SSGM
predictions.model.ssgm <- vector()
observations.ssgm <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Rappetal/Rappetal.model.1.only.ssgm.glm_',j,'.rda', sep=""))
  predictions.model.ssgm.Rappetal <- as.numeric(glm)
  predictions.model.ssgm <- c(predictions.model.ssgm, predictions.model.ssgm.Rappetal)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Rappetal/Rappetal.model.1.only.ssgm.lslpts.obs_',j,'.rda', sep=""))
  obs.ssgm.Rappetal <- as.numeric(obs.glm)
  observations.ssgm <- c(observations.ssgm, obs.ssgm.Rappetal)
}


labels.ssgm.Rappetal <- as.data.frame(observations.ssgm)
predictions.model.ssgm.Rappetal <- as.data.frame(predictions.model.ssgm)

pred.model.ssgm.Rappetal <- prediction(predictions.model.ssgm.Rappetal, labels.ssgm.Rappetal)
perf.pred.model.ssgm.Rappetal <- performance(pred.model.ssgm.Rappetal, measure = "tpr", x.measure = "fpr") 

# # - AUC / ROC curve
AUC.Rappetal.ssgm <- performance(pred.model.ssgm.Rappetal, measure = "auc")
auc.ssgm.Rappetal <- round(as.numeric(AUC.Rappetal.ssgm@y.values),3)
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Rappetal_onlySsgm.pdf")
# par(mfrow=c(1,1))
# plot(perf.pred.model.ssgm.Rappetal, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.ssgm)), bty='n', cex=1.6)
# dev.off()  




# ******** TURBACH

# SLOPE
predictions.model.slope <- vector()
observations.slope <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Turbach/Turbach.model.1.only.slope.glm_',j,'.rda', sep=""))
  predictions.model.slope.Turbach <- as.numeric(glm)
  predictions.model.slope <- c(predictions.model.slope, predictions.model.slope.Turbach)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Turbach/Turbach.model.1.only.slope.lslpts.obs_',j,'.rda', sep=""))
  obs.slope.Turbach <- as.numeric(obs.glm)
  observations.slope <- c(observations.slope, obs.slope.Turbach)
}


labels.slope.Turbach <- as.data.frame(observations.slope)
predictions.model.slope.Turbach <- as.data.frame(predictions.model.slope)

pred.model.slope.Turbach <- prediction(predictions.model.slope.Turbach, labels.slope.Turbach)
perf.pred.model.slope.Turbach <- performance(pred.model.slope.Turbach, measure = "tpr", x.measure = "fpr") 

# # - AUC / ROC curve
AUC.Turbach.slope <- performance(pred.model.slope.Turbach, measure = "auc")
auc.slope.Turbach <- round(as.numeric(AUC.Turbach.slope@y.values),3)
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Turbach_onlySlope.pdf")
# plot(perf.pred.model.slope.Turbach, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.slope)), bty='n', cex=1.6)
# dev.off()  

# SSGM
predictions.model.ssgm <- vector()
observations.ssgm <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Turbach/Turbach.model.1.only.ssgm.glm_',j,'.rda', sep=""))
  predictions.model.ssgm.Turbach <- as.numeric(glm)
  predictions.model.ssgm <- c(predictions.model.ssgm, predictions.model.ssgm.Turbach)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Turbach/Turbach.model.1.only.ssgm.lslpts.obs_',j,'.rda', sep=""))
  obs.ssgm.Turbach <- as.numeric(obs.glm)
  observations.ssgm <- c(observations.ssgm, obs.ssgm.Turbach)
}


labels.ssgm.Turbach <- as.data.frame(observations.ssgm)
predictions.model.ssgm.Turbach <- as.data.frame(predictions.model.ssgm)

pred.model.ssgm.Turbach <- prediction(predictions.model.ssgm.Turbach, labels.ssgm.Turbach)
perf.pred.model.ssgm.Turbach <- performance(pred.model.ssgm.Turbach, measure = "tpr", x.measure = "fpr") 

# # - AUC / ROC curve
AUC.Turbach.ssgm <- performance(pred.model.ssgm.Turbach, measure = "auc")
auc.ssgm.Turbach <- round(as.numeric(AUC.Turbach.ssgm@y.values),3)
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Turbach_onlySsgm.pdf")
# par(mfrow=c(1,1))
# plot(perf.pred.model.ssgm.Turbach, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.ssgm)), bty='n', cex=1.6)
# dev.off()  




# ******** URSEREN


# SLOPE
predictions.model.slope <- vector()
observations.slope <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Urseren/Urseren.model.1.only.slope.glm_',j,'.rda', sep=""))
  predictions.model.slope.Urseren <- as.numeric(glm)
  predictions.model.slope <- c(predictions.model.slope, predictions.model.slope.Urseren)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Urseren/Urseren.model.1.only.slope.lslpts.obs_',j,'.rda', sep=""))
  obs.slope.Urseren <- as.numeric(obs.glm)
  observations.slope <- c(observations.slope, obs.slope.Urseren)
}


labels.slope.Urseren <- as.data.frame(observations.slope)
predictions.model.slope.Urseren <- as.data.frame(predictions.model.slope)

pred.model.slope.Urseren <- prediction(predictions.model.slope.Urseren, labels.slope.Urseren)
perf.pred.model.slope.Urseren <- performance(pred.model.slope.Urseren, measure = "tpr", x.measure = "fpr") 

# # - AUC / ROC curve
AUC.Urseren.slope <- performance(pred.model.slope.Urseren, measure = "auc")
auc.slope.Urseren <- round(as.numeric(AUC.Urseren.slope@y.values),3)
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Urseren_onlySlope.pdf")
# plot(perf.pred.model.slope.Urseren, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.slope)), bty='n', cex=1.6)
# dev.off()  

# SSGM
predictions.model.ssgm <- vector()
observations.ssgm <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Urseren/Urseren.model.1.only.ssgm.glm_',j,'.rda', sep=""))
  predictions.model.ssgm.Urseren <- as.numeric(glm)
  predictions.model.ssgm <- c(predictions.model.ssgm, predictions.model.ssgm.Urseren)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Urseren/Urseren.model.1.only.ssgm.lslpts.obs_',j,'.rda', sep=""))
  obs.ssgm.Urseren <- as.numeric(obs.glm)
  observations.ssgm <- c(observations.ssgm, obs.ssgm.Urseren)
}


labels.ssgm.Urseren <- as.data.frame(observations.ssgm)
predictions.model.ssgm.Urseren <- as.data.frame(predictions.model.ssgm)

pred.model.ssgm.Urseren <- prediction(predictions.model.ssgm.Urseren, labels.ssgm.Urseren)
perf.pred.model.ssgm.Urseren <- performance(pred.model.ssgm.Urseren, measure = "tpr", x.measure = "fpr") 

# # - AUC / ROC curve
AUC.Urseren.ssgm <- performance(pred.model.ssgm.Urseren, measure = "auc")
auc.ssgm.Urseren <- round(as.numeric(AUC.Urseren.ssgm@y.values),3)
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Urseren_onlySsgm.pdf")
# par(mfrow=c(1,1))
# plot(perf.pred.model.ssgm.Urseren, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.ssgm)), bty='n', cex=1.6)
# dev.off()  





# VAL CLUOZZA

# SLOPE
predictions.model.slope <- vector()
observations.slope <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Cluozza/Val_Cluozza.model.1.only.slope.glm_',j,'.rda', sep=""))
  predictions.model.slope.Val_Cluozza <- as.numeric(glm)
  predictions.model.slope <- c(predictions.model.slope, predictions.model.slope.Val_Cluozza)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Cluozza/Val_Cluozza.model.1.only.slope.lslpts.obs_',j,'.rda', sep=""))
  obs.slope.Val_Cluozza <- as.numeric(obs.glm)
  observations.slope <- c(observations.slope, obs.slope.Val_Cluozza)
}


labels.slope.Val_Cluozza <- as.data.frame(observations.slope)
predictions.model.slope.Val_Cluozza <- as.data.frame(predictions.model.slope)

pred.model.slope.Val_Cluozza <- prediction(predictions.model.slope.Val_Cluozza, labels.slope.Val_Cluozza)
perf.pred.model.slope.Val_Cluozza <- performance(pred.model.slope.Val_Cluozza, measure = "tpr", x.measure = "fpr") 

# # - AUC / ROC curve
AUC.Val_Cluozza.slope <- performance(pred.model.slope.Val_Cluozza, measure = "auc")
auc.slope.Val_Cluozza <- round(as.numeric(AUC.Val_Cluozza.slope@y.values),3)
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Val_Cluozza_onlySlope.pdf")
# plot(perf.pred.model.slope.Val_Cluozza, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.slope)), bty='n', cex=1.6)
# dev.off()  

# SSGM
predictions.model.ssgm <- vector()
observations.ssgm <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Cluozza/Val_Cluozza.model.1.only.ssgm.glm_',j,'.rda', sep=""))
  predictions.model.ssgm.Val_Cluozza <- as.numeric(glm)
  predictions.model.ssgm <- c(predictions.model.ssgm, predictions.model.ssgm.Val_Cluozza)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Cluozza/Val_Cluozza.model.1.only.ssgm.lslpts.obs_',j,'.rda', sep=""))
  obs.ssgm.Val_Cluozza <- as.numeric(obs.glm)
  observations.ssgm <- c(observations.ssgm, obs.ssgm.Val_Cluozza)
}


labels.ssgm.Val_Cluozza <- as.data.frame(observations.ssgm)
predictions.model.ssgm.Val_Cluozza <- as.data.frame(predictions.model.ssgm)

pred.model.ssgm.Val_Cluozza <- prediction(predictions.model.ssgm.Val_Cluozza, labels.ssgm.Val_Cluozza)
perf.pred.model.ssgm.Val_Cluozza <- performance(pred.model.ssgm.Val_Cluozza, measure = "tpr", x.measure = "fpr") 
# 
# # - AUC / ROC curve
AUC.Val_Cluozza.ssgm <- performance(pred.model.ssgm.Val_Cluozza, measure = "auc")
auc.ssgm.Val_Cluozza <- round(as.numeric(AUC.Val_Cluozza.ssgm@y.values),3)
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Val_Cluozza_onlySsgm.pdf")
# par(mfrow=c(1,1))
# plot(perf.pred.model.ssgm.Val_Cluozza, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.ssgm)), bty='n', cex=1.6)
# dev.off()  




# VAL D'ENTREMONT

# SLOPE
predictions.model.slope <- vector()
observations.slope <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_D_Entremont/Val_D_Entremont.model.1.only.slope.glm_',j,'.rda', sep=""))
  predictions.model.slope.Val_D_Entremont <- as.numeric(glm)
  predictions.model.slope <- c(predictions.model.slope, predictions.model.slope.Val_D_Entremont)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_D_Entremont/Val_D_Entremont.model.1.only.slope.lslpts.obs_',j,'.rda', sep=""))
  obs.slope.Val_D_Entremont <- as.numeric(obs.glm)
  observations.slope <- c(observations.slope, obs.slope.Val_D_Entremont)
}


labels.slope.Val_D_Entremont <- as.data.frame(observations.slope)
predictions.model.slope.Val_D_Entremont <- as.data.frame(predictions.model.slope)

pred.model.slope.Val_D_Entremont <- prediction(predictions.model.slope.Val_D_Entremont, labels.slope.Val_D_Entremont)
perf.pred.model.slope.Val_D_Entremont <- performance(pred.model.slope.Val_D_Entremont, measure = "tpr", x.measure = "fpr") 


# # - AUC / ROC curve
AUC.Val_D_Entremont.slope <- performance(pred.model.slope.Val_D_Entremont, measure = "auc")
auc.slope.Val_D_Entremont <- round(as.numeric(AUC.Val_D_Entremont.slope@y.values),3)
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Val_D_Entremont_onlySlope.pdf")
# plot(perf.pred.model.slope.Val_D_Entremont, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.slope)), bty='n', cex=1.6)
# dev.off()  

# SSGM
predictions.model.ssgm <- vector()
observations.ssgm <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_D_Entremont/Val_D_Entremont.model.1.only.ssgm.glm_',j,'.rda', sep=""))
  predictions.model.ssgm.Val_D_Entremont <- as.numeric(glm)
  predictions.model.ssgm <- c(predictions.model.ssgm, predictions.model.ssgm.Val_D_Entremont)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_D_Entremont/Val_D_Entremont.model.1.only.ssgm.lslpts.obs_',j,'.rda', sep=""))
  obs.ssgm.Val_D_Entremont <- as.numeric(obs.glm)
  observations.ssgm <- c(observations.ssgm, obs.ssgm.Val_D_Entremont)
}


labels.ssgm.Val_D_Entremont <- as.data.frame(observations.ssgm)
predictions.model.ssgm.Val_D_Entremont <- as.data.frame(predictions.model.ssgm)

pred.model.ssgm.Val_D_Entremont <- prediction(predictions.model.ssgm.Val_D_Entremont, labels.ssgm.Val_D_Entremont)
perf.pred.model.ssgm.Val_D_Entremont <- performance(pred.model.ssgm.Val_D_Entremont, measure = "tpr", x.measure = "fpr") 


# # - AUC / ROC curve
AUC.Val_D_Entremont.ssgm <- performance(pred.model.ssgm.Val_D_Entremont, measure = "auc")
auc.ssgm.Val_D_Entremont <- round(as.numeric(AUC.Val_D_Entremont.ssgm@y.values),3)
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Val_D_Entremont_onlySsgm.pdf")
# par(mfrow=c(1,1))
# plot(perf.pred.model.ssgm.Val_D_Entremont, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.ssgm)), bty='n', cex=1.6)
# dev.off()  




# VAL PIORA

# SLOPE
predictions.model.slope <- vector()
observations.slope <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora/Val_Piora.model.1.only.slope.glm_',j,'.rda', sep=""))
  predictions.model.slope.Val_Piora <- as.numeric(glm)
  predictions.model.slope <- c(predictions.model.slope, predictions.model.slope.Val_Piora)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora/Val_Piora.model.1.only.slope.lslpts.obs_',j,'.rda', sep=""))
  obs.slope.Val_Piora <- as.numeric(obs.glm)
  observations.slope <- c(observations.slope, obs.slope.Val_Piora)
}


labels.slope.Val_Piora <- as.data.frame(observations.slope)
predictions.model.slope.Val_Piora <- as.data.frame(predictions.model.slope)

pred.model.slope.Val_Piora <- prediction(predictions.model.slope.Val_Piora, labels.slope.Val_Piora)
perf.pred.model.slope.Val_Piora <- performance(pred.model.slope.Val_Piora, measure = "tpr", x.measure = "fpr") 

# # - AUC / ROC curve
AUC.Val_Piora.slope <- performance(pred.model.slope.Val_Piora, measure = "auc")
auc.slope.Val_Piora <- round(as.numeric(AUC.Val_Piora.slope@y.values),3)
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Val_Piora_onlySlope.pdf")
# plot(perf.pred.model.slope.Val_Piora, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.slope)), bty='n', cex=1.6)
# dev.off()  

# SSGM
predictions.model.ssgm <- vector()
observations.ssgm <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora/Val_Piora.model.1.only.ssgm.glm_',j,'.rda', sep=""))
  predictions.model.ssgm.Val_Piora <- as.numeric(glm)
  predictions.model.ssgm <- c(predictions.model.ssgm, predictions.model.ssgm.Val_Piora)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora/Val_Piora.model.1.only.ssgm.lslpts.obs_',j,'.rda', sep=""))
  obs.ssgm.Val_Piora <- as.numeric(obs.glm)
  observations.ssgm <- c(observations.ssgm, obs.ssgm.Val_Piora)
}


labels.ssgm.Val_Piora <- as.data.frame(observations.ssgm)
predictions.model.ssgm.Val_Piora <- as.data.frame(predictions.model.ssgm)

pred.model.ssgm.Val_Piora <- prediction(predictions.model.ssgm.Val_Piora, labels.ssgm.Val_Piora)
perf.pred.model.ssgm.Val_Piora <- performance(pred.model.ssgm.Val_Piora, measure = "tpr", x.measure = "fpr") 
# 
# # - AUC / ROC curve
AUC.Val_Piora.ssgm <- performance(pred.model.ssgm.Val_Piora, measure = "auc")
auc.ssgm.Val_Piora <- round(as.numeric(AUC.Val_Piora.ssgm@y.values),3)
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_Val_Piora_onlySsgm.pdf")
# par(mfrow=c(1,1))
# plot(perf.pred.model.ssgm.Val_Piora, 
#      colorize=TRUE, 
#      colorize.palette=colors, 
#      colorkey=FALSE, 
#      print.cutoffs.at=seq(0,1,by=0.1), 
#      text.adj=c(-0.5, 1), 
#      yaxis.cex.axis=1.5, xaxis.cex.axis=1.5, cex.lab=1.5 , text.cex=1.2, lwd=4)
# abline(a=0, b= 1, lty=3, lwd=2)
# legend('bottom', c(paste("AUC =", auc.ssgm)), bty='n', cex=1.6)
# dev.off()  

#colors=c("#6B0077", "#6C3888", "#6E5698", "#7271A7", "#7988B4", "#839EBF", "#92B1C8", "#A5C1CF", "#BACED4", "#D4D4D4")
#colors=c("#1B0063", "#003A5F", "#005B62", "#007756", "#2D8D30", "#8D9B00", "#D69F00", "#FF9E85", "#FFA9D5", "#F3C7F0")
#colors=c("#CD778E", "#C28165", "#AA8E40", "#85993F", "#2D8D30", "#00A58A", "#00A2AE", "#5198C6", "#9388CC", "#BC79BE")
colors=c("#003B01", "#005251", "#005D7B", "#3F5DA1", "#9353B6", "#CB4FB0", "#EB6492", "#F98C5E", "#F8BD00", "#F0E400")

# SLOPE Plot



labels=c(paste("Arosa, AUC =",auc.slope.Arosa), paste("Baulmes, AUC =",auc.slope.Baulmes), paste("Chrauchtal, AUC =",auc.slope.Chrauchtal), paste("Hornbach, AUC =",auc.slope.Hornbach),
         paste("Rappetal, AUC =",auc.slope.Rappetal), paste("Turbach, AUC =",auc.slope.Turbach), paste("Urseren, AUC =",auc.slope.Urseren),paste("Val Cluozza, AUC =",auc.slope.Val_Cluozza),
         paste("Val d'Entremont, AUC =",auc.slope.Val_D_Entremont), paste("Val Piora, AUC =",auc.slope.Val_Piora))
pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_All_in_One_onlySlope.pdf")
par(cex.axis=1.3)
plot(perf.pred.model.slope.Arosa, col=colors[1], lwd=2.8, cex.lab=1.3)
plot(perf.pred.model.slope.Baulmes, add = TRUE, col=colors[2], lwd=2.8)
plot(perf.pred.model.slope.Chrauchtal, add = TRUE, col=colors[3], lwd=2.8)
plot(perf.pred.model.slope.Hornbach, add = TRUE, col=colors[4], lwd=2.8)
plot(perf.pred.model.slope.Rappetal, add = TRUE, col=colors[5], lwd=2.8)
plot(perf.pred.model.slope.Turbach, add = TRUE, col=colors[6], lwd=2.8)
plot(perf.pred.model.slope.Urseren, add = TRUE, col=colors[7], lwd=2.8)
plot(perf.pred.model.slope.Val_Cluozza, add = TRUE, col=colors[8], lwd=2.8)
plot(perf.pred.model.slope.Val_D_Entremont, add = TRUE, col=colors[9], lwd=2.8)
plot(perf.pred.model.slope.Val_Piora, add = TRUE, col=colors[10], lwd=2.8)
legend('bottomright', labels, col=colors, bty="n", lty = 1, lwd=3, cex=1.1)
text(x= 0.242, y= 0.8, labels = "A", 
     cex = 0.9, col = "#003B01", font=2)
text(x= 0.8, y= 0.902, labels = "B", 
     cex = 0.9, col = "#005251", font=2)
text(x= 0.15, y= 0.4, labels = "C", 
     cex = 0.9, col = "#005D7B", font=2)
text(x= 0.164, y= 0.55, labels = "H", 
     cex = 0.9, col = "#3F5DA1", font=2)
text(x= 0.338, y= 0.5, labels = "R", 
     cex = 0.9, col = "#9353B6", font=2)
text(x= 0.419, y= 0.85 , labels = "T", 
     cex = 0.9, col = "#CB4FB0", font=2)
text(x= 0.285, y= 0.75 , labels = "U", 
     cex = 0.9, col = "#EB6492", font=2)
text(x= 0.55, y=0.78 , labels = "VC", 
     cex = 0.9, col = "#F98C5E", font=2)
text(x= 0.55, y= 0.91 , labels = "VE", 
     cex = 0.9, col = "#F8BD00", font=2)
text(x=0.55 , y=1 , labels = "VP", 
     cex = 0.9, col = "#DFD553", font=2)
abline(a=0, b= 1, lty=3, lwd=1)

dev.off()  





# SSGM Plot

labels=c(paste("Arosa, AUC =",auc.ssgm.Arosa), paste("Baulmes, AUC =",auc.ssgm.Baulmes), paste("Chrauchtal, AUC =",auc.ssgm.Chrauchtal), paste("Hornbach, AUC =",auc.ssgm.Hornbach),
         paste("Rappetal, AUC =",auc.ssgm.Rappetal), paste("Turbach, AUC =",auc.ssgm.Turbach), paste("Urseren, AUC =",auc.ssgm.Urseren),paste("Val Cluozza, AUC =",auc.ssgm.Val_Cluozza),
         paste("Val d'Entremont, AUC =",auc.ssgm.Val_D_Entremont), paste("Val Piora, AUC =",auc.ssgm.Val_Piora))


pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_All_in_One_onlySSGM.pdf")
par(cex.axis=1.3)
plot(perf.pred.model.ssgm.Arosa, col=colors[1], lwd=2.8, cex.lab=1.3)
plot(perf.pred.model.ssgm.Baulmes, add = TRUE, col=colors[2], lwd=2.8)
plot(perf.pred.model.ssgm.Chrauchtal, add = TRUE, col=colors[3], lwd=2.8)
plot(perf.pred.model.ssgm.Hornbach, add = TRUE, col=colors[4], lwd=2.8)
plot(perf.pred.model.ssgm.Rappetal, add = TRUE, col=colors[5], lwd=2.8)
plot(perf.pred.model.ssgm.Turbach, add = TRUE, col=colors[6], lwd=2.8)
plot(perf.pred.model.ssgm.Urseren, add = TRUE, col=colors[7], lwd=2.8)
plot(perf.pred.model.ssgm.Val_Cluozza, add = TRUE, col=colors[8], lwd=2.8)
plot(perf.pred.model.ssgm.Val_D_Entremont, add = TRUE, col=colors[9], lwd=2.8)
plot(perf.pred.model.ssgm.Val_Piora, add = TRUE, col=colors[10], lwd=2.8)
legend('bottomright', labels, col=colors, bty="n", lty = 1, lwd=3, cex=1.1)

text(x= 0.38, y= 0.725, labels = "A", 
     cex = 0.9, col = "#003B01", font=2)
text(x= 0.49, y= 0.715, labels = "B", 
     cex = 0.9, col = "#005251", font=2)
text(x= 0.44, y= 0.915, labels = "C", 
     cex = 0.9, col = "#005D7B", font=2)
text(x= 0.5, y= 0.816 , labels = "H", 
     cex = 0.9, col = "#3F5DA1", font=2)
text(x= 0.24, y= 0.55, labels = "R", 
     cex = 0.9, col = "#9353B6", font=2)
text(x= 0.56, y= 0.884 , labels = "T", 
     cex = 0.9, col = "#CB4FB0", font=2)
text(x= 0.25, y= 0.8 , labels = "U", 
     cex = 0.9, col = "#EB6492", font=2)
text(x= 0.45, y=0.55 , labels = "VC", 
     cex = 0.9, col = "#F98C5E", font=2)
text(x= 0.43, y= 0.64 , labels = "VE", 
     cex = 0.9, col = "#F8BD00", font=2)
text(x=0.115 , y=0.15  , labels = "VP", 
     cex = 0.9, col = "#DFD553", font=2)

abline(a=0, b= 1, lty=3, lwd=1)

dev.off()  


