# - load packages

library('ggplot2')
# - load packages
library('lmtest')
library('reshape')
library('gplots')
library('ROCR')


# -------- All Sites inkl GESTEINSKLASSE
k.boot=20

grpreg.model <- vector()
obs <- vector()

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/All/All.model.1.all.sites.cv.grpreg_grLasso_Gestkl_',j,'.rda', sep=""))
  grpreg.model.All.Gestkl <- as.numeric(cv.grpreg)
  grpreg.model <- c(grpreg.model, grpreg.model.All.Gestkl)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/All/All.model.1.all.sites.lslpts.observations_Gestkl_',j,'.rda', sep=""))
  obs.All.Gestkl <- as.numeric(obs.grpreg)
  obs <- c(obs, obs.All.Gestkl)
}

grpreg.model.All.Gestkl <- grpreg.model
obs.All.Gestkl <- obs




sink("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/Brier_Scores_All_Sites_grlasso_Gesteinsklasse_no_bootstrap.txt")

cat(sprintf("Brier Score for All Gesteinsklasse: %10.5f\n", mean( ( obs.All.Gestkl - grpreg.model.All.Gestkl )^2)))

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

#boot.correct  <- matrix( NA, nrow=kboot, ncol=2, 
#                         dimnames=list(NULL,c("grpreg.All.nogeol", "grpreg.All" ) ))
boot.correct  <- matrix( NA, nrow=kboot, ncol=1, 
                         dimnames=list(NULL,c("grpreg.All.Gestkl") ))


# - Looping kboot times, pick THE SAME random elements for BOTH
#   (or all) vectors and store the mean values into boot.correct.
#   "idx" will be the random sample.

for ( i in 1:kboot ) {
  
  n <- length(obs.All.Gestkl)
  idx <- sample(1:n,n,replace=TRUE)
  boot.correct[i,"grpreg.All.Gestkl"] <- mean( ( obs.All.Gestkl[idx] - grpreg.model.All.Gestkl[idx] )^2)
  
  #  n <- length(obs.All)
  #  idx <- sample(1:n,n,replace=TRUE)
  #  boot.correct[i,"grpreg.All"] <- mean( ( obs.All[idx] - grpreg.model.All[idx] )^2)
  
}




# PLOT BRIER SCORE 

lab <- "Brier Score"

pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/bootstrapped_BS_All_Sites_Gesteinsklasse.pdf", width=3, height =5 )

op <- par(mar=c(4,3,1,1))

boxplot( boot.correct,
         pars=list(outcol="black"), 
         t="l", xaxt="n", xlab="", #ylab="Brier Score", 
         cex.lab= 1.2, cex.axis=1.2)
axis(1,1, tick=FALSE, labels=lab, cex.axis=1.2)
#text(x=1, labels=lab, cex=1.3)
par(op) 
dev.off()




# obs.All.nogeol
# grpreg.model.All.nogeol
# obs.All
# grpreg.model.All



# ************************* performance scores ************************* 
# color for ROC curve 
colors <- c("#EBE06E","#BDDA66","#87D26F","#3EC77F","#00BA8F","#00A99C",
            "#0095A4","#007CA4","#005D9D","#22288E")

# ******** All Sites Model
# Gesteinsklasse
# 
predictions.model.Gestkl <- vector()
observations.Gestkl <- vector()
k.boot <- 20

for ( j in 1:k.boot ) {
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/All/All.model.1.all.sites.cv.grpreg_grLasso_Gestkl_',j,'.rda', sep=""))
  predictions.model.All.Gestkll.grpreg <- as.numeric(cv.grpreg)
  predictions.model.Gestkl <- c(predictions.model.Gestkl, predictions.model.All.Gestkll.grpreg)
  load (paste('/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/All/All.model.1.all.sites.lslpts.observations_Gestkl_',j,'.rda', sep=""))
  obs.All.Gestkl <- as.numeric(obs.grpreg)
  observations.Gestkl <- c(observations.Gestkl, obs.All.Gestkl)
}

labels.All.Gestkl <- as.data.frame(observations.Gestkl)
predictions.model.All.Gestkl <- as.data.frame(predictions.model.Gestkl)

pred.model.All.Gestkl <- prediction(predictions.model.Gestkl, labels.All.Gestkl)
perf.model.All.Gestkl <- performance(pred.model.All.Gestkl, measure = "tpr", x.measure = "fpr") 


# - Confusion Matrix
CM.model.All.Gestkl <- table(predictions.model.All.Gestkl  >= 0.5 , observations.Gestkl)
All.Gestkl <- as.vector(CM.model.All.Gestkl)


# - Calculate Rates/Ratios
Bias.All.Gestkl <- (All.Gestkl[2]+All.Gestkl[4])/(All.Gestkl[3]+All.Gestkl[4])
HR.All.Gestkl <- All.Gestkl[4]/(All.Gestkl[4]+All.Gestkl[3])
FARate.All.Gestkl <- All.Gestkl[2]/(All.Gestkl[2]+All.Gestkl[1])
FARatio.All.Gestkl <- All.Gestkl[2]/(All.Gestkl[2]+All.Gestkl[4])
PC.All.Gestkl <- (All.Gestkl[1]+All.Gestkl[4])/(All.Gestkl[1]+All.Gestkl[2]+All.Gestkl[3]+All.Gestkl[4])

# - AUC / ROC curve
AUC.All.Gestkl <- performance(pred.model.All.Gestkl, measure = "auc")
auc.Gestkl <- round(as.numeric(AUC.All.Gestkl@y.values),3)   
pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/AUC_ROC/ROC_AUC_All_Sites_Gesteinsklasse.pdf")
plot(perf.model.All.Gestkl, 
     # colorize=TRUE, 
     # colorize.palette=colors, 
     # colorkey=FALSE, 
     # print.cutoffs.at=seq(0,1,by=0.1), 
     # text.adj=c(-0.5, 1), 
     yaxis.cex.axis=1.5, 
     xaxis.cex.axis=1.5, 
     cex.lab=1.5 , text.cex=1.2, lwd=4)
abline(a=0, b= 1, lty=3, lwd=2)
legend('bottom', c(paste("AUC =", auc.Gestkl)), bty='n', cex=1.6)
dev.off()  


median(boot.correct)


# - Print all scores to txt file of all 10 sites

sink("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Model_Scores/Contingency_Table_results_all_sites_Gesteinsklasse.txt")

cat("=========  Contingency Matrix with 0.5 Probability threshold ================\n")
cat("=========  Bias = (total forecast yes) / (total observed yes)  B >= 1, overforecast; B <= 1, underforecast \n")
cat("=========  Hit Rate =============\n")
cat("=========  FARate (False Alarm Ratio) ============\n")
cat("=========  FARatio  (False Alarm Rate) ============\n")
cat("=========  PC / Accuracy ================\n")

cat("=========  All-Sites Model Gesteinsklasse ================\n")
print(CM.model.All.Gestkl)
print(Bias.All.Gestkl)
print(HR.All.Gestkl)
print(FARate.All.Gestkl)
print(FARatio.All.Gestkl)
print(PC.All.Gestkl)
sink()

