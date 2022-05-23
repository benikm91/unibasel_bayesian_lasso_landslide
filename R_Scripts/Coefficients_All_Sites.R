

library("data.table")

# *************** count coeffcients used in models

# ALL SITES

#Site = "All"

# - load one file to get names of coefficients for site and create file to fill with loop
table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/All/All.model.1.all.sites.coeffiecients_cv_grpreg_grLasso_noGeol_1_1.txt", sep="") , 
                               stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)


table.coef$beta <- as.numeric(table.coef$beta)
table.coef[table.coef == 0] <- NA

summary <- table.coef[1:2]
summary

n.folds=5
k.boot=20


for ( i in 1:n.folds ) {
  for ( j in 1:k.boot ) {
    
    table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/All/All.model.1.all.sites.coeffiecients_cv_grpreg_grLasso_noGeol_",i,"_",j,".txt", sep="") , 
                                   stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)
    
    table.coef$beta <- as.numeric(table.coef$beta)
    table.coef[table.coef == 0] <- NA
    #assign(paste("beta_",i,"_",j,sep=""), table.coef$beta)
    summary <- cbind(summary, table.coef$beta)
  }
}

summary_1 <- summary[-1]
df<- transpose(summary_1[-1])
colnames(df) <- summary_1[, 1]
str(df)

df <- df[c(-1)]
str(df)


labels <- names(df)
labels
labels <- c("Elevation", "Slope", "TWI", "Flow acc.", "Flow dir.", "Curvature plan", "Curvature profile", "Roughness", "Dist. to Roads", 
            "Dist. to Streams", "Roads density", "Stream density", "SSGM Value", "Veg. Roughness", "Winter Precip.", "Max. Precip. (10Y)", 
            "Max. Precip. (5Y)", "Snow days", "Snow cover days", "Growing season length", "Frost change freq.", 
            # "Geology_ad", "Geology_ag", "Geology_al", "Geology_ao", "Geology_dj", "Geology_dl", "Geology_dm", 
            # "Geology_eq", "Geology_er", "Geology_fk", "Geology_fo", 
            # "Geology_fq", "Geology_hh", "Geology_hn", "Geology_hq", "Geology_id", "Geology_aa", "Geology_bh", 
            # "Geology_bi", "Geology_cb", "Geology_cc", "Geology_ac", 
            # "Geology_ae", "Geology_jb", "Geology_jc", "Geology_je", "Geology_jm", "Geology_jn", "Geology_jo", 
            # "Geology_ak", "Geology_be", 
            # "Geology_en", "Geology_if", "Geology_hk", "Geology_hl", "Geology_hv", "Geology_ib", "Geology_ic", 
            # "Geology_ec", 
            # "Geology_ji", "Geology_jl", "Geology_fl", "Geology_fm", "Geology_hs", "Geology_hu", "Geology_ek",
            "Aspect_E", "Aspect_N", "Aspect_NE", "Aspect_NW",
            "Aspect_S", "Aspect_SE", "Aspect_SW", "Aspect_W")
labels


# *************** calculate variable importance

counter <- length(df)
importance <- vector()

for (k in 1:counter) {
  imp <- 100-sum(is.na(df[k]))
  importance <- c(importance,imp)
}

#importance <- as.numeric(importance)


# - Plot Feature Importance 

pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Coefficients/All_Sites_Coefficient_Importance_noGeologyModel.pdf",  width = 6, # The width of the plot in inches
    height = 12)

par(mar=c(3,10,1,1))

colors <- c("#6B0077", "#6B0077","#6B0077","#6B0077","#6B0077","#6B0077","#6B0077","#6B0077","#6B0077","#6B0077",
            "#6B0077","#6B0077","#6B0077","#6B0077","#6B0077","#6B0077","#6B0077","#6B0077","#6B0077","#6B0077",
            "#6B0077",
            # "#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B",
            # "#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B",
            # "#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B",
            # "#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B","#73579B",
            # "#73579B","#73579B","#73579B","#73579B","#73579B","#73579B",
            "#9CB8D6","#9CB8D6","#9CB8D6","#9CB8D6","#9CB8D6","#9CB8D6","#9CB8D6","#9CB8D6")

barplot(rev(importance), horiz=TRUE, names.arg=rev(labels), las=1, col=rev(colors), cex.names=0.75)

dev.off()





# ---- Plot Coefficients 

# if all 100 are NA change to uero for plot
loop<-length(df)

for (i in 1:loop){
  if(sum(is.na(df[i]))==100){
    df[i]=0
  }
}



pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Coefficients/All_Sites_Coefficients_noGeologyModel.pdf",  width = 14, # The width of the plot in inches
    height = 6.5)
par(mar=c(7.9,2,0.2,0.2))
boxplot(df[c(1:counter)], t="l",xaxt="n", xlab="", ylab="", cex.axis=1.2, ylim = c(-2,2))
#boxplot(df[2:21], t="l",xaxt="n", xlab="", ylab="", cex.axis=0.8)
axis(1, at=c(1:counter), labels=FALSE)

#abline(v=c(1.5,3.5,7.5,11.5),col="#6B007745", lty=1 )
abline( h = 0, col = "darkgrey", lty =1)
abline( v=c(21.5),col="grey", lty=1 )

text(x=1:counter, y=-2 , paste(importance[c(1:counter)], sep="") ,cex=1.2) 

text(x=1:counter, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]),
     labels=labels[c(1:counter)], srt=45, adj=1, xpd=TRUE, cex=1.2)

text(x=c(22.4), y=2, labels=c("Aspect"), cex=1.2)
dev.off()

# 
# 
# pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Coefficients/All_Sites_Coefficients_Geol.pdf",  width = 14, # The width of the plot in inches
#     height = 6)
# par(mar=c(6,2,0.2,0.2))
# boxplot(df[c(22:(counter-8))], t="l",xaxt="n", xlab="", ylab="", cex.axis=0.8, ylim = c(-2,2))
# #boxplot(df[2:21], t="l",xaxt="n", xlab="", ylab="", cex.axis=0.8)
# axis(1, at=c(1:46), labels=FALSE)
# 
# #abline(v=c(1.5,3.5,7.5,11.5),col="#6B007745", lty=1 )
# abline( h = 0, col = "darkgrey", lty =1)
# #abline( v=c(21.5),col="grey", lty=1 )
# 
# text(x=1:46, y=-2 , paste(importance[c(22:(counter-8))], sep="") ,cex=0.75) 
# 
# text(x=1:46, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]),
#      labels=labels[c(22:(counter-8))], srt=45, adj=1, xpd=TRUE, cex=0.75)
# 
# text(x=c(1.2), y=2, labels=c("Geology"), cex=0.75)
# dev.off()




