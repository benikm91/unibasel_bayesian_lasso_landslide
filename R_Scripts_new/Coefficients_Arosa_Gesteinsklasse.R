
library("data.table")

# *************** count coeffcients used in models

# AROSA, Gesteinsklasse

Site = "Arosa"

# - load one file to get names of coefficients for site and create file to fill with loop
table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Arosa/Arosa.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_1_1.txt", sep="") , 
                               stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)

table.coef$beta <- as.numeric(table.coef$beta)
table.coef[table.coef == 0] <- NA

summary <- table.coef[1:2]



n.folds=5
k.boot=20


for ( i in 1:n.folds ) {
  for ( j in 1:k.boot ) {
    
    table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Arosa/Arosa.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_",i,"_",j,".txt", sep="") , 
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


beta_fit <- colMeans(df, na.rm = TRUE)
save(beta_fit, file=paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/",Site,"/",Site,".model.1.Gestkl.beta_averaged.rda", sep=""))


df <- df[c(-1)]
str(df)

labels <- names(df)
labels
labels <- c("Elevation", "Slope", "TWI", "Flow acc.", "Curvature plan", "Curvature profile", "Roughness", "Dist. to Roads", 
            "Dist. to Streams", "Roads density", "Stream density", "Max. Precip. (10Y)", 
            "Max. Precip. (5Y)", "Snow days", "Snow cover days", "Growing season length", "Frost change freq.", 
            # "Amphibolite","Basalt","Gneiss/Mica Schist","Coarse Gravel/Sand (Colluvium)","Unconsolidated Rocks","Rhyolithe/Dacite"  ,     
            # "Crystalline/Ophiolithe","Sedimentary Rocks","Serpentinite/Talc Schist","Moraine Material",
            "Unconsolidated Rocks", "Igneous Rocks", "Metamorphic Rocks", "Sedimentary Rocks",
            "Aspect_E", "Aspect_N", "Aspect_NE", "Aspect_NW",
            "Aspect_S", "Aspect_SE", "Aspect_SW", "Aspect_W")
labels


# "Igneous Rocks", "Sedimentary Rocks", "Metamorphic Rocks", "Unconsolidated Rocks"

# "Amphibolite","Basalt","Gneiss/Mica Schist", "Coarse Gravel/Sand (Colluvium)", "Unconsolidated Rocks", "Rhyolithe/Dacite",
# "Crystalline/Ophiolithe","Sedimentary Rocks","Serpentinite/Talc Schist","Moraine Material","Alluivium", "Gravel/Sand/Silt",
# "Boulders", "Gravel/Sand", "Metagranitoid", "Granite"

# *************** calculate variable importance

counter <- length(df)
importance <- vector()

for (k in 1:counter) {
  imp <- 100-sum(is.na(df[k]))
  importance <- c(importance,imp)
}

#importance <- as.numeric(importance)

#labels <- names(df)

# - Plot Feature Importance 

pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Coefficients/Arosa_Coefficient_Importance_Gesteinsklasse.pdf",  width = 6, # The width of the plot in inches
    height = 12)

par(mar=c(3,10,1,1))

colors <- c("#6B0077", "#6B0077","#6B0077","#6B0077","#6B0077","#6B0077","#6B0077","#6B0077","#6B0077",
            "#6B0077","#6B0077","#6B0077","#6B0077","#6B0077","#6B0077","#6B0077","#6B0077",
            "#73579B","#73579B","#73579B","#73579B",
            "#828BBC","#828BBC","#828BBC","#828BBC","#828BBC","#828BBC","#828BBC","#828BBC")

barplot(rev(importance), horiz=TRUE, names.arg=rev(labels), las=1, col=rev(colors), cex.names=1)

dev.off()







# ---- Plot Coefficients 

# if all 100 are NA change to uero for plot
loop<-length(df)

for (i in 1:loop){
  if(sum(is.na(df[i]))==100){
    df[i]=0
  }
}




pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Coefficients/Arosa_Coefficients_Gesteinsklasse.pdf",  width = 14, # The width of the plot in inches
    height = 6)
par(mar=c(8,4.4,0.2,0.2))
boxplot(df[1:counter], t="l",xaxt="n", xlab="", ylab="Coefficient Value", cex.lab=1.4, cex.axis=1.4, ylim = c(-2,2))
#boxplot(df[2:21], t="l",xaxt="n", xlab="", ylab="", cex.axis=0.8)
axis(1, at=c(1:counter), labels=FALSE)

#abline(v=c(1.5,3.5,7.5,11.5),col="#6B007745", lty=1 )
abline( h = 0, col = "darkgrey", lty =1)
abline( v=c(17.5,21.5),col="grey", lty=1 )

text(x=1:counter, y=-2 , paste(importance, sep="") ,cex=1.2) 

text(x=1:counter, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]),
     labels=labels, srt=45, adj=1, xpd=TRUE, cex=1.2)

text(x=c(18.4,22.35), y=c(2,1.985), labels=c("Rocks", "Aspect"), cex=1.2)

dev.off()
