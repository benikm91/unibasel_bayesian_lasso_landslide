# Heatmap for Coefficient selections of models for all sites in one Plot

library("RColorBrewer")
library("data.table")
library("pheatmap")

# HEATMAP with GESTEINSKLASSEN


# ........ AROSA ........
# *************** count coeffcients used in models
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
df_arosa<- transpose(summary_1[-1])
colnames(df_arosa) <- summary_1[, 1]
str(df_arosa)
df_arosa <- df_arosa[c(-1)]
str(df_arosa)

# remove geology to make comparable to other sites?

#df_arosa <- df_arosa[c(-22:-37)]

# *************** calculate variable importance

counter <- length(df_arosa)
importance <- vector()


for (k in 1:counter) {
  imp <- 100-sum(is.na(df_arosa[k]))
  importance <- c(importance,imp)
}

#importance <- as.numeric(importance)

#median.matrix <- matrix(data=NA, nrow=counter, ncol=2)
median.matrix.arosa <- matrix(data=1, nrow=counter, ncol=1)

for (k in 1:counter) {
  median.matrix.arosa[k] <- median(na.omit(df_arosa[[k]]))
}



# ........ BAULMES ........

# - load one file to get names of coefficients for site and create file to fill with loop
table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Baulmes/Baulmes.model.2.Gestkl.coeffiecients_cv_grpreg_grLasso_1_1.txt", sep="") , 
                               stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)

table.coef$beta <- as.numeric(table.coef$beta)
table.coef[table.coef == 0] <- NA

summary <- table.coef[1:2]


n.folds=5
k.boot=20


for ( i in 1:n.folds ) {
  for ( j in 1:k.boot ) {
    
    table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Baulmes/Baulmes.model.2.Gestkl.coeffiecients_cv_grpreg_grLasso_",i,"_",j,".txt", sep="") , 
                                   stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)
    
    table.coef$beta <- as.numeric(table.coef$beta)
    table.coef[table.coef == 0] <- NA
    #assign(paste("beta_",i,"_",j,sep=""), table.coef$beta)
    summary <- cbind(summary, table.coef$beta)
  }
}

summary_1 <- summary[-1]
df_baulmes<- transpose(summary_1[-1])
colnames(df_baulmes) <- summary_1[, 1]
str(df_baulmes)
df_baulmes <- df_baulmes[c(-1)]
str(df_baulmes)


# remove geology to make comparable to other sites?

#df_baulmes <- df_baulmes[c(-22:-26)]


# *************** calculate variable importance

counter <- length(df_baulmes)
importance <- vector()


for (k in 1:counter) {
  imp <- 100-sum(is.na(df_baulmes[k]))
  importance <- c(importance,imp)
}

#importance <- as.numeric(importance)

#median.matrix <- matrix(data=NA, nrow=counter, ncol=2)
median.matrix.baulmes <- matrix(data=1, nrow=counter, ncol=1)

for (k in 1:counter) {
  median.matrix.baulmes[k] <- median(na.omit(df_baulmes[[k]]))
}
median.matrix.baulmes

# missing lines
vec <- c(19,20,24)
new_mat_baulmes <- matrix(data=NA, nrow=29, ncol=1)
new_mat_baulmes[-vec,] <- median.matrix.baulmes   

# ........ CHRAUCHTAL ........
# *************** count coeffcients used in models
# - load one file to get names of coefficients for site and create file to fill with loop
table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Chrauchtal/Chrauchtal.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_1_1.txt", sep="") , 
                               stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)

table.coef$beta <- as.numeric(table.coef$beta)
table.coef[table.coef == 0] <- NA

summary <- table.coef[1:2]



n.folds=5
k.boot=20


for ( i in 1:n.folds ) {
  for ( j in 1:k.boot ) {
    
    table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Chrauchtal/Chrauchtal.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_",i,"_",j,".txt", sep="") , 
                                   stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)
    
    table.coef$beta <- as.numeric(table.coef$beta)
    table.coef[table.coef == 0] <- NA
    #assign(paste("beta_",i,"_",j,sep=""), table.coef$beta)
    summary <- cbind(summary, table.coef$beta)
  }
}


summary_1 <- summary[-1]
df_chrauchtal<- transpose(summary_1[-1])
colnames(df_chrauchtal) <- summary_1[, 1]
str(df_chrauchtal)
df_chrauchtal <- df_chrauchtal[c(-1)]
str(df_chrauchtal)

# remove geology to make comparable to other sites?

#df_chrauchtal <- df_chrauchtal[c(-22:-31)]

# *************** calculate variable importance

counter <- length(df_chrauchtal)
importance <- vector()


for (k in 1:counter) {
  imp <- 100-sum(is.na(df_chrauchtal[k]))
  importance <- c(importance,imp)
}

#importance <- as.numeric(importance)

#median.matrix <- matrix(data=NA, nrow=counter, ncol=2)
median.matrix.chrauchtal <- matrix(data=1, nrow=counter, ncol=1)

for (k in 1:counter) {
  median.matrix.chrauchtal[k] <- median(na.omit(df_chrauchtal[[k]]))
}

# missing lines
vec <- c(19,20)
new_mat_chrauchtal <- matrix(data=NA, nrow=29, ncol=1)
new_mat_chrauchtal[-vec,] <- median.matrix.chrauchtal   


# ........ HORNBACH ........
# *************** count coeffcients used in models
# - load one file to get names of coefficients for site and create file to fill with loop
table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Hornbach/Hornbach.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_1_1.txt", sep="") , 
                               stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)

table.coef$beta <- as.numeric(table.coef$beta)
table.coef[table.coef == 0] <- NA

summary <- table.coef[1:2]



n.folds=5
k.boot=20


for ( i in 1:n.folds ) {
  for ( j in 1:k.boot ) {
    
    table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Hornbach/Hornbach.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_",i,"_",j,".txt", sep="") , 
                                   stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)
    
    table.coef$beta <- as.numeric(table.coef$beta)
    table.coef[table.coef == 0] <- NA
    #assign(paste("beta_",i,"_",j,sep=""), table.coef$beta)
    summary <- cbind(summary, table.coef$beta)
  }
}


summary_1 <- summary[-1]
df_hornbach<- transpose(summary_1[-1])
colnames(df_hornbach) <- summary_1[, 1]
str(df_hornbach)
df_hornbach <- df_hornbach[c(-1)]
str(df_hornbach)

# remove geology to make comparable to other sites?

#df_hornbach <- df_hornbach[c(-22:-23)]


# *************** calculate variable importance

counter <- length(df_hornbach)
importance <- vector()


for (k in 1:counter) {
  imp <- 100-sum(is.na(df_hornbach[k]))
  importance <- c(importance,imp)
}

#importance <- as.numeric(importance)

#median.matrix <- matrix(data=NA, nrow=counter, ncol=2)
median.matrix.hornbach <- matrix(data=1, nrow=counter, ncol=1)

for (k in 1:counter) {
  median.matrix.hornbach[k] <- median(na.omit(df_hornbach[[k]]))
}

# missing lines
vec <- c(19,20)
new_mat_hornbach <- matrix(data=NA, nrow=29, ncol=1)
new_mat_hornbach[-vec,] <- median.matrix.hornbach   



# ........ RAPPETAL ........
# *************** count coeffcients used in models
# - load one file to get names of coefficients for site and create file to fill with loop
table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Rappetal/Rappetal.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_1_1.txt", sep="") , 
                               stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)

table.coef$beta <- as.numeric(table.coef$beta)
table.coef[table.coef == 0] <- NA

summary <- table.coef[1:2]



n.folds=5
k.boot=20


for ( i in 1:n.folds ) {
  for ( j in 1:k.boot ) {
    
    table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Rappetal/Rappetal.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_",i,"_",j,".txt", sep="") , 
                                   stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)
    
    table.coef$beta <- as.numeric(table.coef$beta)
    table.coef[table.coef == 0] <- NA
    #assign(paste("beta_",i,"_",j,sep=""), table.coef$beta)
    summary <- cbind(summary, table.coef$beta)
  }
}


summary_1 <- summary[-1]
df_rappetal<- transpose(summary_1[-1])
colnames(df_rappetal) <- summary_1[, 1]
str(df_rappetal)
df_rappetal <- df_rappetal[c(-1)]
str(df_rappetal)

# remove geology to make comparable to other sites?

#df_rappetal <- df_rappetal[c(-22:-28)]


# *************** calculate variable importance

counter <- length(df_rappetal)
importance <- vector()


for (k in 1:counter) {
  imp <- 100-sum(is.na(df_rappetal[k]))
  importance <- c(importance,imp)
}

#importance <- as.numeric(importance)

#median.matrix <- matrix(data=NA, nrow=counter, ncol=2)
median.matrix.rappetal <- matrix(data=1, nrow=counter, ncol=1)

for (k in 1:counter) {
  median.matrix.rappetal[k] <- median(na.omit(df_rappetal[[k]]))
}

# missing lines
vec <- c(18,19)
new_mat_rappetal <- matrix(data=NA, nrow=29, ncol=1)
new_mat_rappetal[-vec,] <- median.matrix.rappetal



# ........ TURBACH ........
# *************** count coeffcients used in models
# - load one file to get names of coefficients for site and create file to fill with loop
table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Turbach/Turbach.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_1_1.txt", sep="") , 
                               stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)

table.coef$beta <- as.numeric(table.coef$beta)
table.coef[table.coef == 0] <- NA

summary <- table.coef[1:2]


n.folds=5
k.boot=20


for ( i in 1:n.folds ) {
  for ( j in 1:k.boot ) {
    
    table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Turbach/Turbach.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_",i,"_",j,".txt", sep="") , 
                                   stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)
    
    table.coef$beta <- as.numeric(table.coef$beta)
    table.coef[table.coef == 0] <- NA
    #assign(paste("beta_",i,"_",j,sep=""), table.coef$beta)
    summary <- cbind(summary, table.coef$beta)
  }
}


summary_1 <- summary[-1]
df_turbach <- transpose(summary_1[-1])
colnames(df_turbach) <- summary_1[, 1]
str(df_turbach)
df_turbach <- df_turbach[c(-1)]
str(df_turbach)

# remove geology to make comparable to other sites?


# TODO
#df_turbach <- df_turbach[c(-22:-30)]


# *************** calculate variable importance

counter <- length(df_turbach)
importance <- vector()


for (k in 1:counter) {
  imp <- 100-sum(is.na(df_turbach[k]))
  importance <- c(importance,imp)
}

#importance <- as.numeric(importance)

#median.matrix <- matrix(data=NA, nrow=counter, ncol=2)
median.matrix.turbach <- matrix(data=1, nrow=counter, ncol=1)

for (k in 1:counter) {
  median.matrix.turbach[k] <- median(na.omit(df_turbach[[k]]))
}

# missing lines
vec <- c(19,20)
new_mat_turbach <- matrix(data=NA, nrow=29, ncol=1)
new_mat_turbach[-vec,] <- median.matrix.turbach



# ........ URSEREN ........

# *************** count coeffcients used in models
# - load one file to get names of coefficients for site and create file to fill with loop
table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Urseren/Urseren.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_1_1.txt", sep="") , 
                               stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)

table.coef$beta <- as.numeric(table.coef$beta)
table.coef[table.coef == 0] <- NA

summary <- table.coef[1:2]


n.folds=5
k.boot=20


for ( i in 1:n.folds ) {
  for ( j in 1:k.boot ) {
    
    table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Urseren/Urseren.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_",i,"_",j,".txt", sep="") , 
                                   stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)
    
    table.coef$beta <- as.numeric(table.coef$beta)
    table.coef[table.coef == 0] <- NA
    #assign(paste("beta_",i,"_",j,sep=""), table.coef$beta)
    summary <- cbind(summary, table.coef$beta)
  }
}


summary_1 <- summary[-1]
df_urseren <- transpose(summary_1[-1])
colnames(df_urseren) <- summary_1[, 1]
str(df_urseren)
df_urseren <- df_urseren[c(-1)]
str(df_urseren)

# remove geology to make comparable to other sites?

# TODO
#df_urseren <- df_urseren[c(-22:-34)]



# *************** calculate variable importance

counter <- length(df_urseren)
importance <- vector()


for (k in 1:counter) {
  imp <- 100-sum(is.na(df_urseren[k]))
  importance <- c(importance,imp)
}

#importance <- as.numeric(importance)

#median.matrix <- matrix(data=NA, nrow=counter, ncol=2)
median.matrix.urseren <- matrix(data=1, nrow=counter, ncol=1)

for (k in 1:counter) {
  median.matrix.urseren[k] <- median(na.omit(df_urseren[[k]]))
}


# missing lines
# vec <- c()
# new_mat_urseren <- matrix(data=NA, nrow=29, ncol=1)
# new_mat_urseren[-vec,] <- median.matrix.urseren





# ........ VAL CLUOZZA ........

# - load one file to get names of coefficients for site and create file to fill with loop
table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Cluozza/Val_Cluozza.model.2.Gestk.coeffiecients_cv_grpreg_grLasso_1_1.txt", sep="") , 
                               stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)

table.coef$beta <- as.numeric(table.coef$beta)
table.coef[table.coef == 0] <- NA

summary <- table.coef[1:2]



n.folds=5
k.boot=20


for ( i in 1:n.folds ) {
  for ( j in 1:k.boot ) {
    
    table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Cluozza/Val_Cluozza.model.2.Gestk.coeffiecients_cv_grpreg_grLasso_",i,"_",j,".txt", sep="") , 
                                   stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)
    
    table.coef$beta <- as.numeric(table.coef$beta)
    table.coef[table.coef == 0] <- NA
    #assign(paste("beta_",i,"_",j,sep=""), table.coef$beta)
    summary <- cbind(summary, table.coef$beta)
  }
}

summary_1 <- summary[-1]
df_val.cluozza <- transpose(summary_1[-1])
colnames(df_val.cluozza) <- summary_1[, 1]
str(df_val.cluozza)
df_val.cluozza <- df_val.cluozza[c(-1)]
str(df_val.cluozza)

# remove geology to make comparable to other sites?

#df_val.cluozza <- df_val.cluozza[c(-22:-27)]
str(df_val.cluozza)


# *************** calculate variable importance

counter <- length(df_val.cluozza)
importance <- vector()


for (k in 1:counter) {
  imp <- 100-sum(is.na(df_val.cluozza[k]))
  importance <- c(importance,imp)
}

#importance <- as.numeric(importance)

#median.matrix <- matrix(data=NA, nrow=counter, ncol=2)
median.matrix.val.cluozza <- matrix(data=1, nrow=counter, ncol=1)

for (k in 1:counter) {
  median.matrix.val.cluozza[k] <- median(na.omit(df_val.cluozza[[k]]))
}



# missing lines
vec <- c(19)
new_mat_val.cluozza <- matrix(data=NA, nrow=29, ncol=1)
new_mat_val.cluozza[-vec,] <- median.matrix.val.cluozza



# ........ VAL D'ENTREMONT ........

# *************** count coeffcients used in models
# - load one file to get names of coefficients for site and create file to fill with loop
table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_D_Entremont/Val_D_Entremont.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_1_1.txt", sep="") , 
                               stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)

table.coef$beta <- as.numeric(table.coef$beta)
table.coef[table.coef == 0] <- NA

summary <- table.coef[1:2]


n.folds=5
k.boot=20


for ( i in 1:n.folds ) {
  for ( j in 1:k.boot ) {
    
    table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_D_Entremont/Val_D_Entremont.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_",i,"_",j,".txt", sep="") , 
                                   stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)
    
    table.coef$beta <- as.numeric(table.coef$beta)
    table.coef[table.coef == 0] <- NA
    #assign(paste("beta_",i,"_",j,sep=""), table.coef$beta)
    summary <- cbind(summary, table.coef$beta)
  }
}


summary_1 <- summary[-1]
df_val.d.entremont <- transpose(summary_1[-1])
colnames(df_val.d.entremont) <- summary_1[, 1]
str(df_val.d.entremont)
df_val.d.entremont <- df_val.d.entremont[c(-1)]
str(df_val.d.entremont)

# remove geology to make comparable to other sites?

# TODO
#df_val.d.entremont <- df_val.d.entremont[c(-22:-27)]


# *************** calculate variable importance

counter <- length(df_val.d.entremont)
importance <- vector()


for (k in 1:counter) {
  imp <- 100-sum(is.na(df_val.d.entremont[k]))
  importance <- c(importance,imp)
}

#importance <- as.numeric(importance)

#median.matrix <- matrix(data=NA, nrow=counter, ncol=2)
median.matrix.val.d.entremont <- matrix(data=1, nrow=counter, ncol=1)

for (k in 1:counter) {
  median.matrix.val.d.entremont[k] <- median(na.omit(df_val.d.entremont[[k]]))
}


# missing lines
vec <- c(19)
new_mat_val.d.entremont <- matrix(data=NA, nrow=29, ncol=1)
new_mat_val.d.entremont[-vec,] <- median.matrix.val.d.entremont


# ........ VAL PIORA ........

# *************** count coeffcients used in models
# - load one file to get names of coefficients for site and create file to fill with loop
table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora/Val_Piora.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_1_1.txt", sep="") , 
                               stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)

table.coef$beta <- as.numeric(table.coef$beta)
table.coef[table.coef == 0] <- NA

summary <- table.coef[1:2]


n.folds=5
k.boot=20


for ( i in 1:n.folds ) {
  for ( j in 1:k.boot ) {
    
    table.coef <- head(read.table( paste("/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Val_Piora/Val_Piora.model.1.Gestkl.coeffiecients_cv_grpreg_grLasso_",i,"_",j,".txt", sep="") , 
                                   stringsAsFactors=FALSE, row.names=NULL, dec=".", fill=TRUE, flush=TRUE, header = TRUE, skip=1), -4)
    
    table.coef$beta <- as.numeric(table.coef$beta)
    table.coef[table.coef == 0] <- NA
    #assign(paste("beta_",i,"_",j,sep=""), table.coef$beta)
    summary <- cbind(summary, table.coef$beta)
  }
}


summary_1 <- summary[-1]
df_val.piora <- transpose(summary_1[-1])
colnames(df_val.piora) <- summary_1[, 1]
str(df_val.piora)
df_val.piora <- df_val.piora[c(-1)]
str(df_val.piora)

# remove geology to make comparable to other sites?

# TODO
#df_val.piora <- df_val.piora[c(-22:-29)]


# *************** calculate variable importance

counter <- length(df_val.piora)
importance <- vector()


for (k in 1:counter) {
  imp <- 100-sum(is.na(df_val.piora[k]))
  importance <- c(importance,imp)
}

#importance <- as.numeric(importance)

#median.matrix <- matrix(data=NA, nrow=counter, ncol=2)
median.matrix.val.piora <- matrix(data=1, nrow=counter, ncol=1)

for (k in 1:counter) {
  median.matrix.val.piora[k] <- median(na.omit(df_val.piora[[k]]))
}

# missing lines
vec <- c(18)
new_mat_val.piora <- matrix(data=NA, nrow=29, ncol=1)
new_mat_val.piora[-vec,] <- median.matrix.val.piora









matrix.full <- matrix(data=NA, nrow=29, ncol=10)

matrix.full[,1] <- median.matrix.arosa
matrix.full[,2] <- new_mat_baulmes
matrix.full[,3] <- new_mat_chrauchtal
matrix.full[,4] <- new_mat_hornbach
matrix.full[,5] <- new_mat_rappetal
matrix.full[,6] <- new_mat_turbach
matrix.full[,7] <- median.matrix.urseren
matrix.full[,8] <- new_mat_val.cluozza
matrix.full[,9] <- new_mat_val.d.entremont
matrix.full[,10] <- new_mat_val.piora

labels.var <- c("Elevation", "Slope", "TWI", "Flow acc.", "Curvature plan", "Curvature profile", "Roughness", "Dist. to Roads", 
                "Dist. to Streams", "Roads density", "Stream density", "Max. Precip. (10Y)", 
                "Max. Precip. (5Y)", "Snow days", "Snow cover days", "Growing season length", "Frost change freq.", 
                "Unconsolidated Rocks", "Igneous Rocks", "Metamorphic Rocks", "Sedimentary Rocks",
                "Aspect E", "Aspect N", "Aspect NE", "Aspect NW",
                "Aspect S", "Aspect SE", "Aspect SW", "Aspect W")

labels.sites <-c("Arosa", "Baulmes", "Chrauchtal", "Hornbach", "Rappetal", "Turbach", "Urseren", 
                 "Val Cluozza", "Val d'Entremont", "Val Piora")

rownames(matrix.full) <- labels.var
colnames(matrix.full) <- labels.sites

range <- max(na.omit(abs(matrix.full)))


colors <- c("#002F70","#0A468D","#295EAE","#4A76C7","#6F8DD2","#8EA4DE","#ABBBE8","#C5CFF0",
            "#F3C5C5","#EAACAC","#DD9191","#CE7575","#BD5758","#A13F3F","#7F2A2B","#5F1415")

# colors need one less class than breaks in pheatmap

pdf(file="/Volumes/Lauren_Uni/M3_SSGM/R_Modelling/Coefficients/Heatmap_Coefficients_Gesteinsklasse.pdf")

pheatmap(matrix.full, breaks = seq(-range, range, length.out = 17), fontsize=12, border_color=NA, display_numbers = T, angle_col=45, 
         fontsize_number = 11, number_color = "white", 
         cluster_cols=F, cluster_rows = F, scale="none",
         col=colors, na_col = "white", legend=F)
dev.off()







