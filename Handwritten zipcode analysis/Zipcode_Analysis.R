ziptrain<-read.table('zip.train.csv',sep=",")
dim(ziptrain)
ziptrain27<-subset(ziptrain,ziptrain[,1]==2|ziptrain[,1]==7)
dim(ziptrain27)
head(ziptrain27,n=10)

# ------------------------------------- Visualise dis. of digt_id ------------------------------------
library(MASS)              
digit<-ziptrain27[,1]     
digit.freq = table(digit)  
bp<-barplot(digit.freq,main="Distribution of digit id") 
text(bp, 5, digit.freq,cex=1,pos=3) 

# -------------------------------------- Exploratory data analysis -----------------------------------
summary(ziptrain27) # Stat
# Correlation
corr<-as.data.frame(round(cor(ziptrain27),2))
dim(corr)
## To see the letter picture of the 5-th row by changing the row observation to a matrix
rowindex = 15 ## You can try other "rowindex" values to see other rows
ziptrain27[rowindex,1]
Xval = t(matrix(data.matrix(ziptrain27[,-1])[rowindex,],byrow=TRUE,16,16)[16:1,])
image(Xval,col=gray(0:1),axes=FALSE) ## Also try "col=gray(0:32/32)"


# --------------------------------------- Building model --------------------------------------------
## Linear Regression
mod1 <- lm( V1 ~ . , data= ziptrain27)
pred1.train <- predict.lm(mod1, ziptrain27[,-1])
y1pred.train <- 2 + 5*(pred1.train >= 4.5); ## Here 4.5 = (2+7)/2, 2 + 5*0 = 2 and 2+5*1 = 7.
mean( y1pred.train != ziptrain27[,1])

## KNN
library(class)

train_err<-NULL;
for (i in seq(1, 15, by = 2)){
  kk <- i;
  xnew <- ziptrain27[,-1];
  ypred2.train <- knn(ziptrain27[,-1], xnew, ziptrain27[,1], k=kk);
  temptrain_err<-mean( ypred2.train != ziptrain27[,1])
  train_err <- c(train_err, temptrain_err)
}

train_err
seq(1, 15, by = 2)
# Visualise Training erro vs K
plot(seq(1, 15, by = 2), train_err,xlab='K', ylab='Training error', main='Training error of KNN')

# ---------------------------------------- Testing error -------------------------------------
ziptest<-read.table('zip.test.csv',sep=",")
ziptest27<-subset(ziptest,ziptest[,1]==2|ziptest[,1]==7)
dim(ziptest27)

## New Linear Regression
mod2 <- lm( V1 ~ . , data= ziptrain27)
pred.test <- predict.lm(mod1, ziptest27[,-1])
y1pred.test <- 2 + 5*(pred.test >= 4.5); ## Here 4.5 = (2+7)/2, 2 + 5*0 = 2 and 2+5*1 = 7.
mean( y1pred.test != ziptest27[,1])

# New KNN
test_err<-NULL
for (j in seq(1,15,by=2)){
xnew2 <- ziptest27[,-1]; ## xnew2 is the X variables of the "testing" data
kk <- j; ## below we use the training data "ziptrain27" to predict xnew2 via KNN
ypred2.test <- knn(ziptrain27[,-1], xnew2, ziptrain27[,1], k=kk);
temptrain_err<-mean( ypred2.test != ziptest27[,1])
test_err<-c(test_err,temptrain_err)
}
test_err

plot(seq(1,15,by=2),test_err,xlab='K', ylab='Testing error',main='Testing error of KNN')

# -------------------------------------- Monte Carlo CV ---------------------------------------------
zip27full<-rbind(ziptrain27,ziptest27)
dim(zip27full)

n1 = 1376; # training set sample size
n2= 345; # testing set sample size
n = dim(zip27full)[1]; ## the total sample size
set.seed(7406); ### set the seed for randomization
### Initialize the TE values for all models in all $B=100$ loops
B= 100; ### number of loops
TEALL = NULL;### Final TE values

library(class)
for (b in 1:B){
  ### randomly select n1 observations as a new training subset in each loop
  flag <- sort(sample(1:n, n1));
  zip27traintemp <- zip27full[flag,]; ## temp training set for CV
  zip27testtemp <- zip27full[-flag,]; ## temp testing set for CV
  
  # Testing data w/o dep. var
  MC_xnew2 <- zip27testtemp[,-1];
  
  # lm
  MC_lm <- lm( V1 ~ . , data= zip27traintemp)
  MC_lm_pred1 <- predict.lm(MC_lm, MC_xnew2)
  MC_lm_y1pred <- 2 + 5*(MC_lm_pred1 >= 4.5); ## Here 4.5 = (2+7)/2, 2 + 5*0 = 2 and 2+5*1 = 7.
  te0<-mean( MC_lm_y1pred != zip27testtemp[,1])

  # KNN
 
  # k=1
  MC_ypred2.test <- knn(zip27traintemp[,-1], MC_xnew2, zip27traintemp[,1], k=1);
  te1<-mean( MC_ypred2.test != zip27testtemp[,1])
  
  # k=3
  MC_ypred2.test <- knn(zip27traintemp[,-1], MC_xnew2, zip27traintemp[,1], k=3);
  te2<-mean( MC_ypred2.test != zip27testtemp[,1])
  
  # k=5
  MC_ypred2.test <- knn(zip27traintemp[,-1], MC_xnew2, zip27traintemp[,1], k=5);
  te3<-mean( MC_ypred2.test != zip27testtemp[,1])
  
  # k=7
  MC_ypred2.test <- knn(zip27traintemp[,-1], MC_xnew2, zip27traintemp[,1], k=7);
  te4<-mean( MC_ypred2.test != zip27testtemp[,1])
  
  # k=9
  MC_ypred2.test <- knn(zip27traintemp[,-1], MC_xnew2, zip27traintemp[,1], k=9);
  te5<-mean( MC_ypred2.test != zip27testtemp[,1])
  
  # k=11
  MC_ypred2.test <- knn(zip27traintemp[,-1], MC_xnew2, zip27traintemp[,1], k=11);
  te6<-mean( MC_ypred2.test != zip27testtemp[,1])
  
  # k=13
  MC_ypred2.test <- knn(zip27traintemp[,-1], MC_xnew2, zip27traintemp[,1], k=13);
  te7<-mean( MC_ypred2.test != zip27testtemp[,1])
  
  # k=15
  MC_ypred2.test <- knn(zip27traintemp[,-1], MC_xnew2, zip27traintemp[,1], k=15);
  te8<-mean( MC_ypred2.test != zip27testtemp[,1])
  
  # Output matrix
  TEALL = rbind( TEALL, cbind(te0, te1, te2, te3, te4, te5, te6, te7, te8) )
      
}

#dim(TEALL) ### This should be a Bx9 matrices
### if you want, you can change the column name of TEALL
colnames(TEALL) <- c("linearRegression", "KNN1", "KNN3", "KNN5", "KNN7",
                     "KNN9", "KNN11", "KNN13", "KNN15")

#df_TEALL<-as.data.frame(TEALL)

# You can report the sample mean/variances of the testing errors so as to compare these models
TEALL_mean<-as.data.frame(apply(TEALL, 2, mean))
TEALL_var<-as.data.frame(apply(TEALL, 2, var))

# Plot TE_mean
TEALL_mean <- cbind(models = rownames(TEALL_mean), TEALL_mean)
rownames(TEALL_mean) <- 1:nrow(TEALL_mean)
names(TEALL_mean)[names(TEALL_mean) == "apply(TEALL, 2, mean)"] <- "TE_mean"

TEALL_mean$models <- factor(TEALL_mean$models, levels = TEALL_mean$models)

library(ggplot2)
# geom_point() is for scatter plot
TE_mean_plot<-ggplot(TEALL_mean, aes(as.factor(models),TE_mean)) + geom_point()
TE_mean_plot+ xlab('Models')+ylab('TE_mean')+ggtitle('Testing_error_mean of models')+
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Plot TE_var
TEALL_var <- cbind(models = rownames(TEALL_var), TEALL_var)
rownames(TEALL_var) <- 1:nrow(TEALL_var)
names(TEALL_var)[names(TEALL_var) == "apply(TEALL, 2, var)"] <- "TE_var"

TEALL_var$models <- factor(TEALL_var$models, levels = TEALL_var$models)

library(ggplot2)
# geom_point() is for scatter plot
TE_var_plot<-ggplot(TEALL_var, aes(as.factor(models),TE_var)) + geom_point()
TE_var_plot+ xlab('Models')+ylab('TE_var')+ggtitle('Testing_error_var of models') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




# -------------------------------------------- Training error -----------------------------------------
TrainEALL = NULL;### Final TE values

library(class)
for (b in 1:B){
  ### randomly select n1 observations as a new training subset in each loop
  flag <- sort(sample(1:n, n1));
  zip27traintemp <- zip27full[flag,]; ## temp training set for CV
  #zip27testtemp <- zip27full[-flag,]; ## temp testing set for CV
  
  # Training data w/o dep. var
  MC_xnew <- zip27traintemp[,-1];
  
  # lm
  MC_lm <- lm( V1 ~ . , data= zip27traintemp)
  MC_lm_pred1 <- predict.lm(MC_lm, MC_xnew)
  MC_lm_y1pred <- 2 + 5*(MC_lm_pred1 >= 4.5); ## Here 4.5 = (2+7)/2, 2 + 5*0 = 2 and 2+5*1 = 7.
  te0<-mean( MC_lm_y1pred != zip27traintemp[,1])
  
  # KNN
  
  # k=1
  MC_ypred.test1 <- knn(zip27traintemp[,-1], MC_xnew, zip27traintemp[,1], k=1);
  te1<-mean( MC_ypred.test1 != zip27traintemp[,1])
  
  # k=3
  MC_ypred.test3 <- knn(zip27traintemp[,-1], MC_xnew, zip27traintemp[,1], k=3);
  te2<-mean( MC_ypred.test3 != zip27traintemp[,1])
  
  # k=5
  MC_ypred.test5 <- knn(zip27traintemp[,-1], MC_xnew, zip27traintemp[,1], k=5);
  te3<-mean( MC_ypred.test5 != zip27traintemp[,1])
  
  # k=7
  MC_ypred.test7 <- knn(zip27traintemp[,-1], MC_xnew, zip27traintemp[,1], k=7);
  te4<-mean( MC_ypred.test7 != zip27traintemp[,1])
  
  # k=9
  MC_ypred.test9 <- knn(zip27traintemp[,-1], MC_xnew, zip27traintemp[,1], k=9);
  te5<-mean( MC_ypred.test9 != zip27traintemp[,1])
  
  # k=11
  MC_ypred.test11 <- knn(zip27traintemp[,-1], MC_xnew, zip27traintemp[,1], k=11);
  te6<-mean( MC_ypred.test11 != zip27traintemp[,1])
  
  # k=13
  MC_ypred.test13 <- knn(zip27traintemp[,-1], MC_xnew, zip27traintemp[,1], k=13);
  te7<-mean( MC_ypred.test13 != zip27traintemp[,1])
  
  # k=15
  MC_ypred.test15 <- knn(zip27traintemp[,-1], MC_xnew, zip27traintemp[,1], k=15);
  te8<-mean( MC_ypred.test15 != zip27traintemp[,1])
  
  # Output matrix
  TrainEALL = rbind( TrainEALL, cbind(te0, te1, te2, te3, te4, te5, te6, te7, te8) )
  
}

colnames(TrainEALL) <- c("linearRegression", "KNN1", "KNN3", "KNN5", "KNN7",
                     "KNN9", "KNN11", "KNN13", "KNN15")

df_TrainEALL<-as.data.frame(TrainEALL)
# You can report the sample mean/variances of the testing errors so as to compare these models
TrainEALL_mean<-as.data.frame(apply(TrainEALL, 2, mean))
TrainEALL_var<-as.data.frame(apply(TrainEALL, 2, var))

TrainEALL_mean
# Plot Train_mean
TrainEALL_mean <- cbind(models = rownames(TrainEALL_mean), TrainEALL_mean)
rownames(TrainEALL_mean) <- 1:nrow(TrainEALL_mean)
names(TrainEALL_mean)[names(TrainEALL_mean) == "apply(TrainEALL, 2, mean)"] <- "Train_mean"
TrainEALL_mean

TrainEALL_mean$models <- factor(TrainEALL_mean$models, levels = TrainEALL_mean$models)
TrainEALL_mean
library(ggplot2)
# geom_point() is for scatter plot
TrainEALL_mean_plot<-ggplot(TrainEALL_mean, aes(as.factor(models),Train_mean)) + geom_point()
TrainEALL_mean_plot+ xlab('Models')+ylab('Train_mean')+ggtitle('Training_error_mean of models')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Plot TE_var
TrainEALL_var <- cbind(models = rownames(TrainEALL_var), TrainEALL_var)
rownames(TrainEALL_var) <- 1:nrow(TrainEALL_var)
names(TrainEALL_var)[names(TrainEALL_var) == "apply(TrainEALL, 2, var)"] <- "Train_var"

TrainEALL_var$models <- factor(TrainEALL_var$models, levels = TrainEALL_var$models)

library(ggplot2)
# geom_point() is for scatter plot
TrainEALL_var_plot<-ggplot(TrainEALL_var, aes(as.factor(models),Train_var)) + geom_point()
TrainEALL_var_plot+ xlab('Models')+ylab('Train_var')+ggtitle('Training_error_var of models') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

