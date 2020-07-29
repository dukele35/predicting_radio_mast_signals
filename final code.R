library(readxl)
library(dplyr)
library(DataExplorer)
df1 <- read_excel('RF_TrainingDatasetA_Final.xlsx')
df2 <- read_excel('RF_ScoringDatasetA_Final.xlsx')

#### A.1. Basic Cleaning ####
# checking duplications --> no duplications
# remove column RFDBiD, i.e. identification
df1 <- df1[, -c(1)]
df2 <- df2[, -c(1)]

# dropping Antennafilename1, 2
df1 <- df1[,-c(2,3)]
df2 <- df2[, -c(1,2)]

# apart from missing rows of the outcome, 
# the outcome perfectly matches with eng_class
# therefore, remove out_come
df1 <- df1[,-c(76)]

# converting few character columns into number ones:
# Dispersive...B1, Dispersive...B2, Miscell...B1, Miscell...B2, PassiveGain...dB
# df1
df1$DispersivefademargindB1 <- as.numeric(df1$DispersivefademargindB1)
df1$DispersivefademargindB2 <- as.numeric(df1$DispersivefademargindB2)
df1$MiscellaneouslossdB1 <- as.numeric(df1$MiscellaneouslossdB1)
df1$MiscellaneouslossdB2 <- as.numeric(df1$MiscellaneouslossdB2)
df1$Passivegain2dB <- as.numeric(df1$Passivegain2dB)
str(df1)
# df2
df2$DispersivefademargindB1 <- as.numeric(df2$DispersivefademargindB1)
df2$DispersivefademargindB2 <- as.numeric(df2$DispersivefademargindB2)
df2$MiscellaneouslossdB1 <- as.numeric(df2$MiscellaneouslossdB1)
df2$MiscellaneouslossdB2 <- as.numeric(df2$MiscellaneouslossdB2)
df2$Passivegain2dB <- as.numeric(df2$Passivegain2dB)
str(df2)

# fixing white space in characters 
# df1
for(i in 1:ncol(df1)){
  if(is.character(df1[[i]])){
    df1[[i]] <- gsub(" ", "_", df1[[i]])
  }
}
# df2
for(i in 1:ncol(df2)){
  if(is.character(df2[[i]])){
    df2[[i]] <- gsub(" ", "_", df2[[i]])
  }
}

# Factorising all character columns in df1 & df2
# df1
for(i in 1:ncol(df1)){
  if(is.character(df1[[i]])){
    df1[[i]] <- as.factor(df1[[i]])
  }
}
# df2
for(i in 1:ncol(df2)){
  if(is.character(df2[[i]])){
    df2[[i]] <- as.factor(df2[[i]])
  }
}

# checking missing values 
for(i in 1:ncol(df1)){
  print(colnames(df1[i]))
  print(sum(is.na(df1[[i]])))
}
# In df1, 9 missing values for column DpQ_R2 & 
# 6 missing values for column Fullmint1
for(i in 1:ncol(df2)){
  print(colnames(df2[i]))
  print(sum(is.na(df2[[i]])))
}
# In df2, there is no missing value
# dropping missing values in df1
df1 <- na.omit(df1)
dim(df1)

# Screening the dataset
str(df1)
str(df2)
introduce(df1)
introduce(df2)


#### A.2. EDA #### 
#### A.2.1. Univariate Analysis ####
library(DataExplorer)
# class barplot
bp1 <-  barplot(table(df1$Eng_Class), 
                main = 'Barplot of Eng_Class (training set)', 
                ylab = 'Count',
                ylim = c(0,2200)) 
text(bp1, 
     table(df1$Eng_Class) + 100, 
     labels = table(df1$Eng_Class), 
     cex = 1.1)




# finding index of numeric columns
# df1
df1_index_numeric <- c()
for(i in 1:ncol(df1)){
  if(is.numeric(df1[[i]])){
    df1_index_numeric <- c(df1_index_numeric, i)
  }
}
# df2
df2_index_numeric <- c()
for(i in 1:ncol(df2)){
  if(is.numeric(df2[[i]])){
    df2_index_numeric <- c(df2_index_numeric, i)
  }
}

# finding index of factor columns
# df1
df1_index_factor <- c()
for(i in 1:ncol(df1)){
  if(is.factor(df1[[i]])){
    df1_index_factor <- c(df1_index_factor, i)
  }
}
# df2
df2_index_factor <- c()
for(i in 1:ncol(df2)){
  if(is.factor(df2[[i]])){
    df2_index_factor <- c(df2_index_factor, i)
  }
}

df1_num <- df1[,df1_index_numeric]
df1_fac <- df1[,df1_index_factor]
df2_num <- df2[,df2_index_numeric]
df2_fac <- df2[,df2_index_factor]

# barplot and histogram
plot_histogram(df1_num)
plot_bar(df1_fac[,-c(1)], 
         nrow = 1,
         ncol = 1,
         maxcat = 152)



#### A.2.2. Bivariate Analysis ####
## correlation between numeric variables 
# building correlation matrix among variables
# the column Dispersivefadeoccurrencefactor contains all value 1 
# therefore, removing this column for the correlation matrix
library(corrplot)
cor.mat <- cor(df1_num[,-c(15)], 
               use = 'pairwise.complete.obs',
               method = 'pearson')
corrplot(cor.mat, tl.pos = 'n')

# Chi-square tests
library(greybox)
chi_elements <- c()
pchi_elements <- c()
df1_fac1 <- df1_fac
for(i in 1:length(df1_fac1)){
  for(j in 1:length(df1_fac1)){
    chi <- cramer(df1_fac1[[i]], df1_fac1[[j]])
    chi_elements <- c(chi_elements, chi$value)
    pchi_elements <- c(pchi_elements, chi$p.value)
  }
}
chi.mat <- matrix(chi_elements, 
                  nrow = length(df1_fac1), 
                  dimnames = list(names(df1_fac1), names(df1_fac1)))
pchi.mat <- matrix(pchi_elements, 
                   nrow = length(df1_fac1), 
                   dimnames = list(names(df1_fac1), names(df1_fac1)))
corrplot(chi.mat, 
         tl.col = 'black', 
         addCoef.col = 'black', 
         number.cex = 0.7)



# Intraclass coefficients
library(ICC)
intra_elements <- c()
p.intra_elements <- c()
for(i in df1_index_numeric){
  for(j in df1_index_factor){
    # create a vector of intraclass coefficients
    ano <- ICCest(df1[[j]], df1[[i]], alpha = 0.01)
    intra_elements <- c(intra_elements, ano$ICC)
    # create a vector of p-values
    anova_test <- aov(df1[[i]] ~ df1[[j]])
    p.intra <- summary(anova_test)[[1]][["Pr(>F)"]][1]
    p.intra_elements <- c(p.intra_elements, p.intra)
  }
}
intra.mat <- matrix(intra_elements,
                    nrow = length(df1_index_numeric),
                    byrow = TRUE,
                    dimnames = list(names(df1_num), names(df1_fac)))
library(reshape2)
melted_cormat1 <- melt(intra.mat)
melted_cormat1 <- melted_cormat1[1:63,]
melted_cormat1 <- melted_cormat1[order(melted_cormat1[,3]),]
barplot(melted_cormat1[,3], 
        names.arg = melted_cormat1[,1],
        las = 1,
        xlim = c(0,0.7),
        ylim = c(0,0.65),
        cex.names=0.5,
        horiz = T,
        width = 0.01
        )
abline(v=0.5)


#### A.3. Preprocessing data #### 
#### A.3.1.Creating dummy variables ####
library(caret)
# df1 (training set)
df1_dummies <- dummyVars(Eng_Class ~., data = df1)
df1_dummies <- predict(df1_dummies, newdata = df1)
df1_dummies <- as.data.frame(df1_dummies)
dim(df1_dummies)
df1_dummies$Eng_Class <- df1$Eng_Class
dim(df1_dummies)
# df2 (scoring set)
df2$Eng_Class <- c(0,1)                      # fake column
df2_dummies <- dummyVars(Eng_Class~., data = df2)
df2_dummies <- predict(df2_dummies, newdata = df2)
df2_dummies <- as.data.frame(df2_dummies)
dim(df2_dummies)

#### A.3.2. zero and near zero-variance predictors ####
nzv <- nearZeroVar(df1_dummies)
df1_nzv <- df1_dummies[,-nzv]
dim(df1_nzv)
str(df1_nzv)


#### A.3.3. Identify highly-correlated predictors####
cor_matrix <- cor(df1_nzv[, -c(89)])    # removing Eng_Class from the correlation matrix
hist(cor_matrix[upper.tri(cor_matrix)],
     xlab = 'correlation coefficients',
     main = 'Histogram of correlation coefficients')
# visualising different cutoff with no. variables deleted
x <- seq(0,1, by=0.001)
y <- c()
for(i in x){
  y <- c(y, length(findCorrelation(cor_matrix, cutoff = i)))      # this takes 20 seconds
}
cut = 0.982
plot(x,y, type = 'l',
     ylab = 'Number of variables deleted',
     xlab = 'Cutoff level of correlation coefficient',
     main = 'Plot of No. delelted variables vs. Corr Coefficient cutoff')
abline(v = cut, col = 'red')
abline(h = length(findCorrelation(cor_matrix, cutoff = cut)), col = 'red')
length(findCorrelation(cor_matrix, cutoff = cut))
# filtering highly correlated variables
highly_cor_vars <- findCorrelation(cor_matrix, cutoff = cut)
df1_fil_cor <- df1_nzv[,-highly_cor_vars]
dim(df1_nzv)
dim(df1_fil_cor)


#### A.3.4. fixing linear dependencies ####
comboInfo <- findLinearCombos(df1_fil_cor[,-c(57)])    # removing Eng_Class from the dataset
comboInfo   



#### A.3.5. subset columns for df2 ####
needed_col1 <- colnames(df1_fil_cor[,-c(57)])    # removing Eng_Class from training set
length(needed_col1)
needed_col2 = c()
for(i in 1:ncol(df2_dummies)){
  if(colnames(df2_dummies[i]) %in% needed_col1){
    needed_col2 = c(needed_col2, i)
  }
}
df2_fil_cor <- df2_dummies[,needed_col2]
dim(df2_fil_cor)


#### A.3.6. Scaling the data ####
preProcValues <- preProcess(df1_fil_cor[,-c(57)], 
                            method = c("center", "scale", "BoxCox", "pca"))
df1_trans <- predict(preProcValues, df1_fil_cor[,-c(57)])
df1_trans$Eng_Class <- df1_fil_cor$Eng_Class
df2_trans <- predict(preProcValues, df2_fil_cor)

str(df1_trans)
str(df2_trans)



#### A.3.7. Splitting and Subsampling the data ####
# splitting
library(caTools)
set.seed(696)
split1 = sample.split(df1_trans$Eng_Class, SplitRatio = 0.5)
pre_training <- subset(df1_trans, split1 == TRUE)
not_training <- subset(df1_trans, split1 == FALSE)
dim(pre_training) # dimension of the training set before subsampling
barplot(table(pre_training$Eng_Class))   # classes of the training set before subsampling

split2 <- sample.split(not_training$Eng_Class, SplitRatio = 0.5)
testing <- subset(not_training, split2 == TRUE)
validation <- subset(not_training, split2 == FALSE)
dim(testing)  # dimension of the test set
barplot(table(testing$Eng_Class)) # classes of the test set
dim(validation)  # dimension of the validation set
barplot(table(validation$Eng_Class)) # classes of the validation set


# subsampling training set (SMOTE)
library(DMwR)
set.seed(696)
training <- SMOTE(Eng_Class ~ ., data  = pre_training)
dim(training) # dimension of the training set after subsampling 
barplot(table(training$Eng_Class))  # classes of the training set after subsampling




#### B. Building Classification Models ####
#### B.1. Random Forest ####
#### B.1.1. Hyperparameter tuning #####
library(rpart)
fitControl <- trainControl(method = "cv",   # set up cross validation
                           ## 5-fold cross validation
                           number = 5,
                           ## Estimate class probabilities
                           classProbs = T,
                           ## Evaluate performance using the following function which
                           ## computes sensitivity, specificity & area under the ROC
                           summaryFunction = twoClassSummary
                           )

# setting different values of mtry for the model 
man_grid <-  expand.grid(mtry = c(1:28))

# doing the grid search                                                              
set.seed(696)
# grid search                             NB. This takes some time to run           
forest_model1 <- train(Eng_Class ~ . , 
                       data = training, 
                       method = "rf", 
                       ## Specify which metric to optimize, by default, this is the accuracy
                       metric = "ROC",    
                       trControl = fitControl,
                       verbose = FALSE,
                       tuneGrid = man_grid)
forest_model1
plot(forest_model1, xlab = 'mtry')
forest_model1$bestTune 

# build the model
hyperparams <- expand.grid(mtry=6)
fitControl <- trainControl(method = "cv",   # set up cross validation
                           ## 5-fold cross validation
                           number = 5,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using the following function which
                           ## computes sensitivity, specificity & area under the ROC
                           summaryFunction = twoClassSummary
                           )
set.seed(696)
forest_model2 <- train(Eng_Class ~ . , 
                       data = training, 
                       method = 'rf', 
                       tuneGrid = hyperparams,
                       metric = "ROC",   
                       trControl = fitControl,
                       verbose = F)
forest_model2
forest_var <- varImp(forest_model2)
forest_var
plot(forest_var)
predictors(forest_model2)

#### B.1.2. Choosing cut-off #####
# choosing the optimal threshold on validation set
# extracting class posterior probabilities
prob_val <- predict(forest_model2, newdata = validation, type = 'prob')
p1 <- hist(prob_val[[1]], 
     main = 'predicted probablities for class okay', 
     xlab = 'Probability')
p2 <- hist(prob_val[[2]], 
     main = 'predicted probablities for class under', 
     xlab = 'Probability')
plot(p1, col=rgb(0,0,1,1/4), 
     xlim = c(0,1), 
     ylim = c(0,300), 
     main = 'Histogram of class probablities',
     xlab = 'Probability')
plot( p2, col=rgb(1,0,0,1/4), add=T)
legend('top', legend=c("okay", "under"),
       fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)),
       cex=0.8)

# creating a vector of probability from tree prediction on the validation set
forest_val_prob <- prob_val[[2]]
# choosing the best therold for the classifer
library(ROCR)
threshold_pred_rf <- prediction(forest_val_prob, validation$Eng_Class)
acc <- performance(threshold_pred_rf, 'acc')
spec <- performance(threshold_pred_rf, 'spec')
sens <- performance(threshold_pred_rf, 'sens')
# plotting thresholds
plot(acc, main = 'Cut-off Level vs. Accuracy, Specificity & Sensitivity (Random Forest)', 
     xlab = 'Cut-off level', 
     ylab = 'Percentage',
     ylim = c(0,1),
     col = 'grey')
lines(spec@x.values[[1]], spec@y.values[[1]], col = 'red')
lines(sens@x.values[[1]], sens@y.values[[1]], col = 'blue')
chosen_thresold = 0.35
abline(v=chosen_thresold, lty = 'dashed')
# ROC curve on the validation set
roc_rf <- performance(threshold_pred_rf, "tpr", "fpr")
plot(roc_rf, col='blue')


#### B.1.3. Evaluating model #####
# extracting class posterior probabilities
prob_test <- predict(forest_model2, newdata = testing, type = 'prob')
# creating a vector of probability from tree prediction on the test set
forest_test_prob <- prob_test[[2]]
forest_test_pred <- as.factor(ifelse(forest_test_prob <= chosen_thresold, "okay", "under"))     
# confussion matrix
confusionMatrix(forest_test_pred, testing$Eng_Class, positive = 'okay')
# calculating AUC
t1 <- prediction(forest_test_prob, testing$Eng_Class)
rf_auc <- performance(t1, 'auc')
rf_auc@y.values[[1]]

#### B.2. GBM ####
#### B.2.1. Hyperparameter tuning #####
library(gbm)
# setting conditions for trainControl
fitControl <- trainControl(method = "repeatedcv",   # set up cross validation
                           ## 3-fold cross validation
                           number = 5,
                           ## repeat cross validation 1 time
                           repeats = 1,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using the following function which
                           ## computes sensitivity, specificity & area under the ROC
                           summaryFunction = twoClassSummary)
# setting different values for gbm's parameters
man_grid <-  expand.grid(n.trees = c(10,20,50,100,200),
                         shrinkage = c(0.02,0.05,0.1),
                         interaction.depth = c(1,2,3,5,8,10),
                         n.minobsinnode = c(1,2,3,5,8,10))

# doing the grid search                  NB. THIS TAKE AROUND 15 MINUTES - SKIP THIS AND GO TO SECTION
set.seed(696)
gbm_model1 <- train(Eng_Class ~ . ,
                    data = training,
                    method = "gbm",
                    ## Specify which metric to optimize, by default, this is the accuracy
                    metric = "ROC",
                    trControl = fitControl,
                    verbose = FALSE,
                    tuneGrid = man_grid)
plot(gbm_model1)
gbm_model1$bestTune



# build the model by manually selecting the parameters after the grid searches
hyperparams <- expand.grid(n.trees = 200,
                           interaction.depth = 10, 
                           shrinkage = 0.1,
                           n.minobsinnode = 8)
fitControl <- trainControl(method = "repeatedcv",   # set up cross validation
                           ## 5-fold cross validation
                           number = 5,
                           ## repeat cross validation 5 times
                           repeats = 5,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using the following function which
                           ## computes sensitivity, specificity & area under the ROC
                           summaryFunction = twoClassSummary,
                           savePredictions = "all")
set.seed(696)
gbm_model8 <- train(Eng_Class ~ . ,
                    data = training, 
                    method = "gbm", 
                    tuneGrid = hyperparams,
                    metric = "ROC",
                    trControl = fitControl,
                    verbose = F)

gbm_model8 
gbm_var <- varImp(gbm_model8)
plot(gbm_var)


#### B.2.2. Choosing cut-off #####
# choosing the optimal threshold on validation set
# extracting class posterior probabilities
prob_val <- predict(gbm_model8, newdata = validation, type = 'prob')
p1 <- hist(prob_val[[1]], 
           main = 'predicted probablities for class okay', 
           xlab = 'Probability')
p2 <- hist(prob_val[[2]], 
           main = 'predicted probablities for class under', 
           xlab = 'Probability')
plot(p1, col=rgb(0,0,1,1/4), 
     xlim = c(0,1), 
     ylim = c(0,500), 
     main = 'Histogram of class probablities',
     xlab = 'Probability')
plot( p2, col=rgb(1,0,0,1/4), add=T)
legend('top', legend=c("okay", "under"),
       fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)),
       cex=0.8)

# creating a vector of probability from tree prediction on the validation set
gbm_val_prob <- prob_val[[2]]
# choosing the best therold for the classifer
library(ROCR)
threshold_pred_gbm <- prediction(gbm_val_prob, validation$Eng_Class)
acc <- performance(threshold_pred_gbm, 'acc')
spec <- performance(threshold_pred_gbm, 'spec')
sens <- performance(threshold_pred_gbm, 'sens')
# plotting thresholds
plot(acc, main = 'Cut-off Level vs. Accuracy, Specificity & Sensitivity (GBM)', 
     xlab = 'Cut-off level', 
     ylab = 'Accuracy',
     ylim = c(0,1),
     col = 'grey')
lines(spec@x.values[[1]], spec@y.values[[1]], col = 'red')
lines(sens@x.values[[1]], sens@y.values[[1]], col = 'blue')
chosen_thresold = 0.145
abline(v=chosen_thresold, lty = 'dashed')
# ROC curve on the validation set
roc_gbm <- performance(threshold_pred_gbm, "tpr", "fpr")
plot(roc_gbm, col='blue')


#### B.2.3. Evaluating model ##### 
# extracting class posterior probabilities
prob_test <- predict(gbm_model8 , newdata = testing, type = 'prob')
# creating a vector of probability from tree prediction on the test set
forest_test_prob <- prob_test[[2]]
forest_test_pred <- as.factor(ifelse(forest_test_prob <= chosen_thresold, "okay", "under"))     
# confussion matrix
confusionMatrix(forest_test_pred, testing$Eng_Class, positive = 'okay')
# calculating AUC
t2 <- prediction(forest_test_prob, testing$Eng_Class)
gbm_auc <- performance(t2, 'auc')
gbm_auc@y.values[[1]]




#### B.3. Best Model ####
# ROC
plot(roc_rf, col='blue', main = 'ROC curve')
plot(roc_gbm, col ='red', add=T)
legend(0.4,0.3, 
       legend=c("gbm", "random forest"),
       col= c('red', 'blue'),
       lty=1,
       box.lty=0,
       cex=0.8)



#### C. Feature Selection ####
#### C.1 Recursive Elimination ####
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl <- rfeControl(method = "repeatedcv", 
                   repeats = 5,
                   saveDetails = TRUE,
                   functions = caretFuncs,
                   returnResamp = "final")
ctrl$functions$summary <- fiveStats
cvCtrl <- trainControl(method = "cv",
                       verboseIter = FALSE,
                       classProbs = TRUE,
                       allowParallel = TRUE)
set.seed(696)
gbmRFE <- rfe(Eng_Class ~., data = training,
              sizes = 1:28,
              rfeControl= ctrl,
              metric = "ROC",
              method = "gbm",
              tuneLength = 10,
              trControl = cvCtrl)
gbmRFE


#### C.2. Boruta ####
dim(training)
set.seed(696)
library(Boruta)
boruta <- Boruta(Eng_Class ~., data = training, doTrace = 2, maxRuns = 500)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.8, 
     main = 'Boruta Feature Selection')
plotImpHistory(boruta,
               main = 'Boruta Feature Selection over Iterations')



#### E. Different scenarios for label variable  ####

#### F. Predictions for scoring set ####
# gbm
dim(df2_trans)
gbm_threshold <- 0.145
gbm_prob <- predict(gbm_model8 , newdata = df2_trans, type = 'prob')[[2]]
hist(gbm_prob, xlab = 'predicted probability', main = 'Predicted Probablitiy - GBM')
abline(v=gbm_threshold, col = 'red')
legend('topright', 
       legend=c("cut-off = 0.145"),
       col= c('red'),
       lty=1,
       box.lty=0,
       cex=0.8)
gbm_pred <- as.factor(ifelse(gbm_prob <= gbm_threshold, "okay", "under"))  
bp8 <- barplot(table(gbm_pred), 
               ylim = c(0,900),
               main = 'Predicted Classes - GBM')
text(bp8, 
     table(gbm_pred) + 50, 
     labels = table(gbm_pred), 
     cex = 1.1)


# random forest
rf_threshold <- 0.35
rf_prob <- predict(forest_model2, newdata = df2_trans, type = 'prob')[[2]]
hist(rf_prob, xlab = 'predicted probability', main = 'Predicted Probablitiy - Random Forest')
abline(v=rf_threshold, col = 'red')
legend('topright', 
       legend=c("cut-off = 0.35"),
       col= c('red'),
       lty=1,
       box.lty=0,
       cex=0.8)
rf_pred <- as.factor(ifelse(rf_prob <= rf_threshold, "okay", "under"))  
bp9 <- barplot(table(rf_pred), 
               ylim = c(0,900),
               main = 'Predicted Classes - Random Forest')
text(bp9, 
     table(rf_pred) + 50, 
     labels = table(rf_pred), 
     cex = 1.1)







