## Stat5650 Group Project
## Group Members:
# - Nate Nellis
# - Brian Nalley
# - Will Gullion
# - McKade Thomas
# Will this work?
# What about this?

## Libraries Needed
library(dplyr)
library(MASS)
library(verification)
library(caTools)
library(klaR)
library(randomForest)
library(glmnet)

## Read in the data
# https://www.kaggle.com/datasets/alexteboul/heart-disease-health-indicators-dataset

# Description of variables found in https://www.cdc.gov/brfss/annual_data/2015/pdf/codebook15_llcp.pdf
#health <- read.csv("C:/Users/n8nel/Desktop/STAT_5650/Project/Stat5650_Project/heart_disease_health_indicators.csv")

## Response
# - HeartDiseaseorAttack, binary

## Predictor Variables
## 14 factors, 6 numeric, 1 categorical
# HighBP
  # Adults who have been told they have high blood pressure by a doctor, nurse, or other health professional 
# HighChol
  # Have you EVER been told by a doctor, nurse or other health professional that your blood cholesterol is high? --> TOLDHI2
# CholCheck
  # Cholesterol check within past five years 
# BMI
# Smoker
 # Have you smoked at least 100 cigarettes in your entire life?
# Stroke
# Diabetes
    # 0 is no diabetes,
    # 1 is pre-diabetes
    # 2 is diabetes
# PhysActivity
  # During the past month, other than your regular job, did you participate in 
  # any physical activities or exercises such as running, calisthenics, golf, 
  # gardening, or walking for exercise?
# Fruits
  # Consume Fruit 1 or more times per day
# Veggies
  # Consume Vegetables 1 or more times per day
# HvyAlcoholConsum
  # Heavy drinkers (adult men having more than 14 drinks per week and adult women having more than 7 drinks per week)
# AnyHealthcare
  # Do you have any kind of health care coverage, including health insurance, 
  # prepaid plans such as HMOs, or government plans such as Medicare, or Indian Health Service?
# NoDocbcCost
  # Was there a time in the past 12 months when you needed to see a doctor but could not because of cost?
# GenHlth
  # Would you say that in general your health is:
    # 1 = Excellent
    # 2 = Very Good
    # 3 = Good
    # 4 = Fair
    # 5 = Poor
# MentHlth
  # Now thinking about your mental health, which includes stress, depression, and 
  # problems with emotions, for how many days during the past 30 days was your mental health not good?
# PhysHlth
  # Now thinking about your physical health, which includes physical illness and 
  # injury, for how many days during the past 30 days was your physical health not good? 
# DiffWalk
  # Do you have serious difficulty walking or climbing stairs?
# Sex
    # 0 = Female
    # 1 = Male
# Age
    # 1 Age 18 to 24
    # 2 Age 25 to 29
    # 3 Age 30 to 34
    # 4 Age 35 to 39
    # 5 Age 40 to 44
    # 6 Age 45 to 49
    # 7 Age 50 to 54
    # 8 Age 55 to 59
    # 9 Age 60 to 64
    # 10 Age 65 to 69
    # 11 Age 70 to 74
    # 12 Age 75 to 79
    # 13 Age 80 or older
    # 14 Don't Know / Refused to answer (I removed these as well)
# Education, categorical
  # What is the highest grade or year of school you completed?
    # 1 Never attended school or only kindergarten 
    # 2 Grades 1 through 8 (Elementary)
    # 3 Grades 9 through 11 (Some high school)
    # 4 Grade 12 or GED (High school graduate)
    # 5 College 1 year to 3 years (Some college or technical school)
    # 6 College 4 years or more (College graduate)
# Income
  # Is your annual household income from all sources
    # 1 Less than $10,000
    # 2 Less than $15,000 ($10,000 to less than $15,000)
    # 3 Less than $20,000 ($15,000 to less than $20,000)
    # 4 Less than $25,000 ($20,000 to less than $25,000)
    # 5 Less than $35,000 ($25,000 to less than $35,000)
    # 6 Less than $50,000 ($35,000 to less than $50,000)
    # 7 Less than $75,000 ($50,000 to less than $75,000)
    # 8 $75,000 or more

health <- read.csv("heart_disease_health_indicators.csv")

health <- health %>% mutate(
  HeartDiseaseorAttack = as.factor(HeartDiseaseorAttack),
  HighBP = as.factor(HighBP),  # LASSO approved
  HighChol = as.factor(HighChol),  # LASSO approved
  CholCheck = as.factor(CholCheck),
  ln.BMI = log(BMI),  # RF approved
  Smoker = as.factor(Smoker),  # LASSO approved
  Stroke = as.factor(Stroke),  # LASSO 1-SE only, RF approved
  Diabetes = as.factor(Diabetes),  # LASSO approved, RF approved
  PhysActivity = as.factor(PhysActivity),
  Fruits = as.factor(Fruits),
  Veggies = as.factor(Veggies),
  HvyAlcoholConsump = as.factor(HvyAlcoholConsump),
  AnyHealthcare = as.factor(AnyHealthcare),
  NoDocbcCost = as.factor(NoDocbcCost),  # LASSO approved
  GenHlth = as.factor(GenHlth),  # LASSO approved, RF approved
  ln.MentHlth = log(MentHlth + 1), # Probably shouldn't use
  bin.MentHlth = as.factor(ifelse(MentHlth == 0, 0, 1)),  #RF approved
  fac.MentHlth = as.factor(MentHlth), # Probably shouldn't use
  ln.PhysHlth = log(PhysHlth + 1), # Probably shouldn't use
  bin.PhysHlth = as.factor(ifelse(PhysHlth == 0, 0, 1)),  # LASSO approved, RF approved
  fac.PhysHlth = as.factor(PhysHlth), # Probably shouldn't use
  DiffWalk = as.factor(DiffWalk),  # LASSO , RF approved
  Sex = as.factor(ifelse(Sex == 0, "F", "M")),  # LASSO approved, RF approved
  Age = as.factor(Age),  # LASSO approved, RF approved
  Education = as.factor(Education),  # RF approved
  Income = as.factor(Income)  # LASSO approved, RF approved
)

# BMI
par(mfrow = c(1, 3))
hist(health$BMI)
boxplot(health$BMI)
qqnorm(health$BMI)

# ln.BMI
par(mfrow = c(1, 3))
hist(health$ln.BMI)
boxplot(health$ln.BMI)
qqnorm(health$ln.BMI)

# MentHlth
par(mfrow = c(1, 3))
hist(health$MentHlth)
boxplot(health$MentHlth)
qqnorm(health$MentHlth)

# ln.MentHlth
par(mfrow = c(1, 3))
hist(health$ln.MentHlth)
boxplot(health$ln.MentHlth)
qqnorm(health$ln.MentHlth)
    # It helped a little bit, but didn't do a ton.  I wonder if we should make 
    # this a binary variable where we say that it is 0 if the respondent marked 
    # zero (175,680 observations) and one otherwise.  I made this as bin.MentHlth

# PhysHlth
par(mfrow = c(1, 3))
hist(health$PhysHlth)
boxplot(health$PhysHlth)
qqnorm(health$PhysHlth)

# ln.PhysHlth
par(mfrow = c(1, 3))
hist(health$ln.PhysHlth)
boxplot(health$ln.PhysHlth)
qqnorm(health$ln.PhysHlth)
    # It helped a little bit, but didn't do a ton.  I wonder if we should make 
    # this a binary variable where we say that it is 0 if the respondent marked 
    # zero (160,052 observations) and one otherwise.  I made this as bin.PhysHlth

par(mfrow = c(1, 1))
summary(health$HeartDiseaseorAttack) # about 10% of respondents said yes.
summary(health$HighBP) # About 2/5 of respondents said yes.
summary(health$HighChol) # About 2/5 of respondents said yes.
summary(health$CholCheck) # Only 4% respondents didn't have, so this variable is possibly useless.  Analysis will tell.
summary(health$BMI) # Median is 27.
summary(health$ln.BMI)
summary(health$Smoker) # About 2/5 of respondents said yes.
summary(health$Stroke) # Only 4% respondents didn't have, so this variable is possibly useless.  Analysis will tell.
summary(health$Diabetes) # 13% are diabetic with 1.8% pre-diabetic
summary(health$PhysActivity) # 75% of respondents said yes.  Since this is any time 
                              #during the month and includes gardening, I don't know how helpful this variable is going to be.
summary(health$Fruits) # 63% of respondents said yes.
summary(health$Veggies) # 81% of respondents said yes.
summary(health$HvyAlcoholConsump) # Only 6% of respondents said yes.
summary(health$AnyHealthcare) # Only 5% of respondents said no.
summary(health$NoDocbcCost) # 8% of respondents said yes.
summary(health$GenHlth) # Most people think they are good and above.  Only 17% said fair or poor.
summary(health$bin.MentHlth) # 30% have non-zero days
summary(health$bin.PhysHlth) # 37% have non-zero days
summary(health$DiffWalk) # 17% responded yes.
summary(health$Sex) # 141,974 Females (56%), 111,706 Males (44%)
summary(health$Age) 
hist(as.numeric(health$Age)) # Mode is around 9, which is 60-64.
summary(health$Education) # 42% have at least graduated college.
hist(as.numeric(health$Education))
summary(health$Income) # They set the catagories super low and a third put response 8.
hist(as.numeric(health$Income))


## Class.Sum & Kappa ##
kappa=function(x){
  n=sum(x)
  pobs=(x[1,1]+x[2,2])/n
  pexp=(sum(x[1,])*sum(x[,1])+sum(x[2,])*sum(x[,2]))/n^2
  kappa=(pobs-pexp)/(1-pexp)
  t1=0
  t2=0
  t3=0
  pii=x/n
  pidot=apply(pii,1,sum)
  pdotj=apply(pii,2,sum)
  for(i in 1:2){
    t1 = t1 + pii[i,i]*((1-pexp) - (1-pobs)*(pidot[i]+pdotj[i]))^2
  }
  t2 = pii[1,2]*(pdotj[1]+pidot[2])^2 + pii[2,1]*(pdotj[2] + pidot[1])^2
  t3 = (pobs*pexp-2*pexp+pobs)^2
  vhat = (t1 + t2*(1-pobs)^2 -t3)/(n*(1-pexp)^4)
  se=sqrt(vhat)
  return(c(kappa,se))
}


class.sum=function(truth,predicted){
  xt=table(truth,round(predicted+0.000001))
  pcc=round(100*sum(diag(xt))/sum(xt),2)
  spec=round(100*xt[1,1]/sum(xt[1,]),2)
  sens=round(100*xt[2,2]/sum(xt[2,]),2)
  kap=round(kappa(xt)[1],4)
  au=round(roc.area(truth,predicted)$A,4)
  return(cbind(c("Percent Correctly Classified = ","Specificity = ","Sensitivity = ","Kappa =","AUC= "),c(pcc,spec,sens,kap,au)))
}
# McKade, feel free to put your new and improved version here instead if you'd like

# new data with transformed variables
health2 <- health[c(1:4, 6:15, 18:23, 25, 28)]

# Train and Test Sets
health2_split <- sample.split(health2, SplitRatio = 0.3)
health_test <- subset(health2, health2_split == TRUE)
health_train <- subset(health2, health2_split == FALSE)


## PCA ##
# Can't really do because we have almost exclusively factor variables


## LDA ##
health_lda <- lda(HeartDiseaseorAttack ~ . , CV = TRUE, data = health_train)

# LOO CV Confusion Matrix
table(health_train$HeartDiseaseorAttack, health_lda$class)
# 90.07% PCC

# 10 fold CV
health_lda_xval <- rep(0, length = nrow(health_train))
x <- rep(1:10, length = nrow(health_train))
x <- sample(x)
for(i in 1:10){
  train <- health_train[x != i, ]
  test <- health_train[x == i, ]
  glub <- lda(HeartDiseaseorAttack ~ . , data = train)
  health_lda_xval[x == i] <- predict(glub, test)$class
}

table(health_train$HeartDiseaseorAttack, health_lda_xval)
# 90.11% PCC

# Test Set Prediction
table(health_test$HeartDiseaseorAttack, predict(health_lda, health_test)$class)
# Not sure why it doesn't like this

## QDA ##
health_qda <- qda(HeartDiseaseorAttack ~ . , CV = TRUE, data = health_train)

# LOO CV Confusion Matrix
table(health_train$HeartDiseaseorAttack, health_qda$class)
# 68.44% PCC

# 10 fold CV
health_qda_xval_class <- rep(0, nrow(health_train))
health_qda_xval_posterior <- rep(0, nrow(health_train))
xvs <- rep(1:10, length = nrow(health_train))
xvs <- sample(xvs)
for(i in 1:10){
  train <- health_train[xvs != i, ]
  test <- health_train[xvs == i, ]
  glub <- qda(HeartDiseaseorAttack ~ . , data = train)
  health_qda_xval_posterior[xvs == i] <- predict(glub, test)$posterior[, 2]
  health_qda_xval_class[xvs == i] <- predict(glub, test)$class
}

table(health_train$HeartDiseaseorAttack, health_qda_xval_class)
# 68.42% PCC

# Test Set Prediction
table(health_test$HeartDiseaseorAttack, predict(health_qda, health_test)$class)
# Doesn't like this either


## K-NN ##

# k = 10
health_knn10 <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = health_train,
                     kn = 10)

set.seed(96570) 
health_knn_xval_class <- rep(0, nrow(health_train))
health_knn_xval_posterior <- rep(0, nrow(health_train))
xvs <- rep(1:10, length = nrow(health_train))
xvs<- sample(xvs)
for(i in 1:10){
  train <- health_train[xvs != i, ]
  test <- health_train[xvs == i, ]
  glub <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = train, kn = 10)
  health_knn_xval_posterior[xvs == i] <- predict(glub, test)$posterior[, 2]
  health_knn_xval_class[xvs == i] <- predict(glub, test)$class
}

table(health_train$HeartDiseaseorAttack, health_knn_xval_class)
class.sum(health_train$HeartDiseaseorAttack, health_knn_xval_posterior)
# PCC = 

# Test Accuracy
table(health_test$HeartDiseaseorAttack, predict(health_knn10, health_test)$class)
class.sum(health_test$HeartDiseaseorAttack, predict(health_knn10,
                                                    health_test)$posterior[, 2])
# PCC = 

# K = 9
health_knn9 <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = health_train,
                     kn = 9)

set.seed(96570) 
health_knn_xval_class <- rep(0, nrow(health_train))
health_knn_xval_posterior <- rep(0, nrow(health_train))
xvs <- rep(1:10, length = nrow(health_train))
xvs<- sample(xvs)
for(i in 1:10){
  train <- health_train[xvs != i, ]
  test <- health_train[xvs == i, ]
  glub <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = train, kn = 9)
  health_knn_xval_posterior[xvs == i] <- predict(glub, test)$posterior[, 2]
  health_knn_xval_class[xvs == i] <- predict(glub, test)$class
}

table(health_train$HeartDiseaseorAttack, health_knn_xval_class)
class.sum(health_train$HeartDiseaseorAttack, health_knn_xval_posterior)
# PCC = 

# Test Accuracy
table(health_test$HeartDiseaseorAttack, predict(health_knn9, health_test)$class)
class.sum(health_test$HeartDiseaseorAttack, predict(health_knn9,
                                                    health_test)$posterior[, 2])
# PCC = 

# K = 8
health_knn8 <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = health_train,
                    kn = 8)

set.seed(96570) 
health_knn_xval_class <- rep(0, nrow(health_train))
health_knn_xval_posterior <- rep(0, nrow(health_train))
xvs <- rep(1:10, length = nrow(health_train))
xvs<- sample(xvs)
for(i in 1:10){
  train <- health_train[xvs != i, ]
  test <- health_train[xvs == i, ]
  glub <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = train, kn = 8)
  health_knn_xval_posterior[xvs == i] <- predict(glub, test)$posterior[, 2]
  health_knn_xval_class[xvs == i] <- predict(glub, test)$class
}

table(health_train$HeartDiseaseorAttack, health_knn_xval_class)
class.sum(health_train$HeartDiseaseorAttack, health_knn_xval_posterior)
# PCC = 

# Test Accuracy
table(health_test$HeartDiseaseorAttack, predict(health_knn8, health_test)$class)
class.sum(health_test$HeartDiseaseorAttack, predict(health_knn8,
                                                    health_test)$posterior[, 2])
# PCC = 

# K = 7
health_knn7 <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = health_train,
                    kn = 7)

set.seed(96570) 
health_knn_xval_class <- rep(0, nrow(health_train))
health_knn_xval_posterior <- rep(0, nrow(health_train))
xvs <- rep(1:10, length = nrow(health_train))
xvs<- sample(xvs)
for(i in 1:10){
  train <- health_train[xvs != i, ]
  test <- health_train[xvs == i, ]
  glub <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = train, kn = 7)
  health_knn_xval_posterior[xvs == i] <- predict(glub, test)$posterior[, 2]
  health_knn_xval_class[xvs == i] <- predict(glub, test)$class
}

table(health_train$HeartDiseaseorAttack, health_knn_xval_class)
class.sum(health_train$HeartDiseaseorAttack, health_knn_xval_posterior)
# PCC = 

# Test Accuracy
table(health_test$HeartDiseaseorAttack, predict(health_knn7, health_test)$class)
class.sum(health_test$HeartDiseaseorAttack, predict(health_knn7,
                                                    health_test)$posterior[, 2])
# PCC = 

# K = 6
health_knn6 <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = health_train,
                    kn = 6)

set.seed(96570) 
health_knn_xval_class <- rep(0, nrow(health_train))
health_knn_xval_posterior <- rep(0, nrow(health_train))
xvs <- rep(1:10, length = nrow(health_train))
xvs<- sample(xvs)
for(i in 1:10){
  train <- health_train[xvs != i, ]
  test <- health_train[xvs == i, ]
  glub <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = train, kn = 6)
  health_knn_xval_posterior[xvs == i] <- predict(glub, test)$posterior[, 2]
  health_knn_xval_class[xvs == i] <- predict(glub, test)$class
}

table(health_train$HeartDiseaseorAttack, health_knn_xval_class)
class.sum(health_train$HeartDiseaseorAttack, health_knn_xval_posterior)
# PCC = 

# Test Accuracy
table(health_test$HeartDiseaseorAttack, predict(health_knn6, health_test)$class)
class.sum(health_test$HeartDiseaseorAttack, predict(health_knn6,
                                                    health_test)$posterior[, 2])
# PCC = 

# K = 5
health_knn5 <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = health_train,
                    kn = 5)

set.seed(96570) 
health_knn_xval_class <- rep(0, nrow(health_train))
health_knn_xval_posterior <- rep(0, nrow(health_train))
xvs <- rep(1:10, length = nrow(health_train))
xvs<- sample(xvs)
for(i in 1:10){
  train <- health_train[xvs != i, ]
  test <- health_train[xvs == i, ]
  glub <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = train, kn = 5)
  health_knn_xval_posterior[xvs == i] <- predict(glub, test)$posterior[, 2]
  health_knn_xval_class[xvs == i] <- predict(glub, test)$class
}

table(health_train$HeartDiseaseorAttack, health_knn_xval_class)
class.sum(health_train$HeartDiseaseorAttack, health_knn_xval_posterior)
# PCC = 

# Test Accuracy
table(health_test$HeartDiseaseorAttack, predict(health_knn5, health_test)$class)
class.sum(health_test$HeartDiseaseorAttack, predict(health_knn5,
                                                    health_test)$posterior[, 2])
# PCC = 

# K = 4
health_knn4 <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = health_train,
                    kn = 4)

set.seed(96570) 
health_knn_xval_class <- rep(0, nrow(health_train))
health_knn_xval_posterior <- rep(0, nrow(health_train))
xvs <- rep(1:10, length = nrow(health_train))
xvs<- sample(xvs)
for(i in 1:10){
  train <- health_train[xvs != i, ]
  test <- health_train[xvs == i, ]
  glub <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = train, kn = 4)
  health_knn_xval_posterior[xvs == i] <- predict(glub, test)$posterior[, 2]
  health_knn_xval_class[xvs == i] <- predict(glub, test)$class
}

table(health_train$HeartDiseaseorAttack, health_knn_xval_class)
class.sum(health_train$HeartDiseaseorAttack, health_knn_xval_posterior)
# PCC = 

# Test Accuracy
table(health_test$HeartDiseaseorAttack, predict(health_knn4, health_test)$class)
class.sum(health_test$HeartDiseaseorAttack, predict(health_knn4,
                                                    health_test)$posterior[, 2])
# PCC = 

# K = 3
health_knn3 <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = health_train,
                    kn = 3)

set.seed(96570) 
health_knn_xval_class <- rep(0, nrow(health_train))
health_knn_xval_posterior <- rep(0, nrow(health_train))
xvs <- rep(1:10, length = nrow(health_train))
xvs<- sample(xvs)
for(i in 1:10){
  train <- health_train[xvs != i, ]
  test <- health_train[xvs == i, ]
  glub <- sknn(as.factor(HeartDiseaseorAttack) ~ . , data = train, kn = 3)
  health_knn_xval_posterior[xvs == i] <- predict(glub, test)$posterior[, 2]
  health_knn_xval_class[xvs == i] <- predict(glub, test)$class
}

table(health_train$HeartDiseaseorAttack, health_knn_xval_class)
class.sum(health_train$HeartDiseaseorAttack, health_knn_xval_posterior)
# PCC = 

# Test Accuracy
table(health_test$HeartDiseaseorAttack, predict(health_knn3, health_test)$class)
class.sum(health_test$HeartDiseaseorAttack, predict(health_knn3,
                                                    health_test)$posterior[, 2])
# PCC = 

## LASSO LR ##
set.seed(96570)

library(doParallel)
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)


health_train_x <- data.matrix(health_train[ , 2:22])
health_train_y <- as.matrix(health_train[ , 1])

health_lasso <- glmnet(health_train_x, health_train_y, family = "binomial",
                       alpha = 1)
health_cv_lasso <- cv.glmnet(health_train_x, health_train_y, family = "binomial",
                             alpha = 1)

# 1-SE
lambda_1se <- health_cv_lasso$lambda.1se
table(health_train$HeartDiseaseorAttack, predict(health_lasso, health_train_x,
                                                 s = lambda_1se, type = "class"))
class.sum(health_train$HeartDiseaseorAttack, predict(health_lasso, health_train_x,
                                                     s = lambda_1se, type = "response"))

coef(health_lasso, s = lambda_1se)
# PCC = 90.84
# Variables Kept = HighBP, HighChol, Smoker, Diabetes, NoDocbcCost, GenHlth,
# DiffWalk, Sex, Age, Income, bin.PhysHlth

# Min
lambda_min <- health_cv_lasso$lambda.min
table(health_train$HeartDiseaseorAttack, predict(health_lasso, health_train_x,
                                                 s = lambda_min, type = "class"))
class.sum(health_train$HeartDiseaseorAttack, predict(health_lasso, health_train_x,
                                                     s = lambda_min, type = "response"))
coef(health_lasso, s = lambda_1se)
# PCC = 90.82
# Variables Kept = HighBP, HighChol, Smoker, Stroke, Diabetes, NoDocbcCost, 
# GenHlth, DiffWalk, Sex, Age, Income, bin.PhysHlth

# Test Predictions
health_test_x <- data.matrix(health_test[ , 2:22])
health_test_y <- as.matrix(health_test[ , 1])

table(health_test$HeartDiseaseorAttack, predict(health_lasso, health_test_x,
                                                s = lambda_min, type = "class"))
class.sum(health_test$HeartDiseaseorAttack, predict(health_lasso, health_test_x,
                                                    s = lambda_min, type = "response"))
# PCC = 90.75

table(health_test$HeartDiseaseorAttack, predict(health_lasso, health_test_x,
                                                s = lambda_1se, type = "class"))
class.sum(health_test$HeartDiseaseorAttack, predict(health_lasso, health_test_x,
                                                    s = lambda_1se, type = "response"))
# PCC = 90.73


## Random Forest ##

health_rf <- randomForest(as.factor(HeartDiseaseorAttack) ~ . ,
                          data = health_train)

health_rf$confusion

class.sum(health_train$HeartDiseaseorAttack, predict(health_rf, type = "prob")[,2])
# PCC = 90.76

table(health_test$HeartDiseaseorAttack, predict(health_rf, health_test,
                                                type = "response"))
class.sum(health_test$HeartDiseaseorAttack, predict(health_rf, health_test,
                                                    type = "prob")[, 2])
# PCC = 90.63

#Variable Importance Plots
health_rf <- randomForest(as.factor(HeartDiseaseorAttack) ~ . ,
                          importance = TRUE, data = health_train)

varImpPlot(health_rf, scale = FALSE)
# Age really important, then GenHlth, big gap before Stroke and Sex then group at
# ln.BMI, DiffWalk, Diabetes, bin.MentlHlth, Income, bin.PhysHlth and Education
# Could probably get rid of everything else.

stopCluster(cl)