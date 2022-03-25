## Stat5650 Group Project
## Group Members:
# - Nate Nellis
# - Brian Nalley
# - Will Gullion
# - McKade Thomas
# Will this work?
# What about this?

## Libraries Needed

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
  HighBP = as.factor(HighBP),
  HighChol = as.factor(HighChol),
  CholCheck = as.factor(CholCheck),
  ln.BMI = log(BMI),
  Smoker = as.factor(Smoker),
  Stroke = as.factor(Stroke),
  Diabetes = as.factor(Diabetes),
  PhysActivity = as.factor(PhysActivity),
  Fruits = as.factor(Fruits),
  Veggies = as.factor(Veggies),
  HvyAlcoholConsump = as.factor(HvyAlcoholConsump),
  AnyHealthcare = as.factor(AnyHealthcare),
  NoDocbcCost = as.factor(NoDocbcCost),
  GenHlth = as.factor(GenHlth),
  ln.MentHlth = log(MentHlth + 1), # Probably shouldn't use
  bin.MentHlth = as.factor(ifelse(MentHlth == 0, 0, 1)),
  fac.MentHlth = as.factor(MentHlth), # Probably shouldn't use
  ln.PhysHlth = log(PhysHlth + 1), # Probably shouldn't use
  bin.PhysHlth = as.factor(ifelse(PhysHlth == 0, 0, 1)),
  fac.PhysHlth = as.factor(PhysHlth), # Probably shouldn't use
  DiffWalk = as.factor(DiffWalk),
  Sex = as.factor(ifelse(Sex == 0, "F", "M")),
  Age = as.factor(Age),
  Education = as.factor(Education),
  Income = as.factor(Income)
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
