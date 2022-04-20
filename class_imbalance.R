## Stat5650 Group Project
## Group Members:
# - Nate Nellis
# - Brian Nalley
# - Will Gullion
# - McKade Thomas


## Libraries Needed
### NOTE: You'll need to install this straight from Github
## Use this command to do it (hit 1 when it asks which libraries to update): 
# remotes::install_github("cran/DMwR")
library(DMwR)


## Read in the data
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

# new data with transformed variables
health_subset <- health[c(1:4, 6:15, 18:23, 25, 28)]
summary(health$HeartDiseaseorAttack)[1] / summary(health$HeartDiseaseorAttack)[2]
## Class Imbalance is about 10 to 1 (way fewer positive class)

## SMOTE for Class Imbalance
# Docs: https://www.rdocumentation.org/packages/DMwR/versions/0.4.1/topics/SMOTE
health_balanced <- SMOTE(HeartDiseaseorAttack ~ ., data = health_subset, perc.over = 50, perc.under = 0)
summary(health_balanced$HeartDiseaseorAttack)
