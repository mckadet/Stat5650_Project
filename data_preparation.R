## Stat5650 Group Project
## Group Members:
# - Nate Nellis
# - Brian Nalley
# - Will Gullion
# - McKade Thomas

## Libraries Needed
library(caTools)
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


best_features <- c(1:4, 6:15, 18:23, 25, 28)
health_subset <- health[,best_features]


# Train and Test Sets
health2_split <- sample.split(health_subset, SplitRatio = 0.3)
health_test <- subset(health_subset, health2_split == TRUE)
health_train <- subset(health_subset, health2_split == FALSE)

# write.csv(health_test, 'health_test.csv', row.names = FALSE)
# write.csv(health_train, 'health_train.csv', row.names = FALSE)
