
# Laster inn pakker vi trenger
library(tidyverse)
library(randomForest)
library(caret)
library(rsample)
library(fairness)


# Lese inn data ####
# Omkoder og sletter variable som ikke skal brukes
# se info om datasettet her: https://torbskar.github.io/SOS2901_oppgavehefte/datasets.html#diabetes-rehospitalization
# og se artikkel der de er omtalt nøyere. 

# Omkodingen her er litt praktisk forenkling. Andre ting er også mulig. 
diabetic <- read.csv("data/diabetic_data.csv", 
                     stringsAsFactors = TRUE) %>% 
  mutate(readmitted = factor( if_else(readmitted == "<30", "Yes", "No") )) %>% 
  mutate(diag_3 = as.numeric(diag_3), 
         diag_2 = as.numeric(diag_2),
         diag_1 = as.numeric(diag_1), 
         race = case_when(race == "?" ~ "Other",
                          TRUE ~ race)) %>% 
  select(-encounter_id, -patient_nbr, -medical_specialty) # ID-variable må ikke med i algoritmen


table(diabetic$race)

glimpse(diabetic)

# Splitter data i trenings- og testsett
set.seed(123)
split <- initial_split(diabetic, prop = 0.7)
train <- training(split)
test <- testing(split)

table(train$readmitted)


# Random Forest ####
# Sampsize for å balansere utfallet
rf <- randomForest(readmitted ~ ., 
                   sampsize = c(500, 500), 
                   ntrees = 100,
                   importance = TRUE, 
                   data = train, 
                   type = "classification")


plot(rf)
rf


# prediksjon og confusion matrix 
train_p <- train %>% 
  mutate(pred = predict(rf))

confusionMatrix(train_p$pred, reference = train_p$readmitted)

# samme med test-data
test_p <- test %>% 
  mutate(pred = predict(rf, newdata = test))

confusionMatrix(test_p$pred, reference = test_p$readmitted)


# variable importance
varImpPlot(rf, type = 1)

# partial dependence
partialPlot(rf, pred.data = train, x.var = "A1Cresult", 
            which.class = "Yes")

table(train$A1Cresult)

partialPlot(rf, pred.data = test, x.var = "diag_1", 
            which.class = "Yes")


partialPlot(rf, pred.data = train, x.var = "age", 
            which.class = "Yes")



# Fairness ####

table(train$race)
library(fairness)
acc <- acc_parity(data = test_p, 
                    outcome = "readmitted", 
                  group = "race", 
                  preds = "pred", 
                  base = "Caucasian")


table(train$age)
acc <- acc_parity(data = test_p, 
                  outcome = "readmitted", 
                  group = "age", 
                  preds = "pred", 
                  base = "[70-80)")
acc



# Stratifisering ####
strat <- train %>% 
  mutate(strat = case_when(age %in% c("[0-10)", "[10-20)", "[20-30)") ~ "[0-30)",
                           age %in% c("[90-100)", "[80-90)") ~ "[80-100)",
                           TRUE ~ age)) %>%
  pull(strat)


strat <- train %>% 
  mutate(strat = case_when(age %in% c("[0-10)", "[10-20)", "[20-30)") ~ "[0-30)",
                           age %in% c("[90-100)", "[80-90)") ~ "[80-100)",
                           TRUE ~ age)) %>%
  mutate(strat = paste0(strat, readmitted)) %>% 
  pull(strat)

table(strat)

rf2 <- randomForest(readmitted ~ ., 
                   strata = strat,
                   #sampsize = c(700, 500, 500, 500, 500, 500, 500),
                   #sampsize = c(700, 1200, 500, 300, 500, 500, 500),
                   sampsize = c(100, 100, 200, 200, 200, 200, 200,
                                200, 200, 200, 200, 200, 200, 200),
                   ntree = 500,
                   #importance = TRUE, 
                   data = train, 
                   type = "classification")

plot(rf2)

test_p2 <- test %>% 
  mutate(pred = predict(rf2, newdata = test))


acc <- acc_parity(data = test_p2, 
                  outcome = "readmitted", 
                  group = "race", 
                  preds = "pred", 
                  base = "Caucasian")
acc

table(train$age)
acc <- acc_parity(data = test_p2, 
                  outcome = "readmitted", 
                  group = "age", 
                  preds = "pred", 
                  base = "[70-80)")
acc


