
library(rsample)
library(tidyverse)
library(gtsummary)
library(forcats)
library(ipred)

iowa <- read_csv("data/3-Year_Recidivism_for_Offenders_Released_from_Prison_in_Iowa_elaborated.csv") %>% 
  janitor::clean_names()

glimpse(iowa)


# Sjekk fordeling på data. To-veis tabell er fint. 
iowa %>% 
  tbl_summary(by = recidivism_return_to_prison_numeric)


# Omkoding
iowa2 <- iowa %>% 
  mutate(race_ethnicity = case_when(str_detect(race_ethnicity, "White") ~ "White", 
                              str_detect(race_ethnicity, "Black") ~ "Black", 
                              TRUE ~ "Other minority" ) ) %>% 
  mutate(convicting_offense_classification = fct_lump(convicting_offense_classification, n = 6), 
         convicting_offense_subtype = fct_lump(convicting_offense_subtype, n = 12), 
         main_supervising_district = replace_na(main_supervising_district, "Other"), 
         main_supervising_district = fct_lump_min(main_supervising_district, min = 500), 
         release_type = ifelse( str_detect(release_type, "Parole"), "Parole", release_type), 
         release_type = replace_na(release_type, "Other"))



iowa2 %>% 
  tbl_summary(by = recidivism_return_to_prison_numeric)





set.seed(42)
baggedtree <- bagging(recidivism_return_to_prison_numeric ~ . , data = iowa2, 
                      nbagg = 100, coob = TRUE)

baggedtree









# Attrition data


# Leser inn data
attrition <- readRDS("data/Attrition.rds") %>% 
  select(-EmployeeNumber)

glimpse(attrition)
summary(attrition)


## Splitte datasettet ####
set.seed(426)
attrition_split <- initial_split(attrition)
training <- training(attrition_split) 
testing  <- testing(attrition_split) 


# Klassifikasjonstre ####

library(rpart)
library(rpart.plot)


glimpse(training)

attr_tre <- rpart( Attrition ~ . , data = training, cp = 0.00001, maxdepth = 5, 
                    minbucket = 15, method = "class")

# for å endre cut-off i splitt, legg til: parms = list(prior = c(.65,.35)) 
rpart.plot(attr_tre)

rpart.rules(attr_tre)

testing_tre <- testing %>% 
  mutate(attrklass = predict(attr_tre, newdata = testing, type = "class")) 

library(caret)
confusionMatrix(reference = testing_tre$Attrition, testing_tre$attrklass, positive = "Yes")





# Bagging ####
# NB! utfallsvariabelen må være factor-variabel for klassifisering
set.seed(42)
attr_bag <- bagging(Attrition ~ . , data = training, 
                    nbagg = 500, coob = TRUE)

attr_bag


testing_bag <- testing %>% 
  mutate(attrklass = predict(attr_bag, newdata = testing, type = "class")) 

library(caret)
confusionMatrix(reference = testing_bag$Attrition, testing_bag$attrklass, positive = "Yes")


table(training$Attrition)

# Random Forest ####
library(randomForest)
set.seed(426)
attr_rf <- randomForest(Attrition ~ . , data = training, 
                        mtry = 4, ntree = 700,  
                        sampsize = c(150, 150))

plot(attr_rf)
attr_rf

testing_rf <- testing %>% 
  mutate(attrklass = predict(attr_bag, newdata = testing, type = "class")) 

confusionMatrix(reference = testing_rf$Attrition, testing_rf$attrklass, positive = "Yes")





# Compas-data 


compas <- readRDS("data/compas.rds")

glimpse(compas)

# Klassifikasjonstre
klass_tre <- rpart( Two_yr_Recidivism ~ . , data = compas, cp = 0.00001, maxdepth = 5, 
                    minbucket = 15, method = "class")

# for å endre cut-off i splitt, legg til: parms = list(prior = c(.65,.35)) 
rpart.plot(klass_tre)



# Bagging ####
bag <- bagging(Two_yr_Recidivism ~ . , data = compas, 
                    nbagg = 500, coob = TRUE)

bag


# Random forest ####
set.seed(4356)
rf <- randomForest(Two_yr_Recidivism ~ . , 
                   data = compas, ntree = 600,
                   sampsize = c(500, 500))
rf

plot(rf)

# Predikerer ####
compas_p <- compas %>% 
  mutate(pred_tre = predict(klass_tre, type = "class"),
         pred_bag = predict(bag), 
         pred_rf = predict(rf))


# Confusion matrix
confusionMatrix(reference = compas_p$Two_yr_Recidivism, compas_p$pred_tre, positive = "1")

confusionMatrix(reference = compas_p$Two_yr_Recidivism, compas_p$pred_bag, positive = "1")

confusionMatrix(reference = compas_p$Two_yr_Recidivism, compas_p$pred_rf, positive = "1")





