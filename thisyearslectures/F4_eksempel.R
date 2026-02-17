

library(tidyverse)
library(rsample)
library(ipred)
library(randomForest)
library(caret)

# Compas-data 

compas <- readRDS("data/compas.rds")

glimpse(compas)


# Bagging ####
set.seed(4356)
bag <- bagging(Two_yr_Recidivism ~ . , data = compas, 
               nbagg = 500, coob = TRUE)

bag

# Legg til parametre for de enkelte trær (ikke sikkert det hjelper)
bag <- bagging(Two_yr_Recidivism ~ . , data = compas, 
               nbagg = 500, coob = TRUE, 
               control = rpart.control(cp = 0.00001, minbucket = 5))

bag

compas_p <- compas %>% 
  mutate(pred = predict(bag, newdata = compas, type = "class"))

confusionMatrix(reference = compas_p$Two_yr_Recidivism, 
                compas_p$pred, positive = "1")





# Random forest ####
set.seed(4356)
rf <- randomForest(Two_yr_Recidivism ~ . , 
                   data = compas, ntree = 100)

set.seed(4356)
rf <- randomForest(Two_yr_Recidivism ~ . , 
                   data = compas, ntree = 600,
                   sampsize = c(500, 500))
# plot(rf)
# rf
# 
# plot(rf)

# Predikerer ####
compas_p <- compas %>% 
  mutate(pred_rf = predict(rf, newdata = compas))


# Confusion matrix

confusionMatrix(reference = compas_p$Two_yr_Recidivism, 
                compas_p$pred_rf, positive = "1")




iowa <- readRDS("data/iowa.rds") %>% 
  mutate(recidivism = factor(recidivism_return_to_prison_numeric)) %>% 
  select(-recidivism_return_to_prison_numeric) %>% 
  drop_na()
glimpse(iowa)

summary(iowa)

iowa_rf <- randomForest(recidivism ~ ., data = iowa)




