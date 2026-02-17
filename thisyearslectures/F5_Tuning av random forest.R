library(tidyverse)
library(rsample)
library(caret)
library(randomForest)



# Synthetic data #### 
load("data/synthetic_birthCohort96.RData")

glimpse(synthetic_birthCohort)

synth <- synthetic_birthCohort %>% 
  select(-drugs, -violence) %>% 
  mutate(felonies = factor(felonies)) %>% 
  mutate(across(everything(), ~replace_na(.x, 0)))
  


# splitter datasettet
set.seed(426)
synth_split <- initial_split(synth)
training <- training(synth_split) 
testing  <- testing(synth_split) 

table(training$felonies)


rf1 <- randomForest(felonies ~ ., data = training)

test1 <- testing %>% 
  mutate(pred = predict(rf1, newdata = ., type = "response"))

confusionMatrix(reference=test1$felonies, test1$pred, positive = "1")


rf2 <- randomForest(felonies ~ ., 
                    sampsize = c(600, 600), 
                    data = training)

rf2
table(training$felonies)

test2 <- testing %>% 
  mutate(pred = predict(rf2, newdata = ., type = "response"))

confusionMatrix(reference=test2$felonies, test2$pred, positive = "1")


rf3 <- randomForest(felonies ~ ., 
                    sampsize = c(500, 600), 
                    data = training)
rf3

test3 <- testing %>% 
  mutate(pred = predict(rf3, newdata = ., type = "response"))

confusionMatrix(reference=test3$felonies, test3$pred, positive = "1")

rf4 <- randomForest(felonies ~ ., 
                    sampsize = c(1000, 1000), 
                    data = training)
rf4

test4 <- testing %>% 
  mutate(pred = predict(rf4, newdata = ., type = "response"))

confusionMatrix(reference=test4$felonies, test4$pred, positive = "1")



