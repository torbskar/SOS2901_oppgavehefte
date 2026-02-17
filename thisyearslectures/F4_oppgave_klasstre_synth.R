

library(tidyverse)
library(rsample)
library(rpart)
library(rpart.plot)
library(caret)


# Synthetic data #### 
load("data/synthetic_birthCohort96.RData")

glimpse(synthetic_birthCohort)

synth <- synthetic_birthCohort %>% 
  select(-drugs, -violence) %>% 
  mutate(felonies = factor(felonies))


# splitter datasettet
set.seed(426)
synth_split <- initial_split(synth)
training <- training(synth_split) 
testing  <- testing(synth_split) 


glimpse(training)

table(training$felonies)

klass_tre <- rpart( felonies ~ . , data = training, method = "class")
rpart.plot(klass_tre)


klass_tre <- rpart( felonies ~ . , data = training, cp = 0.0000001, 
                    maxdepth = 20, 
                    minbucket = 3, method = "class")

rpart.plot(klass_tre)

# for å endre cut-off i splitt, legg til: parms = list(prior = c(.65,.35)) 

rpart.plot(klass_tre)

rpart.rules(klass_tre)


testing <- testing %>% 
  mutate(klassifiser = predict(klass_tre, newdata = testing, 
                               type = "class")) 


confusionMatrix(reference = testing$felonies, 
                testing$klassifiser, positive = "1")





# Basic bagging ####
bag <- bagging(felonies ~ .  , data = training, 
               nbagg = 100, coob = TRUE)

# Legger til justeringer for de enkelte trærne 
bag <- bagging(felonies ~ .  , data = training, 
               nbagg = 100, coob = TRUE,
               control = rpart.control(cp = 0.0000001, maxdepth = 20, 
                             minbucket = 3))

bag

# Predikerer
testing <- testing %>% 
  mutate(pred = predict(bag, newdata = .)) 


# Confusion matrix
confusionMatrix(reference = testing$felonies, testing$pred, positive = "1")