library(tidyverse)
library(rsample)
library(caret)

# Leser inn data
attrition <- readRDS("data/Attrition.rds")

glimpse(attrition)


## Splitte datasettet ####
set.seed(426)
attrition_split <- initial_split(attrition)
training <- training(attrition_split) 
testing  <- testing(attrition_split) 


# logistisk modell
est.glm.2 <- glm(Attrition ~ ., data = training[, -32], family= "binomial")

testing <- testing %>% 
  mutate(prob_glm2 = predict(est.glm.2, newdata = testing, type = "response")) %>% 
  mutate(klass.glm = ifelse(prob_glm2 > .5, "Yes", "No" )) %>% 
  mutate(klass.glm = factor(klass.glm))

glimpse(testing)

# Enkel tabell 
testing %>% 
  select(Attrition, klass.glm) %>% 
  table()

library(gtsummary)
testing %>% 
  select(Attrition, klass.glm) %>% 
  tbl_cross()

confusionMatrix(reference = testing$Attrition, testing$klass.glm, positive = "Yes")

# sensitivity: 
24/(24+44)






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



# logistisk modell
est.glm <- glm(felonies ~ ., data = training, family= "binomial")

summary(est.glm)

hist(est.glm$fitted.values)


testing <- testing %>% 
  mutate(prob_glm = predict(est.glm, newdata = testing, type = "response")) %>% 
  mutate(klassifiser = factor(ifelse(prob_glm < .5, 0, 1 ))) %>% 
  drop_na()

summary(testing$prob_glm)

testing %>% 
  select(felonies, klassifiser) %>% 
  table()

confusionMatrix(reference = factor(testing$felonies), factor(testing$klassifiser), positive = "1")







# Klassifikasjonstrær

library(rpart)
library(rpart.plot)

glimpse(training)

table(training$felonies)

klass_tre <- rpart( felonies ~ . , data = training, cp = 0, maxdepth = 25, 
                    minbucket = 5, method = "class")

# for å endre cut-off i splitt, legg til: parms = list(prior = c(.65,.35)) 

rpart.plot(klass_tre)

rpart.rules(klass_tre)




testing <- testing %>% 
  mutate(klassifiser = predict(klass_tre, newdata = testing, type = "class")) 


confusionMatrix(reference = testing$felonies, testing$klassifiser, positive = "1")

