library(tidyverse)
library(rsample)


# Synthetic data #### 
load("data/synthetic_birthCohort96.RData")

# ta en titt på struktur, sjekk variabeltype for felonies
glimpse(synthetic_birthCohort)

# sjekk: f.eks. antall eller dummy? 
table(synthetic_birthCohort$felonies)

# Litt enkel datahåndtering: fjern to variable, gjør felonies til factor 
synth <- synthetic_birthCohort %>% 
  select(-drugs, -violence) %>% 
  mutate(felonies = factor(felonies))



# splitter datasettet
set.seed(426)
synth_split <- initial_split(synth, prop = .7)
training <- training(synth_split) 
testing  <- testing(synth_split) 
?initial_split

# logistisk modell, alle variable 
est.glm <- glm(felonies ~ ., data = training, family= "binomial")

summary(est.glm)

# Enkel måte å sjekke fordeling av predikert sannsynlighet
hist(est.glm$fitted.values)

# Predikerer på testing-data
# Gjør klassifisering med cut-off på 50/50
testing <- testing %>% 
  mutate(prob_glm = predict(est.glm, newdata = testing, type = "response")) %>% 
  mutate(klassifiser = factor(ifelse(prob_glm < .5, 0, 1 ))) %>% 
  drop_na()

# Enkel krysstabell 
testing %>% 
  select(klassifiser, felonies) %>% 
  table()

library(caret)
# Bruker confusionMatrix
confusionMatrix(reference = testing$felonies, 
                testing$klassifiser, 
                positive = "1")

