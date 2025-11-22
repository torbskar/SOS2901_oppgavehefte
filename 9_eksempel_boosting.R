
library(tidyverse)   # datahåndtering, grafikk og glimpse()
library(rsample)     # for å dele data i training og testing
library(gbm)         # Funksjoner for boosting 
library(caret)       # For funksjonen confusionMatrix()
library(fairness)    # For fairnessmetrics 
library(gtsummary)   # For å lage litt penere tabeller


# Problemstilling: 
# Turnover i kundemasse. 
# Tiltak: Forutsi hvilke kunder som vil forlate selskapet og gi dem et tilbud, f.eks. billig med bindingstid... 
# Kostnader: Taper penger på kunder som forlater selskapet. Å gi for mange tilbud er også dyrt. 
#   Men hvis churn er høyt, kan det være verdt det uansett. 
#   Ønsker å godta flere false positive enn false negative. F.eks. dobbelt så mange? 


table(churn$Churn)

# Leser inn data 
# Gjør om utfallsvariabel til numerisk 0/1
# Sletter kunde-ID siden den ikke er relevant for modellen - skaper bare trøbbel 
churn <- read.csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv", stringsAsFactors = TRUE) %>% 
  mutate(Churn = ifelse(Churn == "Yes", 1, 0),
         SeniorCitizen = factor(SeniorCitizen)) %>% 
  select(-customerID)
glimpse(churn)


# Deler data i trenings- og testsett
set.seed(426)
training_init <- initial_split(churn)

training <- training(training_init)
testing  <- testing(training_init)




# Lager en modell med boosting
set.seed(542)
gb1 <- gbm(Churn ~ .,
                  data = training,
                  distribution = "bernoulli",
                  n.trees = 6000,
                  interaction.depth = 2,
                  n.minobsinnode = 1,
                  shrinkage = 0.001,
                  bag.fraction = 0.5)

gbm.perf(gb1, oobag.curve = TRUE, method = "OOB", 
         plot.it=T, overlay = T) 



# Tolkning ####

# Viktigste variable
summary(gb1) 


plot(gb1, i = "Contract", type = "response")
plot(gb1, i = "TotalCharges", type = "response")
plot(gb1, i = "SeniorCitizen", type = "response")




# Prediksjon på training data ####
test_p <- training %>% 
  mutate(pred = predict(gb1, n.trees = 6000, type = "response"),
         churn_k = ifelse(pred > .5, 1, 0))  
  
hist(test_p$pred)  # OBS! De predikerte sannsynlighetene er alle under .5 

tab <- table(test_p$Churn, test_p$churn_k) 

confusionMatrix(tab, positive = "1") 


## Asymmetriske kostnader? ####
# Kommentar: Modellen er ikke så god på å predikere churn
# Høy accuracy skyldes primært sanne negative
#

wts <- training %>% 
  mutate(wts = ifelse(Churn == 1, 2, 1)) %>%   # se begrunnelse s. 274 i Berk 
  pull(wts)


set.seed(542)
gb2 <- gbm(Churn ~ .,
           data = training,
           weights = wts,
           distribution = "bernoulli",
           n.trees = 6000,
           interaction.depth = 2,
           n.minobsinnode = 1,
           shrinkage = 0.001,
           bag.fraction = 0.5)

summary(gb2)

# Prediksjon på training data ####
# Vi kan også justere grensen for å predikere churn! Det har betydning. Prøv ut forskjellige verdier. 
test_p2 <- training %>% 
  mutate(pred = predict(gb2, n.trees = 6000, type = "response"),
         churn_k = ifelse(pred > .5, 1, 0))  

#hist(test_p2$pred)   

tab <- table(test_p2$Churn, test_p2$churn_k) 

confusionMatrix(tab, positive = "1")



# Fairness ####
# For et firma er kanskje anklager om aldersdiskriminering viktig å unngå! Reelt eller ikke... Så best å unngå det. 

# TP/(TP + FP)
pred_parity <- pred_rate_parity(data = test_p2, 
                            outcome      = 'Churn', 
                            group        = 'SeniorCitizen',
                            preds        = 'churn_k', 
                            base         = '0')

pred_parity


# False negative rate parity
# FN/(TP + FN) 
fn_parity <- fnr_parity(data = test_p2, 
                                outcome      = 'Churn', 
                                group        = 'SeniorCitizen',
                                preds        = 'churn_k', 
                                base         = '0')

fn_parity




## Justere modellen i henhold til bedre false negative rate parity ####

table(churn$SeniorCitizen)

wts <- training %>% 
  mutate(wts = case_when(
    SeniorCitizen == 1 & Churn == 0 ~ 1.2,
    SeniorCitizen == 1 & Churn == 1 ~ 1.2, 
    SeniorCitizen == 0 & Churn == 0 ~ 1,
    SeniorCitizen == 0 & Churn == 1 ~ .8)) %>% 
  pull(wts)


set.seed(542)
gb3 <- gbm(Churn ~ .,
           data = training,
           weights = wts,
           distribution = "bernoulli",
           n.trees = 6000,
           interaction.depth = 2,
           n.minobsinnode = 1,
           shrinkage = 0.001,
           bag.fraction = 0.5)

summary(gb3)


test_p3 <- training %>% 
  mutate(pred = predict(gb3, n.trees = 6000, type = "response"),
         churn_k = ifelse(pred > .6, 1, 0))  

#hist(test_p3$pred)   
tab <- table(test_p3$Churn, test_p3$churn_k) 
confusionMatrix(tab, positive = "1")


fn_parity <- fnr_parity(data = test_p3, 
                        outcome      = 'Churn', 
                        group        = 'SeniorCitizen',
                        preds        = 'churn_k', 
                        base         = '0')

fn_parity





# Så gjør det igjen på testing data ####
test_p4 <- testing %>% 
  mutate(pred = predict(gb3, newdata = testing, n.trees = 6000, type = "response"),
         churn_k = ifelse(pred > .5, 1, 0))  

hist(test_p4$pred)   
tab <- table(test_p4$Churn, test_p4$churn_k) 
confusionMatrix(tab, positive = "1")


fn_parity <- fnr_parity(data = test_p4, 
                        outcome      = 'Churn', 
                        group        = 'SeniorCitizen',
                        preds        = 'churn_k', 
                        base         = '0')

fn_parity






