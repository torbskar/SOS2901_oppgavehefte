library(tidyverse)    
library(pROC)
library(rsample)
library(janitor)   # Inneholder funksjonen clean_names() som bl.a. fjerner punktum i variabelnavnene så det blir litt greiere å bruke
library(caret)

# Lese inn data og forenkle noen variable 
recidivism_iowa <- read.csv("data/3-Year_Recidivism_for_Offenders_Released_from_Prison_in_Iowa_elaborated.csv") %>% 
  clean_names() %>% 
  rename(recidivism = recidivism_return_to_prison_numeric) %>%                            # Forenkler variabelnavnet
  mutate( race_ethnicity = case_when( str_starts(race_ethnicity, "White") ~ "white",      # Litt mange kategorier. Forenkler. 
                                      str_starts(race_ethnicity, "Black") ~ "Black", 
                                      TRUE ~ "Other" ),
          convicting_offense_classification = case_when(str_detect(convicting_offense_classification, "Felony") ~ "Felony",  # Litt mange kategorier. Forenkler. 
                                                        TRUE ~ "Other"),
          convicting_offense_subtype = fct_lump_min(convicting_offense_subtype, min = 200),  # fct_lump_min gjør små grupper (her min. 200) til "other"
          release_type = case_when( release_type == "" ~ "Other",                            # Litt mange kategorier. Forenkler. 
                                    str_detect(release_type, "Parole") ~ "Parole",
                                    TRUE ~ release_type)) %>% 
  filter(age_at_release != "") %>%                                                           # noen få observasjoner mangler alder. Sletter disse.  
  select(-release_type_paroled_to_detainder_united)                                          # Sletter en variabel. 
          
# Se om det ser ok ut
glimpse(recidivism_iowa)


# Splitte datasettet 

set.seed(426)
iowa_split <- initial_split(recidivism_iowa)
training <- training(iowa_split) 
testing  <- testing(iowa_split) 




gtsummary::tbl_summary(training)

est_logit <- glm(recidivism ~ . , data = training, family = binomial(link = "logit"))

summary(est_logit)



training_pred <- training %>% 
  mutate(prob = predict(est_logit, type = "response"), 
         prob_class = ifelse(prob > 0.5, 1, 0))

testing_pred <- testing %>% 
  mutate(prob = predict(est_logit, type = "response", newdata = testing),
         prob_class = ifelse(prob > 0.5, 1, 0))

glimpse(testing_pred)


testing_pred %>% 
  select(prob_class, recidivism) %>% 
  table() %>% 
  confusionMatrix()



confusionMatrix(as.factor(testing_pred$prob_class), reference = as.factor(testing_pred$recidivism))




ggplot(testing_pred, aes(x = prob)) +
  geom_histogram()


ROC <- roc( testing_pred$recidivism, testing_pred$prob )

auc(ROC)

df <- data.frame(Sensitivity = ROC$sensitivities, 
                 Specificity = ROC$specificities)

ggplot(df, aes(y = Sensitivity, x= (1-Specificity))) + 
  geom_line() + 
  geom_abline(intercept = 0, slope = 1, col = "gray")+
  coord_equal()



library(fairness)
res_acc <- acc_parity(data    = testing_pred, 
                      outcome = 'recidivism', 
                      group   = 'race_ethnicity',
                      probs   = 'prob', 
                      cutoff  = 0.4, 
                      base    = 'white')
res_acc$Metric
res_acc$Metric_plot

res_acc$Probability_plot
