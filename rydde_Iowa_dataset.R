library(tidyverse)    
library(pROC)
library(rsample)
library(janitor)   # Inneholder funksjonen clean_names() som bl.a. fjerner punktum i variabelnavnene så det blir litt greiere å bruke

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
