#-----------------------------------; 
# Til forelesning 13.01.2026 
# SOS2901 
#-----------------------------------; 

# TIPS: Merk bruk av overskrifter med #### på slutten. Klikk på knapp øverst i høyre hjørne i script-vinduet for å se innholdsfortegnelse. 


# Laste pakker ####
library(caret)        # enkelte funksjoner som f.eks. confusionMatrix()
library(tidyverse)    # generell databehandling, inkludert grafikk 
library(splines)      # funksjoner for splines  
library(Ecdat)        # pakke med div. datasett, deriblant Clothing 
library(mgcv)         # pakke for generaliserte additive modeller, herunder gam() 


# Lineær regresjon - basics #### 


## Lese inn kommunedata #### 
# Lager også nye variable med andeler
kommune <- readRDS( "data/kommunedata.rds") %>% 
  mutate(prop_unge_menn = (menn_18_25 + menn_26_35)/bef_totalt*100) %>% 
  mutate(prop_menn_18_25 = menn_18_25/bef_totalt*100, 
         prop_menn_26_35 = menn_26_35/bef_totalt*100) %>% 
  na.omit()                                                     # Kan legges til for sikkerhetsskyld. Dropper alle observasjoner med missing


# Grafikk
ggplot(kommune, aes(x = prop_menn_18_25, 
                    y = voldskriminalitet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 



## Split datasett ####
# Bruk seed for å få samme resultat neste gang. Denne nullstiller for funksjoner basert på tilfeldigheter. 
set.seed(42) 
kommune_split <- initial_split(kommune, prop = .7) 
kommune_train <- training(kommune_split)
kommune_test <- testing(kommune_split)


# Ta en titt på om fordelingene ser like ut på train og test data. 
ggplot(kommune_train, aes(x=prop_unge_menn)) +
  geom_density(fill = "red", alpha=.3)+
  geom_density(data = kommune_test, fill = "blue", alpha=.3)   # Merk: man kan legge på plot for flere datasett ved å spesifisere. Bruker aes som angitt over.


## Estimere enkel lineær regresjon ####
est1 <- lm(voldskriminalitet ~ prop_menn_18_25 + prop_menn_26_35, data = kommune_train)
summary(est1)


# sjekk SE for RMSE. Trenger ikke dette i kurset. Tok en sjekk fordi resultatet på testing først så litt rart ut.
n <- length(est1$residuals)
std_err_rmse <- sd(est1$residuals) / sqrt(n - 2)

# RMSE / MSE  
rmse1 <- RMSE( predict(est1), kommune_train$voldskriminalitet)

# konfidensintervall. 
rmse1 - 1.96*std_err_rmse
rmse1 + 1.96*std_err_rmse


## Predikere på nye data ####
kommune_pred <- kommune_test %>% 
  mutate(pred1 = predict(est1, newdata = kommune_test),  # Merk bruk av newdata = kommune_test  Kan også sette newdata = . pga hvordan %>% fungerer. 
         res = voldskriminalitet - pred1)                # residual

# RMSE
RMSE(kommune_pred$pred1, kommune_pred$voldskriminalitet)  

# Residual plot (standard)
ggplot(kommune_pred, aes(x = pred1, y = res)) + 
  geom_point() 




# Splines ####

# Eksempel data som ligger i pakken Ecdat
data(Clothing)

# ta en titt på data
glimpse(Clothing)

p0 <- ggplot(Clothing, aes(inv2, tsales))+
  geom_point()+
  geom_smooth(method = "lm", se = F, col = "gray")

p0



## Lage data for predict/plot ####
#  bruker dette til å vise grafisk etterpå 
inv2.grid <- data.frame(inv2 = seq(min(Clothing$inv2), max(Clothing$inv2)))  # seq() lager en sekvens av tall fra angitt minste til største tall
head(inv2.grid)


## Piecewise linear spline ####
model1 <- lm(tsales ~ bs(inv2, knots = c(12000, 60000, 150000), degree = 1), data = Clothing)  # merk: degree = 1 angir lineær funksjon 
summary(model1)

## B-spline ####
model2 <- lm(tsales ~ bs(inv2, knots = c(12000, 60000, 150000), degree = 3), data = Clothing)  # merk: degree = 3 angir kubisk funksjon (prøv gjerne høyere grads polynomer hvis du vil)
summary(model2)

## Predikerer på nye observasjoner ####
# predikerer på datasettet vi lage ovenfor: inv2.grid 

# modell med linear spline
pred1 <- predict(model1, newdata = inv2.grid, se = TRUE, interval = "prediction")$fit %>% 
  bind_cols(inv2.grid)  # Linjen over lager en data.frame. bind_cols() setter disse sammen. Funker bare hvis observasjonenene har sammme rekkefølge, slik de har her. 
head(pred1)

# modell med cubic spline
pred2 <- predict(model2, newdata = inv2.grid, se = TRUE, interval = "prediction")$fit %>% 
  bind_cols(inv2.grid)

# grafikk. Merk: grunnleggende plot ble lagret i p0 ovenfor
p0 +
  geom_line(data = pred1, aes( y = fit), linewidth = 1, col = "blue")
  
## Grafikk: punkter og modeller i samme. #### 
ggplot(Clothing, aes(inv2, tsales))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  geom_line(data = pred1, aes( y = fit), linewidth = 1, col = "blue") +
  geom_line(data = pred2, aes( y = fit), linewidth = 1, col = "red")


# Modell med GAM 
model3 <- gam(tsales ~ s(inv2), data = Clothing)  # merk at s() angir smoothing spline. Forvalg er 'thin plate regression spline', men det finnes flere typer. 

# predikerer for gam
pred3 <- inv2.grid %>% 
  mutate(fit = predict(model3, newdata = inv2.grid) )

summary(model3)

# Grafikk: alle modeller uten punkter. Fjern # for å få med punkter også. 
ggplot(Clothing, aes(inv2, tsales))+
  #geom_point()+
  geom_smooth(method = "lm", se = F)+
  geom_line(data = pred1, aes( y = fit)) +
  geom_line(data = pred2, aes( y = fit), col = "red") +
  geom_line(data = pred3, aes( y = fit), col = "purple")



# Kategorisk utfall ####

# Leser inn data
attrition <- readRDS("data/Attrition.rds")

glimpse(attrition)

# OBS! fjern variabel for "ident"
#      lag numerisk versjon av utfallsvariabel for lm(), mens glm() kan bruke original factor-variabel
attrition <- attrition %>%  
  select(- EmployeeNumber) %>% 
  mutate(Attrition.num = as.numeric(Attrition == "Yes"))


# grafikk 
ggplot(attrition, aes(x = Age, y = Attrition.num)) +
  geom_point()+
  

## Splitte datasettet ####
set.seed(426)
attrition_split <- initial_split(attrition)
training <- training(attrition_split) 
testing  <- testing(attrition_split) 


# Lineær modell med lm()
est.lm <- lm(Attrition.num ~ Age, data = training)
summary(est.lm)

# Logistisk modell med glm()
est.glm <- glm(Attrition ~ Age, data = training, family= "binomial")
summary(est.glm)

# Prediker nye verdier i testing-data ####
testing <- testing %>% 
  mutate(prob_lm = predict(est.lm, newdata = testing, type = "response"),
         prob_glm = predict(est.glm, newdata = testing, type = "response")) 

# grafikk 
ggplot(testing, aes(x = prob_lm))+
  geom_density(fill = "red", alpha = .3)+
  geom_density( aes(x = prob_glm), fill = "blue", alpha = .3)


# Multippel reg ####
# Merk: 1) dropper en factor-versjonen av utfallsvariabelen fra datasettet, 
#       2) bruker ~ . for å indikere at vi bruker ALLE variable som prediktorer

# lineær modell
est.lm.2 <- lm(Attrition.num ~ ., data = training[, -2])
summary(est.lm.2)

# logistisk modell
est.glm.2 <- glm(Attrition ~ ., data = training[, -32], family= "binomial")
summary(est.glm.2)

# prediksjon og klassifikasjon #### 
# Merk: prediksjonen er sannsynlighet, og klassifikasjon bruker cut-off på denne. 
#       klassifikasjonen må ha samme verdier som opprinnelige variabel når vi skal bruke confusionMatrix() etterpå
testing <- testing %>% 
  mutate(prob_lm2 = predict(est.lm.2, newdata = testing, type = "response"),
         prob_glm2 = predict(est.glm.2, newdata = testing, type = "response")) %>% 
  mutate(klass.lm = ifelse(prob_lm2 > .5, "Yes", "No" ),
         klass.glm = ifelse(prob_glm2 > .5, "Yes", "No" ))

# Se på fordeling av sannsynligheter 
ggplot(testing, aes(x = prob_lm2))+
  geom_density(fill = "red", alpha = .3)+
  geom_density( aes(x = prob_glm2), fill = "blue", alpha = .3)

## Confusion matrix ####
# Merk at for å få riktige utregninger må funksjonen vite hva som er hva. 
#   1) reference = ... angir hva som er observert utfall. 
#   2) positive = ... angir hva som er utfallsverdi av interesse. Utfallet vi bryr oss om er når Attrition er "Yes". 

# lm
confusionMatrix(reference = testing$Attrition, factor(testing$klass.lm), positive = "Yes")

# glm
confusionMatrix(reference = testing$Attrition, factor(testing$klass.glm), positive = "Yes")


## ROC curves #### 
library(pROC)
ROC <- roc( testing$Attrition, testing$prob_glm2 )
auc(ROC)

df <- data.frame(Sensitivity = ROC$sensitivities, 
                 Specificity = ROC$specificities)

ggplot(df, aes(y = Sensitivity, x= (1-Specificity))) + 
  geom_line() + 
  geom_abline(intercept = 0, slope = 1, col = "gray")+
  coord_equal()


