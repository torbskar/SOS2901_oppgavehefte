# https://www.geeksforgeeks.org/machine-learning/understanding-spline-regression-in-r/


# Pakker ####
library(caret)
library(tidyverse)
library(splines)
library(Ecdat)



# Lineær regresjon - basics #### 


kommune <- readRDS( "data/kommunedata.rds") %>% 
  mutate(prop_unge_menn = (menn_18_25 + menn_26_35)/bef_totalt*100) %>% 
  mutate(prop_menn_18_25 = menn_18_25/bef_totalt*100, 
         prop_menn_26_35 = menn_26_35/bef_totalt*100) %>% 
  na.omit()



ggplot(kommune, aes(x = prop_menn_18_25, 
                    y = voldskriminalitet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 



## Split dataset ####
set.seed(42) 
kommune_split <- initial_split(kommune, prop = .7) 
kommune_train <- training(kommune_split)
kommune_test <- testing(kommune_split)


ggplot(kommune_train, aes(x=prop_unge_menn)) +
  geom_density(fill = "red", alpha=.3)+
  geom_density(data = kommune_test, fill = "blue", alpha=.3)

## Estimere enkel lineær regresjon ####
#est1 <- lm(voldskriminalitet ~ prop_unge_menn, data = kommune_train)
#summary(est1)

est1 <- lm(voldskriminalitet ~ prop_menn_18_25 + prop_menn_26_35, data = kommune_train)
summary(est1)

# sjekk SE for RMSE
n <- length(est1$residuals)
# Using N-2 for general regression context (adjust as needed)
std_err_rmse <- sd(est1$residuals) / sqrt(n - 2)



## RMSE / MSE  ####
rmse1 <- RMSE( predict(est1), kommune_train$voldskriminalitet)

rmse1 - 1.96*std_err_rmse
rmse1 + 1.96*std_err_rmse


## Predikere på nye data ####
kommune_pred <- kommune_test %>% 
  mutate(pred1 = predict(est1, newdata = kommune_test),
         res = voldskriminalitet - pred1) 

RMSE(kommune_pred$pred1, kommune_pred$voldskriminalitet)

# residual plot
ggplot(kommune_pred, aes(x = pred1, y = res)) + 
  geom_point() 







# Splines ####
library(Ecdat)
library(splines)
data(Clothing)

glimpse(Clothing)

## Se på data ####
p0 <- ggplot(Clothing, aes(inv2, tsales))+
  geom_point()+
  geom_smooth(method = "lm", se = F, col = "gray")

p0

summary(Clothing$inv2)


## data for predict/plot
inv2.grid <- data.frame(inv2 = seq(min(Clothing$inv2), max(Clothing$inv2)))
head(inv2.grid)


## Piecewise linear spline ####
model1 <- lm(tsales ~ bs(inv2, knots = c(12000, 60000, 150000), degree = 1), data = Clothing)
summary(model1)

## B-spline ####
model2 <- lm(tsales ~ bs(inv2, knots = c(12000, 60000, 150000), degree = 3), data = Clothing)
summary(model2)

pred1 <- predict(model1, newdata = inv2.grid, se = TRUE, interval = "prediction")$fit %>% 
  bind_cols(inv2.grid)
head(pred1)

pred2 <- predict(model2, newdata = inv2.grid, se = TRUE, interval = "prediction")$fit %>% 
  bind_cols(inv2.grid)


p0 +
  geom_line(data = pred1, aes( y = fit), linewidth = 1, col = "blue")
  

pred2 <- predict(model2, newdata = inv2.grid, se = TRUE, interval = "prediction")$fit %>% 
  bind_cols(inv2.grid)

ggplot(Clothing, aes(inv2, tsales))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  geom_line(data = pred1, aes( y = fit), linewidth = 1, col = "blue") +
  geom_line(data = pred2, aes( y = fit), linewidth = 1, col = "red")


library(mgcv)
model3 <- gam(tsales ~ s(inv2), data = Clothing)

pred3 <- inv2.grid %>% 
  mutate(fit = predict(model3, newdata = inv2.grid) )

summary(model3)

ggplot(Clothing, aes(inv2, tsales))+
  #geom_point()+
  geom_smooth(method = "lm", se = F)+
  geom_line(data = pred1, aes( y = fit)) +
  geom_line(data = pred2, aes( y = fit), col = "red") +
  geom_line(data = pred3, aes( y = fit), col = "purple")



## Kategorisk utfall
attrition <- readRDS("data/Attrition.rds")

glimpse(attrition)

# OBS! fjern "ident"
attrition <- attrition %>%  
  select(- EmployeeNumber) %>% 
  mutate(Attrition.num = as.numeric(Attrition == "Yes"))



ggplot(attrition, aes(x = Age, y = Attrition.num)) +
  geom_point()+
  


set.seed(426)
attrition_split <- initial_split(attrition)
training <- training(attrition_split) 
testing  <- testing(attrition_split) 


est.lm <- lm(Attrition.num ~ Age, data = training)
summary(est.lm)

est.glm <- glm(Attrition ~ Age, data = training, family= "binomial")
summary(est.glm)

testing <- testing %>% 
  mutate(prob_lm = predict(est.lm, newdata = testing, type = "response"),
         prob_glm = predict(est.glm, newdata = testing, type = "response")) 


ggplot(testing, aes(x = prob_lm))+
  geom_density(fill = "red", alpha = .3)+
  geom_density( aes(x = prob_glm), fill = "blue", alpha = .3)


# Multippel reg
est.lm.2 <- lm(Attrition.num ~ ., data = training[, -2])
summary(est.lm.2)

est.glm.2 <- glm(Attrition ~ ., data = training[, -32], family= "binomial")
summary(est.glm.2)


testing <- testing %>% 
  mutate(prob_lm2 = predict(est.lm.2, newdata = testing, type = "response"),
         prob_glm2 = predict(est.glm.2, newdata = testing, type = "response")) %>% 
  mutate(klass.lm = ifelse(prob_lm2 > .5, "Yes", "No" ),
         klass.glm = ifelse(prob_glm2 > .5, "Yes", "No" ))


ggplot(testing, aes(x = prob_lm2))+
  geom_density(fill = "red", alpha = .3)+
  geom_density( aes(x = prob_glm2), fill = "blue", alpha = .3)


# lm
confusionMatrix(reference = testing$Attrition, factor(testing$klass.lm))

# glm
confusionMatrix(reference = testing$Attrition, factor(testing$klass.glm))


library(pROC)
ROC <- roc( testing$Attrition, testing$prob_glm2 )
auc(ROC)

df <- data.frame(Sensitivity = ROC$sensitivities, 
                 Specificity = ROC$specificities)

ggplot(df, aes(y = Sensitivity, x= (1-Specificity))) + 
  geom_line() + 
  geom_abline(intercept = 0, slope = 1, col = "gray")+
  coord_equal()





