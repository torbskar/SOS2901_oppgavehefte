
# Hvis du ikke har gjort det før: installere noen nødvendige pakker 
# install.packages(c("tidyverse", "rsample"))


# Last inn pakker 
library(tidyverse)
library(rsample)


# Lese inn data  
kommune <- readRDS( "data/kommunedata.rds") 

# Forslag: lag en ny variabel som angir andel unge menn i kommunen
kommune <- kommune %>% 
  mutate(prop_unge_menn = (menn_18_25 + menn_26_35)/bef_totalt*100) 


# Splitter datasettet i training og testing
set.seed(42) 
kommune_split <- initial_split(kommune, prop = .7) 

kommune_train <- training(kommune_split) 
kommune_test <- testing(kommune_split)


# Estimere lineær regresjon på training-data 
# Lagrer resultatet i nytt objekt 
est <- lm(voldskriminalitet ~ prop_unge_menn, data = kommune_train)

# Lag ny variabel for predikert verdi 
# Merk bruk av mutate() og predict()
kommune_test <- kommune_test %>% 
  mutate(pred = predict(est, newdata = kommune_test )) %>% 
  mutate(res = pred - voldskriminalitet)

# Root mean square error (RMSE) kan regnes ut på training data fra residual som ligger i est-objektet
rmse_train <- sqrt(mean(est$residuals^2))
rmse_train

# RMSE finnes jo ikke i testing-data, men vi lagde residualer ovenfor, så bruk disse
rmse_test <- sqrt(mean(kommune_test$res^2))
rmse_test







training <- kommune %>% 
  filter(year == 2018)

testing <- kommune %>% 
  filter(year == 2020)





glimpse(kommune)

kommune %>% 
  group_by(year) %>% 
  summarise(n = n())



kommune_mean = kommune %>% 
  mutate(prop_unge_menn = round(prop_unge_menn, digits = 1)) %>% 
  group_by(prop_unge_menn) %>% 
  summarise(mean = mean(voldskriminalitet)) %>% 
  ungroup()

glimpse(kommune)
summary(kommune)

p0 <-ggplot(kommune, aes(x = prop_unge_menn, y = voldskriminalitet)) +
  geom_point(alpha = .3) +
  labs(x = "Andel unge menn", y = "Voldskrim. per 1000 innb.")+
  facet_wrap(~year)

p0  +
  geom_point(data = kommune_mean, aes(y = mean), col = "red", size = 2)+
  geom_line(data = kommune_mean, aes(y = mean), col = "red", linewidth = 2)




p0 + 
  geom_line(data = kommune_pred, aes(y = pred), col = "red", linewidth = 1)



predict(est, newdata = data.frame(prop_unge_menn = seq(8, 18))) %>%  round(digits = 1)









ggplot(testing, aes(x = prop_unge_menn, y = voldskriminalitet)) + 
  geom_point()




