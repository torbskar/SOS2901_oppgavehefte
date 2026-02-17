library(tidyverse)   # datahåndtering, grafikk og glimpse()
library(rsample)     # for å dele data i training og testing





kommune <- readRDS( "data/kommunedata.rds") %>% 
  mutate(prop_unge_menn = (menn_18_25 + menn_26_35)/bef_totalt*100, 
         bef_18min = bef_18min/bef_totalt*100, 
         bef_18_25 = bef_18_25/bef_totalt*100, 
         bef_26_35 = bef_26_35/bef_totalt*100, 
         prop_shj_unge = shj_unge/bef_totalt*100
         ) %>% 
  select(voldskriminalitet, prop_unge_menn, bef_18min, bef_18_25, bef_26_35, prop_shj_unge, inntekt_eskatt_median) %>% 
  na.omit()




ggplot(kommune, aes(x = prop_unge_menn, 
                          y = voldskriminalitet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 





set.seed(42) 
kommune_split <- initial_split(kommune, prop = .7) 
kommune_train <- training(kommune_split)
kommune_test <- testing(kommune_split)

est1 <- lm(voldskriminalitet ~ prop_unge_menn, data = kommune_train)
summary(est1)

est2 <- lm(voldskriminalitet ~ prop_unge_menn + bef_18min + bef_18_25 + prop_shj_unge + inntekt_eskatt_median, data = kommune_train)
summary(est2)

est3 <- lm(voldskriminalitet ~ .^2 , data = kommune_train)
summary(est3)

est4 <- lm(voldskriminalitet ~ .^3 , data = kommune_train)
summary(est4)




sqrt(mean(est1$res^2))
sqrt(mean(est2$res^2))
sqrt(mean(est3$res^2))


kommune_pred <- kommune_test %>% 
  mutate(pred1 = predict(est1, newdata = .), 
         pred2 = predict(est2, newdata = .), 
         pred3 = predict(est3, newdata = .),
         pred4 = predict(est4, newdata = .))



rmse <- kommune_pred %>% 
  mutate(res1 = pred1 - voldskriminalitet,
         res2 = pred2 - voldskriminalitet,
         res3 = pred3 - voldskriminalitet,
         res4 = pred4 - voldskriminalitet) %>% 
  mutate(sq.resid1 = res1^2,
         sq.resid2 = res2^2,
         sq.resid3 = res3^2,
         sq.resid4 = res4^2) %>% 
  summarise(sqrt(mean(sq.resid1)), sqrt(mean(sq.resid2)), sqrt(mean(sq.resid3)), sqrt(mean(sq.resid4)) ) 

rmse


hist(kommune_pred$pred1)

plot(roc(kommune_pred$voldskriminalitet/10, kommune_pred$pred3), print.auc = TRUE)

plot(roc(dat_sd$y, lin_mod), print.auc = TRUE)


library(mgcv)

est1_gam <- gam( voldskriminalitet ~ s(prop_unge_menn) + s(voldskriminalitet_lag1), data = kommune_train)

plot(est1_gam)







####
# %>% 
  arrange(kommune, year) %>% 
  group_by(kommune) %>% 
  mutate(voldskriminalitet_lag1 = lag(voldskriminalitet)) %>% 
  select(voldskriminalitet, voldskriminalitet_lag1, prop_unge_menn, bef_18min, bef_18_25, bef_26_35, prop_shj_unge, inntekt_eskatt_median) %>% 
  na.omit()
  
  
  ggplot(kommune, aes(x = voldskriminalitet_lag1, 
                      y = voldskriminalitet)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) 
  