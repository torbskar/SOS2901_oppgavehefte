
library(tidyverse)
n <- 1000

set.seed(70)
x <- rnorm(n, sd=3) 

x <- runif(n, min = 0, max = 10)

y <- 1 + 2.5*x + .5*x^2  - .06*x^3 + rnorm(n)
df <- data.frame(x = x, y = y)

ggplot(df, aes(x, y)) +
  geom_smooth( se = FALSE, linewidth = 2) +
  geom_point() +
  theme_minimal()+
  theme(legend.position = "none")



# no normalization 
km_df <- kmeans(df, centers = 4, nstart=20)

df_p <- df %>% 
  mutate(cluster = km_df$cluster)

ggplot(df_p, aes(x=x, y = y, col = factor(cluster), shape = factor(cluster))) +
  geom_point()+
  theme_minimal()+
  theme(legend.position = "none") 


ggplot(df_p, aes(x=x, y = y, col = factor(cluster), shape = factor(cluster))) +
  geom_point()+
  theme_minimal() +
  theme(legend.position = "none") +
  coord_equal()



# normalize all 
df2 <- df %>% 
  scale() %>% 
  as.data.frame()
  # mutate(x = (x - mean(x))/sd(x),
  #        y = (y - mean(y))/sd(y))

km_df <- kmeans(df2, centers = 4, nstart=20)

df_p <- df2 %>% 
  mutate(cluster = km_df$cluster)


ggplot(df_p, aes(x=x, y = y, col = factor(cluster), shape = factor(cluster))) +
  geom_point()+
  theme_minimal() +
  theme(legend.position = "none") 


library(gtsummary)
theme_gtsummary_mean_sd()



desc <- df %>% 
  mutate(cluster = km_df$cluster)
desc %>% 
  tbl_summary(by = cluster)


df3 <- data.frame(x = 1, y = -.5)

predict(km_df, df3) 

?predict.kmeans