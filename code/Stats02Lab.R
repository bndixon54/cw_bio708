#Sampling Lab

library(tidyverse)
library(patchwork)

df_h <- read_csv(here::here("data_raw/data_plant_height.csv"))

#true mean and variance
mu <- mean(df_h$height)
sigma2 <- sum((df_h$height - mu)^2) / nrow(df_h)

#1. We used 10 plants to estimate sample means and variances. Obtain 100 sub-datasets with 50 and 100 measures each, and draw histograms of sample means and unbiased variances (use var()).
#for 100 individuals 
mu100_i <- var100_i <- NULL 

for (i in 1:100) {
  df_i <- df_h %>% 
    sample_n(size = 100)
  
  # save mean for sample set of 100
  mu100_i[i] <- mean(df_i$height)
  
  # save variance for sample set 100
  var100_i[i] <- var(df_i$height)
}



#for 50 individuals 
mu50_i <- var50_i <- NULL 

for (i in 1:100) {
  df_i <- df_h %>% 
    sample_n(size = 50)
  
  # save mean for sample set of 50
  mu50_i[i] <- mean(df_i$height)
  
  # save variance for sample set 50
  var50_i[i] <- var(df_i$height)
}

#create tibble
df_est <- tibble(mu100 = mu100_i,
                 var100 = var100_i,
                 mu50 = mu50_i,
                 var50 = var50_i)



#histogram for 100 mean
g100_mu <- df_est %>%
  ggplot(aes(x = mu100)) +
  geom_histogram() +
  geom_vline(xintercept = mu) +
  scale_x_continuous(limits = c(18, 22))

#histogram for 50 mean
g50_mu <- df_est %>%
  ggplot(aes(x = mu50)) +
  geom_histogram() +
  geom_vline(xintercept = mu) +
  scale_x_continuous(limits = c(18, 22))

g100_mu / g50_mu


#histogram for 100 variance
g100_var <- df_est %>%
  ggplot(aes(x = var100)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits = c(10, 40))

#histogram for 50 variance 
g50_var <- df_est %>%
  ggplot(aes(x = var50)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits = c(10, 40))

g100_var / g50_var


