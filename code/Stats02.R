#16 September 2025 - Sampling in class 
#command + shift + N is hotkey for creating a new file in R

library(tidyverse)


# 10 individual samples ---------------------------------------------------

#first data set
h1 <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h1 <- tibble(plant_id = 1:10, #a vector from 1 to 10 by 1
                height = h1,   #height
                unit = "cm")  #unit

#calculate mean height and average height 
df_h1 <- df_h1 %>% 
  mutate(mu_h = mean(h1), 
         var_h = sum((h1 - mean(h1))^2) / nrow(.))

print(df_h1)



#different data set
h2 <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h2,
                unit = "cm") %>% 
  mutate(mu_h = mean(height),
         var_height = sum((height - mu_h)^2) / nrow(.))

print(df_h2)

#parameter is an unknown constant that represents the entire data set, but cannot be measured. 
#in this data set, parameter is the mean 
#sample mean is unbiased estimation of the overall mean



#read garden data that was saved as a csv file 

# load csv data on R; specify the file path 
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

# show the first 10 rows
print(df_h0)

#calculate true mean (average for 1000 individuals) and true variance 
mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)


#determine if these are biased or unbiased estimates by repeating the estimation processs
mu_i <- var_i <- NULL

for (i in 1:100) {df_i <- df_h0 %>%
                      sample_n(10)
                      (mu_i[i] <- mean(df_i$height))
                      (var_i[i] <- mean((df_i$height - mean(df_i$height))^2) / nrow(df_i))}


#____________________________________________________________
#randomly select 10 rows with sample_n function 
df_i <- df_h0 %>%
  sample_n(10)
#____________________________________________________________
#repeat process over and over again
mu_i <- mean(df_i$height) 
var_i <- mean((df_i$height - mu_i)^2) / nrow(df_i) #variance
#_____________________________________________________________
#install.packages("ggplot2")
#install.packages("patchwork")
library(patchwork)


df_sample <- tibble(mu_hat = mu_i, 
                      var_hat = var_i)

#histogram for the mean
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_i)) +
  geom_histogram() + 
  geom_vline(xintercept = mu)

#histogram for variance 
g_var <- df_sample %>% 
  ggplot(aes(x = var_i)) +
  geom_histogram() + 
  geom_vline(xintercept = sigma2) 
#mine here looks super off. histogram skewed to the far left, but the line is on the far right not touching the figure itself ??

g_mu / g_var 
#Error in g_mu/g_var : non-numeric argument to binary operator ??


#_______________________________________________________________________________
#variance estimate unbiased

# for reproducibility
set.seed(3)

# redo simulations ----
mu_i <- var_i <- var_ub_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
  var_ub_i[i] <- var(df_i$height)
}

#______________________________________________________________________________
#add histograms of unbiased variance 
# draw histograms ----
df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i,
                    var_ub_hat = var_ub_i)

# histogram for mu
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

# histogram for variance
# scale_x_continuous() adjusts scale in x-axis
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

# histogram for unbiased variance
g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

g_mu / g_var / g_var_ub

