library(tidyverse)
library(patchwork)

# Normal Distribution -----------------------------------------------------
#Generate a variable with 50 observations.
#Create a figure similar to Figure 9.3

#step one is to create the data set from normal distribution 
x <- rnorm(50, mean = 100, sd = 5)

#create vector with minimum value and maximum value of v using floor/ceiling functions
bin <- seq(floor(min(x)),  #bin becomes a vector
           ceiling(max(x)),
           by = 1)

#calculate probability in each bin with pnorm()
pnorm(bin[2], mean = mean(x), sd = sd(x)) - pnorm(bin[1], mean = mean(x), sd = sd(x))

#looped so function applies to each variable 
p <- NULL # empty object for probability
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mean(x), sd = sd(x)) - pnorm(bin[i], mean = mean(x), sd = sd(x))
}
print(p)

#create tibble to organize the data 
df_prob <- tibble(bin = bin[-length(bin)] + 0.5,
                  prob = p) %>%
  mutate(freq = length(x) * prob)

#convert probability to frequency 
df_x <- tibble(x = x)

df_x %>%
  ggplot(aes(x = x)) +
  geom_histogram() + 
  geom_point(data = df_prob, 
             aes(x = bin,
                 y = freq),
             color = "salmon") +
  geom_line(data = df_prob, 
            aes(x = bin,
                y = freq),
            color = "salmon")


# Poisson Distribution ----------------------------------------------------
#get data for 1000 samples
x <- rpois(n = 1000, lambda = 10)
bin <- seq(0,
           max(x) + 5, #to cover wider range of values
           by = 1)

# calculate probability mass
p <- dpois(bin, lambda = mean(x))

#make the dataframe
df_prob <- tibble(bin = bin,
                  prob = p) %>% 
  mutate(freq = length(x) * p) 

df_x <- tibble(x = x)

#figure
df_x %>% 
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 0.5, 
                 center = 0) +
  geom_line(data = df_prob,
            aes(x = bin,
                y = freq)) +
  geom_point(data = df_prob,
             aes(x = bin,
                 y = freq))

