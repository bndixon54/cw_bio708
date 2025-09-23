# 23 September 2025
#Probability distribution in class 

library(tidyverse)

# load csv data on R
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width
                 center = 0.5) + # bin's center specification
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean


# normal distribution -----------------------------------------------------

#approximate this historgram with mean in standard deviation... using R function

# vector of x values
# seq() to calculate min to max values with specified numbers of elements or interval
# the following produce 100 elements
x <- seq(min(df_h0$height), max(df_h0$height), length = 100)

mu <- mean(df_h0$height)

sigma <- sd(df_h0$height)

#can now use these to calculate the probability density (at each data point)
pd <- dnorm(x, mean = mu, sd = sigma) #pd is the y-axis variable

#make figure using these numbers
tibble(y = pd, 
       x = x) %>%    #data frame
  ggplot(aes(x = x, 
             y = y)) +
  geom_line() +    #draw lines
  labs(y = "Probability density",
       x = "Plant height")   #re-label to adjust the figure

#larger sd number will result in more spread out curve, because it is using more data points
#overlay histogram onto pd curve to check if it is a good fit

#covert probability density to probability using pnorm function 

# probability of (negative infinity) x < 10
p10 <- pnorm(q = 10, mean = mu, sd = sigma)
print(p10)

# probability of x < 20
p20 <- pnorm(q = 20, mean = mu, sd = sigma)
print(p20)

# probability of 10 < x < 20 (between 10 and 20)
p20_10 <- p20 - p10
print(p20_10)

# p20_10 is still not resolved enough to overlay onto original histogram; have to repeate the function over and over again:
x_min <- floor(min(df_h0$height)) # floor takes the integer part of the value
x_max <- ceiling(max(df_h0$height)) # ceiling takes the next closest integer
bin <- seq(x_min, x_max, by = 1) # each bin has 1cm

#calculate probability in each bin with for loop:
p <- NULL # empty object for probability values to go into
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) - pnorm(bin[i], mean = mu, sd = sigma)
}
print(p)


# data frame for probability
# bin: last element [-length(bin)] was removed to match length
# expected frequency in each bin is "prob times sample size"
# "+ 0.5" was added to represent a midpoint in each bin
df_prob <- tibble(p, 
                  bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_h0))


# Overlay observed freq values over   !!!
df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, #specify bin width; must match width for probability
                 center = 0.5) + #bin's center position
  geom_point(data = df_prob,
             mapping = aes(y = freq,
                           x = bin),
             color = "salmon") +
  geom_line(data = df_prob,
            mapping = aes(y = freq,
                          x = bin),
            color = "salmon")

#most important part is first getting the mean and standard distribution from the data



# Discrete variable -------------------------------------------------------

#poisson distribution 
df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))
print(df_count)

#make histogram showing variation in plant count
df_count %>% 
  ggplot(aes(x = count)) + 
  geom_histogram(binwidth = 0.5, #define binwidth
                 center = 0) #relative position of each bin

#probability mass function (PMF) represents the probability of a discrete variable (x) taking a specific value.


# vector of x values
# create a vector of 0 to 10 with an interval one (this will cover the whole range of df_count, which is 7)
# must be integer of > 0
x <- seq(0, 10, by = 1) 

#probability mass
lambda_hat <- mean(df_count$count) #lambda_hat is estimated 
pm <- dpois(x, lambda = lambda_hat) #lambda is mean from the data


#make figure
tibble(y = pm, x = x) %>% #data frame
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "dashed") + #draw dashed lines
  geom_point() + #draw points
  labs(y = "Probability",
       x = "Count") #re-label

#convert this to probability! 
df_prob <- tibble(x = x, 
                  y = pm) %>%
  mutate(freq = y * nrow(df_count)) 

df_count %>%
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5) +
  geom_line(data = df_prob,  #overlay expected frequency onto histogram
            aes(x = x,
                y = freq),
            linetype = "dashed") + 
  geom_point(data = df_prob, 
             aes(x = x,
                 y = freq))


