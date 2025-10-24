#Biostats assessment02

library(tidyverse)

# sampling ----------------------------------------------------------------

# GUIDE:
# The original `iris` dataset has 150 observations, with 50 samples per species.
# Convert the base R `iris` dataframe to a tibble for easier manipulation.
df_iris <- as_tibble(iris)

# Using `nrow(df_iris)` will confirm there are 150 rows.
nrow(df_iris) # return 150

# Using `group_by()` will confirm there are 50 rows for each species.
df_iris %>% 
  group_by(Species) %>% 
  summarize(n_sample = n()) # return n_sample = 50 each

# The function `sample_n(size = 3)` randomly selects 3 observations from the dataset. 
# When applied after `filter(Species == XX)`, it samples 3 observations from a given species.
df_s3 <- df_iris %>% 
  filter(Species == "versicolor") %>% 
  sample_n(size = 3)

# You can take the mean of a selected column using `pull()` and `mean()`
df_s3 %>% 
  pull(Petal.Width) %>% 
  mean()

# Questions:
# 1. Create a `for` loop (with loop index `i`) to perform the following steps for 100 iterations:
#    a. Randomly sample 5 individuals of "versicolor".
#    b. Calculate the mean of their "Petal.Width" values.
#    c. Assign the result to `mu5[i]`. 
#       (Hint: initialize an empty object `mu5` before starting the loop.)
#    This procedure will yield 100 estimates of the mean Petal.Width from 5 individuals.

set.seed(3)

mu5 <- var_i <- NULL # create empty objects

#for loop
for (i in 1:100) {
  
  df_s5 <- df_iris %>% 
    filter(Species == "versicolor") %>% 
    sample_n(size = 5)
  
  # save mean for sample set i
  mu5[i] <- mean(df_s5$Petal.Width)
  
  # save variance for sample set i
  var_i[i] <- sum((df_s5$Petal.Width - mean(df_s5$Petal.Width))^2) / nrow(df_s5) 
}

# 2. Calculate the standard deviation of `mu5` and assign it to `s_mu5`.
s_mu5 <- sd(mu5)

# 3. Repeat step 1 but sample 20 individuals in each iteration, and assign the results to `mu20`.


set.seed(3)

mu20 <- var_x <- NULL # create empty objects

#for loop
for (i in 1:100) {
  
  df_s20 <- df_iris %>% 
    filter(Species == "versicolor") %>% 
    sample_n(size = 20)
  
  # save mean for sample set i
  mu20[i] <- mean(df_s20$Petal.Width)
  
  # save variance for sample set i
  var_x[i] <- sum((df_s20$Petal.Width - mean(df_s20$Petal.Width))^2) / nrow(df_s5) 
}

# 4. Calculate the standard deviation of `mu20` and assign it to `s_mu20`.
#    Verify that `s_mu20` is smaller than `s_mu5` by printing the comparison (`s_mu20 < s_mu5`)
s_mu20 <- sd(mu20)

s_mu20 < s_mu5
#TRUE

# probability density -----------------------------------------------------

# GUIDE:
# For continuous variables (e.g., body length), we use a probability density function (PDF) 
# to describe how values are distributed and how likely different values are.
# R provides built-in functions to work with probability distributions:
# - Probability Density Functions (PDF) for evaluating how likely a value is.
# - Cumulative Distribution Functions (CDF) for evaluating the probability 
#   that a value is less than or equal to a given number.
# These functions are available for many common distributions (e.g., normal).

# `dnorm(x = 1, mean = 0, sd = 1)` calculates the probability density of x = 1 
# for a normal distribution with mean = 0 and standard deviation = 1.
# Probability density tells us how "likely" a value is relative to other values.
# In this example, x = 1 is more likely than x = 2 under this distribution.

dnorm(x = 1, mean = 0, sd = 1)  # returns 0.2419707
dnorm(x = 2, mean = 0, sd = 1)  # returns 0.05399097

# `pnorm(q = 1, mean = 0, sd = 1)` calculates the probability (not probability density!) of x <= 1
# for a normal distribution with mean = 0 and standard deviation = 1.
pnorm(q = 1, mean = 0, sd = 1) # returns 0.8413447
pnorm(q = 0, mean = 0, sd = 1) # returns 0.5

# Questions:
# 1. Calculate the probability that 0 < x <= 10 for a normal distribution with mean = 5 and sd = 3.
#    Hint: Use `pnorm()` function and subtract P(x <= 0) from P(x <= 10).
p0 <- pnorm(q = 0, mean = 5, sd = 3)
p10 <- pnorm(q = 10, mean = 5, sd =3)

# probability of 0 < x <= 10
p0_10 <- p10 - p0
print(p0_10)
#0.904


# 2. Calculate the probability that 0 < x <= 10 for a normal distribution with mean = -5 and sd = 3.
p0 <- pnorm(q = 0, mean = -5, sd = 3)
p10 <- pnorm(q = 10, mean = -5, sd =3)

# probability of 0 < x <= 10
p0_10 <- p10 - p0
print(p0_10)
#0.047

# t-test ------------------------------------------------------------------

# GUIDE:
# The `t.test()` function in R can perform a t-test with two options for handling variances: `var.equal = TRUE` or `FALSE`.
#If the equal-variance assumption holds, using `var.equal = TRUE` can increase the power to detect a true difference.
#If the assumption is seriously violated, using `var.equal = TRUE` may produce incorrect or misleading results.
#Even though both options can give similar results in many cases, it is important to understand the assumptions behind each test.

# Questions:
# 1. Using the `df_iris` dataset, calculate the variance of "Petal.Length" for each species.
#    (Hint: use `group_by()` and `summarize()` from dplyr.)

df_var <- df_iris %>% 
  group_by(Species) %>% # group operation
  summarize(mu_l = mean(Petal.Length), # summarize by mean()
            sd_l = var(Petal.Length)) # summarize with sd()

#setosa mu_l = 1.462, sd_l = 0.03015918
#versicolor mu_l = 4.260, sd_l = 0.22081633
#virginica mu_l = 5.552, sd_l = 0.30458776

#visualize
df_iris %>% 
  ggplot(aes(x = Species,
             y = Petal.Length)) +
  geom_jitter(width = 0.1, # scatter width
              height = 0, # scatter height (no scatter with zero)
              alpha = 0.25) + # transparency of data points
  geom_segment(data = df_var, # switch data frame
               aes(x = Species,
                   xend = Species,
                   y = mu_l - sd_l,
                   yend = mu_l + sd_l)) +
  geom_point(data = df_var, # switch data frame
             aes(x = Species,
                 y = mu_l),
             size = 3) +
  labs(x = "Species", # x label
       y = "Petal length") # y label


# 2. Perform a t-test comparing "Petal.Length" between "setosa" and "versicolor".
#    Choose the appropriate `var.equal` option depending on whether the SDs appear equal.
x <- df_iris %>%
  filter(Species == "setosa") %>% 
  pull(Petal.Length)

y <- df_iris %>%
  filter(Species == "versicolor") %>% 
  pull(Petal.Length)

t.test(x, y, var.equal = FALSE)


# anova -------------------------------------------------------------------

# GUIDE:
# The `aov()` function in R can be used to perform ANOVA.
# The built-in dataset `InsectSprays` provides a useful example.
# It contains insect count data for different types of insect sprays.

df_insect <- as_tibble(InsectSprays)

# Questions:
# 1. Using the `df_insect` dataset, create a plot to visualize the distribution of insect counts for each spray type.
#Use a violin plot to show the distribution and include the median (y = count, x = spray).
#Add individual data points with a small horizontal jitter for clarity.
df_insect %>% 
  ggplot(aes(x = spray,
             y = count)) +
  geom_violin(draw_quantiles = 0.5, 
              alpha = 0.4,
              fill = "steelblue") + 
  geom_jitter(alpha = 0.4)

# Question:
# 2. Using the `df_insect` dataset, perform a one-way ANOVA to test whether insect counts differ among spray types.
#    - Use the `aov()` function and display the summary of the model with `summary()`.
#    - Report if there was a significant difference between spray groups.
m <- aov(count ~ spray,
         data = df_insect) 
summary(m)

#difference between groups calculated by...
# Between group variability -----------------------------------------------

#estimate overall mean of response variable (count)
mu <- mean(df_insect$count)

#estimate group means and sample size each from global/overall mean 
s_b <- df_insect %>% 
  group_by(spray) %>% 
  summarize(mu_g = mean(count), #mean for each group
            n = n()) %>% #sample size for each group
  mutate(dev_g = (mu_g - mu)^2, #squared deviation for each group
         ss_g = dev_g * n) %>% #sum across groups
  pull(ss_g) %>%
  sum()


# Within group variability ------------------------------------------------
#group level mean vs individual fish lengths

s_w <- df_insect %>%
  group_by(spray) %>%
  mutate(mu_g = mean(count)) %>% #use mutate() to retain individual rows
  ungroup() %>% #dont need group structure anymore
  mutate(dev_i = (count - mu_g)^2) %>% #deviation from group mean for each fish
  pull(dev_i) %>%
  sum()


# summed square to values -------------------------------------------------
s2_b <- s_b / (n_distinct(df_insect$spray) - 1) 
s2_w <- s_w / (nrow(df_insect) - n_distinct(df_insect$spray)) #to get the number of lakes (3)


# calculate F statistic ---------------------------------------------------
(f_value <- s2_b / s2_w)

f_null <- seq(0, 10, by = 0.1)
y <- df(x = f_null,
        df1 = 1, 
        df2 = 70) #individuals minus # of groups

tibble(x = f_null,
       y = y) %>%
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_vline(xintercept = f_value,
             color = "red") 
#p-value is the area under the curve on the left side of the red line

p_value <- 1 - pf(q = f_value, 
                  df1 = 1, 
                  df2 = 70)




