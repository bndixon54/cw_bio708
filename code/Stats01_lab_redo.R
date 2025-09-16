#11 Sept Lab redo
library(tidyverse)

#1. Create a new vector z with length 100 as exp(rnorm(n = 100, mean = 0, sd = 0.1)), and calculate the arithmetic mean, geometric mean, and median of z.
z <- exp(rnorm(n = 1000, mean = 0, sd = 0.1))

#arithmetic mean is sum of vector divided by length of sum of vector 
mu_z <- sum(z) / length(z)

#geometric mean can be calculated by scratch or calculated with built-in function
prod_z <- prod(z) # product of vector x; x1 * x2 * x3...
n_z <- length(z)
geom_z <- prod_z^(1 / n_z)
              # OR
geom_z <- exp(mean(log(z)))

#median just use built in function...
med_z <- median(z)



#2. Draw a histogram of z using functions tibble(), ggplot(), and geom_histogram().
df_z <- tibble(x = z)

df_z %>%
  ggplot(aes(x = x)) +
  geom_histogram() 



#3. Draw vertical lines of arithmetic mean, geometric mean, and median on the histogram with different colors using a function geom_vline().
g1 <- df_z %>%
  ggplot(aes(x = x)) +
  geom_histogram() +
  geom_vline(xintercept = mu_z,
             color = "red") +
  geom_vline(xintercept = geom_z,
            color = "blue") +
  geom_vline(xintercept = med_z,
             color = "green")
print(g1)



#5. Create a new vector z_rev as -z + max(z) + 0.1, and repeat step 1 – 4.
z_rev <- -z + max(z) + 0.1

#arithmetic mean is sum of vector divided by length of sum of vector 
mu_z_rev <- sum(z_rev) / length(z_rev)

#geometric mean from built-in function
geom_z_rev <- exp(mean(log(z_rev)))

#median from built in function
med_z_rev <- median(z_rev)




#2. Draw a histogram of z using functions tibble(), ggplot(), and geom_histogram().
df_z_rev <- tibble(x = z_rev)

df_z_rev %>%
  ggplot(aes(x = x)) +
  geom_histogram() 



#3. Draw vertical lines of arithmetic mean, geometric mean, and median on the histogram with different colors using a function geom_vline().
g2 <- df_z_rev %>%
  ggplot(aes(x = x)) +
  geom_histogram() +
  geom_vline(xintercept = mu_z_rev,
             color = "red") +
  geom_vline(xintercept = geom_z_rev,
             color = "blue") +
  geom_vline(xintercept = med_z_rev,
             color = "green")
print(g2)



# 7.3.2 variation ---------------------------------------------------------------
w <- rnorm(100, mean = 10, sd = 1) #unit is grams
head(w) # show first 10 elements in w


#1. Convert the unit of w to “milligram” and create a new vector m.
m <- 1000 * w

#2. Calculate SD and MAD for w and m.
s2_w <- sum((w - mean(w))^2) / length(w)
s_w <- sqrt(s2_w)

s2_m <- sum((m - mean(m))^2) / length(m)
s_m <- sqrt(s2_m)

mad_w <- median(abs(w - median(w)))
mad_m <- median(abs(m - median(m)))

#3. Calculate CV and MAD/Median for w and m.
#CV
cv_w <- s_w / mean(w) 
cv_m <- s_m / mean(m)

#MAD
madr_w <- mad_w / median(w)
madr_m <- mad_m / median(m)



