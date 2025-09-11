#11 September 2025 Lab

# 7.3.1 central tendency --------------------------------------------------------
library(tidyverse)

#1. Create a new vector z with length 1000 as exp(rnorm(n = 1000, mean = 0, sd = 0.1)), and calculate the arithmetic mean, geometric mean, and median.

z <- exp(rnorm(n = 1000, mean = 0, sd = 0.1))

#arithmetic mean
mu_z <- sum(z) / length(z)
#or just use built in function
am_z <- mean(z)

#geometric mean
#calculate using prod(), length(), ^
prod(z)^ (1 / length(z))

#or take in log scale
geo_z <- exp(mean(log(z)))

#median 
z <- runif(9, min = 10, max = 20)
z <- sort(z)   #sorts vector from smallest to largest
index <- (length(z) + 1) / 2   #DONT FORGET parenthesis on numerator!!!
z[index]
#or built in function
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
geom_vline(xintercept = am_z) +
  geom_vline(xintercept = geo_z,
             color = "red") +
  geom_vline(xintercept = med_z,
             color = "green")
print(g1)

#4. Compare the values of the central tendency measures.



#5. Create a new vector z_rev as -z + max(z) + 0.1, and repeat step 1 – 4.
z_rev <- -z + max(z) + 0.1

#5.1 arithmetic mean
am_z_rev <- mean(z_rev)

#5.2 geometric mean
geo_z_rev <- exp(mean(log(z_rev)))

#5.3 median 
med_z_rev <- median(z_rev)

#5.4 histogram with lines
df_z_rev <- tibble(x = z)

df_z_rev %>%
  ggplot(aes(x = x)) +
  geom_histogram() 

g2 <- df_z_rev %>%
  ggplot(aes(x = x)) +
  geom_histogram() +
  geom_vline(xintercept = am_z_rev) +
  geom_vline(xintercept = geo_z_rev,
             color = "red") +
  geom_vline(xintercept = med_z_rev,
             color = "green")
print(g2)

#Well... that isnt right... but its close? 
#will redo this later to correct it 


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
  