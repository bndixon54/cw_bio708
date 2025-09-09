# ctrl + shift + n to make a new script file 

library(tidyverse)


# central tendency --------------------------------------------------------
#several ways to get the "average"


    # arithmetic mean
#calculate the arithmetic mean of v_x using length() and sum()
v_x <- rnorm(10) #rnorm produces a set of 10 random numbers, normal distribution 
mu_x <- sum(v_x) / length(v_x)

#arithemtic mean is very sensitive to outliers, so we use geometric mean in these cases. population growth is a good example of this
  #appropriate to use if distribution is normal


    # geometric mean 
#calculate using prod(), length(), ^
v_y <- runif(10, min = 10, max = 20)
prod(v_y)^ (1 / length(v_y))

#or
exp(mean(log(v_y)))


    # median
v_z <- runif(9, min = 10, max = 20)
v_z <- sort(v_z)   #sorts vector from smallest to largest
index <- (length(v_z) + 1) / 2   #DONT FORGET parenthesis on numerator!!!
v_z[index]
median(v_z)


# variance measures -------------------------------------------------------

  # variance 
#use sum(), length() and mean() to define variance 
v_a <- rnorm(100)
s2 <- sum((v_a - mean(v_a))^ 2) / length(v_a)
s <- sqrt(s2)
s
  

  # interquantile range 
#xl is lower quantile, xh is higher
a_l <- quantile(v_a, probs = 0.25)
a_h <- quantile(v_a, probs = 0.75)
(iqr <- abs(a_h - a_l))



  # MAD
abs(v_a - median(v_a)) #to get the median, then ... 
median(abs(v_a - median(v_a)))




  # Coefficient of Variation
      #commonly used in ecology 
#use v_b, s and mean() of v_b to define cv
v_b <- runif(100, min = 10, max = 20)
s2 <- sum((v_b - mean(v_b))^ 2) / length(v_b)
s <- sqrt(s2)

cv <- s / mean(v_b)



  # MAD / median using v_b

mad <- median(abs(v_b - median(v_b))) 
med <- median(v_b)

mad2med <- mad / med









