#Stats 09 
#Maximum Likelihood Model 

pacman::p_load(tidyverse,
               patchwork,
               here)


# Least square ------------------------------------------------------------
set.seed(1) #can be any number

#hypothetical sample size
n <- 100

#true intercept and slope
b <- c(0.1, 0.5)

#simulated predictor 
x1 <- rnorm(n = n, mean = 0, sd = 1)

#design matrix
X <- model.matrix(~ x1)


#get simulated y value
y_hat <- X %*% b #%*% is matrix multiplication

plot(y_hat ~ x1)


#add errors
y <- rnorm(n = n, mean = y_hat, sd = 0.5)

#make a tibble
df0 <- tibble(y = y, 
              x1 = x1)

ggplot(df0,
       aes(y = y,
           x = x1)) + 
geom_point()


#fit model to simulated data
lm(y ~ x1,
   data = df0)



# likelihood --------------------------------------------------------------

#probability of observing 3 with 
dpois(3, lambda = 3.5)

dpois(1, lambda = 3.5)

dpois(4, lambda = 3.5)

dpois(10, lambda = 3.5)


#try different lambda values
lambda <- seq(0, 10, by = 0.1)
pr <- dpois(3, lambda = lambda)

#visualize
df_pois <- tibble(y = 3,
                  lambda = lambda, 
                  pr = pr)


df_pois %>%
  ggplot(aes(x = lambda,
             y = pr)) +
  geom_point() +
  geom_line() +
  labs(x = "lambda",
       y = "Pr(y = 3)")


df_pois %>%
  arrange(desc(pr))

#probability of observing values 3, 2, and 5 at the same time
pr <- dpois(c(3, 2, 5), lambda = 3)
prod(pr)



y <- c(3, 2, 5)
lambda <- seq(0, 10, by = 0.01)

#sapply repeats the task in FUN
pr <- sapply(X = lambda,
             FUN = function(z) prod(dpois(y, lambda = z)))

#make a data frame and arrange by pr (likelihood)
df_pois <- tibble(lambda = lambda,
                  pr = pr)
             
df_pois %>% 
   arrange(desc(pr)) %>% 
   print()

#visualize
df_pois %>% 
  ggplot(aes(x = lambda,
             y = pr)) +
  geom_line() +
  labs(x = "lambda",
       y = "Pr")        

mean(c(3, 2, 5))


      # 15.2.2 General Case
#load garden plant data
df_count <- read_csv(here("data_raw/data_garden_count.csv"))

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")

logLik(m_pois)
