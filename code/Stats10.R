#Stats 10
#Maximum Likelihood Model 

pacman::p_load(tidyverse,
               patchwork,
               here)

# R squared ---------------------------------------------------------------
set.seed(1) # for reproducibility

# hypothetical sample size
n <- 100

# true intercept and slope
b <- c(0.1, 0.5)

# hypothetical explanatory variable
x1 <- rnorm(n = n, mean = 0, sd = 1)

# create a design matrix
X <- model.matrix(~x1)

# expected values of y is a function of x
# %*% means matrix multiplication
# y = X %*% b equals y = b[1] + b[2] * x
# recall linear algebra
y_hat <- X %*% b

# add normal errors
y <- rnorm(n = n, mean = y_hat, sd = 0.5)

# plot
df0 <- tibble(y = y, x1 = x1)

df0 %>% 
  ggplot(aes(y = y,
             x = x1)) + 
  geom_point()

#develop a linear model
m1 <- lm(y ~ x1, 
         data = df0)
summary(m1)

df0 <- df0 %>%
  mutate(x2 = rnorm(nrow(.)))

df0 %>%
  ggplot(aes(x = x2,
             y = y)) +
  geom_point()

#develop model
m2 <- lm(y ~ x1 + x2,
         data = df0)
summary(m2)

#model complexity increased as the number of factors increases



#develop three models to compare
m1 <- lm(y ~ x1, 
         data = df0)
#Adjusted R squared: 0.4619 best model!!!

m2 <- lm(y ~ x1 + x2,
         data = df0)
#Adjusted R squared: 0.4577 

m3 <- lm(y ~ x2,
         data = df0)
#Adjusted R squared: -0.00964



# likelikood ratio test ---------------------------------------------------
logLik(m1)
logLik(m2)

#deviance: 
dev_m1 <- -2 * logLik(m1)
dev_m2 <- -2 * logLik(m2)
#greater value now worse, smaller now better because it is a measure of DEVIATION! 

#ratio:
log_lr <- dev_m1 - dev_m2

#avona function to compare using log likelihood; input is models you want to compare:
anova(m1, m2, test = "Chisq")


# AIC ---------------------------------------------------------------------

#Calculate with AIC function
AIC(m1)
AIC(m2)

#multi model inference with AIC
#install.packages("MuMIn")
library(MuMIn)

#using fish data from 14.4 Laboratory
url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

#first, develop model
m_full <- glm(n_sp ~ distance + cat_area + hull_area,
              data = df_fish,
              family = "poisson")

#magic spell
options(na.action = "na.fail")

#model selection table:
mst <- dredge(m_full,
              rank = "AIC")






