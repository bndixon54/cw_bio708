#Stats 06 Lab
library(tidyverse)
df_iris <- iris
#Sepal.Width (response variable, y) and Petal.Width (explanatory variable, x)

# Setosa ------------------------------------------------------------------
df_setosa <- df_iris %>%
  filter(Species == "setosa")

#visualize
df_setosa %>%
  ggplot(aes(x = Petal.Width,
             y = Sepal.Width)) +
  geom_point()

###regression         
m_set <- lm(Sepal.Width ~ Petal.Width, #response ~ predictor
          data = df_setosa) 
  
alpha_set <- coef(m_set)[1]
beta_set <- coef(m_set)[2]
  
###t-value for regression slope beta
se_set <- sqrt(diag(vcov(m_set)))
t_value <- beta_set / se_set[2]

###p-value for slope
1 - pt(t_value, df = 48) + pt(-t_value, df = 48)
# 0.104



# Versicolor --------------------------------------------------------------
df_versicolor <- df_iris %>%
  filter(Species == "versicolor")

#visualize
df_versicolor %>%
  ggplot(aes(x = Petal.Width,
             y = Sepal.Width)) +
  geom_point()
        
###regression
m_vers <- lm(Sepal.Width ~ Petal.Width, #response ~ predictor
              data = df_versicolor) 
         
alpha_vers <- coef(m_vers)[1]
beta_vers <- coef(m_vers)[2]

###t-value for regression slope beta
se_vers <- sqrt(diag(vcov(m_vers)))
t_value <- beta_vers / se_vers[2]
         
###p-value for slope
1 - pt(t_value, df = 48) + pt(-t_value, df = 48)
# 1.466


# Virginica ---------------------------------------------------------------
df_virginica <- df_iris %>%
  filter(Species == "virginica")

#visualize
df_virginica %>%
  ggplot(aes(x = Petal.Width,
             y = Sepal.Width)) +
  geom_point()

###regression
m_vir <- lm(Sepal.Width ~ Petal.Width, #response ~ predictor
             data = df_versicolor) 

alpha_vir <- coef(m_vir)[1]
beta_vir <- coef(m_vir)[2]

###t-value for regression slope beta
se_vir <- sqrt(diag(vcov(m_vir)))
t_value <- beta_vir / se_vir[2]

###p-value for slope
1 - pt(t_value, df = 48) + pt(-t_value, df = 48)
# 1.466


# exercise 2 --------------------------------------------------------------
#select one species and compare between:
#model 1: Petal.Width only - versicolor
m_vers_2 <- lm(Sepal.Width ~ Petal.Width + Sepal.Length, 
             data = df_versicolor) 

summary(m_vers)
summary(m_vers_2)


alpha_vers <- coef(m_vers)[1]
beta_vers <- coef(m_vers)[2]


#model 2: Petal.Width and Petal.Length

#compare the following
#regression estimate of Petal.Width
#R^2 values (which model is better?)


# exercise 3 --------------------------------------------------------------
#try this...
x <- rnorm(nrow(iris), mean = 0, sd = 1)
iris <- iris %>%
  mutate(x = x)

#'x' is a random variable
#include this 'x' in one of the models you developed in exercise 1
#investigate whether 'x' improves R^2 or not
df_vers <- iris %>%
  filter(Species == "versicolor")

m_ver <- lm(Sepal.Width ~ Petal.Width,
            data = df_vers)

m_ver_w_x <- lm(Sepal.Length ~ Petal.Width + x,
                df_vers)