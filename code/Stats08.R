#Stats 08
#Generalized Linear Model

pacman::p_load(tidyverse,
               patchwork,
               here)


# Count data --------------------------------------------------------------
df_count <- read_csv(here("data_raw/data_garden_count.csv"))

nrow(df_count)

m_normal <- lm(count ~ nitrate, #response ~ predictor
               data = df_count)

summary(m_normal)

#visualize
alpha <- coef(m_normal)[1]
beta <- coef(m_normal)[2]

ggplot(df_count) + 
  geom_point(aes(x = nitrate,
                 y = count)) +
  geom_abline(intercept = alpha,
              slope = beta)
#limitation of drawing a straight line on this plot: densitiy CANNOT be negative, but the model can predict negative values

#poison distribution can capture the error here. appropriate for this response variable #is it continuous or discrete? Discrete
#is it only positive numbers? Yes

#Random number generator from poisson
(y <- rpois(n = 10, #number of samples you want to draw
           lambda = 2))

#apply poisson distribution using GLM
m_pois <- glm(count ~ nitrate, #response ~ predictor, assumes a linear relationship
              data = df_count,
              family = "poisson") #defines what probability distribution to use

summary(m_pois)

#visualize
ggplot(df_count) + 
  geom_point(aes(x = nitrate,
                 y = count)) +
  geom_abline(intercept = coef(m_pois)[1],
              slope = coef(m_pois)[2])
#this code doesnt work because our intercept and slope values are not correct


# poisson regression ------------------------------------------------------

df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
                                max(df_count$nitrate),
                                length = 100))

y_pred <- predict(m_pois,
                  newdata = df_pred) %>%
         exp()

df_pred <- df_pred %>%
  mutate(y = y_pred)

#visualize
ggplot(df_count,
       aes(x = nitrate,
           y = count)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(y = y))

#compare summary distributions 
summary(m_normal)
summary(m_pois)
#is this case, both results work. Chose type of model based on the nature of the data we are exploring 


# Proportional data -------------------------------------------------------
#normal distribution is not appropriate to use here

df_mussel <- read_csv(here("data_raw/data_mussel.csv"))
print(df_mussel)

#n_fertilized is dependent on n_examined, which sets the upper limit of values
#use binomial model!!! because there is a specific number of examined objects

#plot data: calculate the proportion of fertilized eggs
df_mussel <- df_mussel %>% #overwrite
  mutate(prop_fert = n_fertilized / n_examined)

# plot
df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() 

#coding the binomial model needs cbind function for response variable
cbind(df_mussel$n_fertilized, df_mussel$n_examined - df_mussel$n_fertilized) 


#binomial model with GLM
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")


summary(m_binom)


#how logit function works
# x: produce 100 numbers from -100 to 100 (assume logit scale)
# y: convert with inverse-logit transformation (ordinary scale)
df_test <- tibble(logit_x = seq(-10, 10, length = 100),
                  x = exp(logit_x) / (1 + exp(logit_x)))

#visualized the relationshiop
df_test %>% 
  ggplot(aes(x = logit_x,
             y = x)) +
  geom_point() +
  geom_line()


















