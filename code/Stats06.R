#Regression 21 October 2025

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               terra,
               tidyterra,
               mapview,
               stars,
               here)

#read data
df_algae <- read_csv(here("data_raw/data_algae.csv"))
print(df_algae)

#visualize
df_algae %>%
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point()


# try regression ----------------------------------------------------------
m <- lm(biomass ~ conductivity, #response ~ predictor
        data = df_algae) 

alpha <- coef(m)[1]
beta <- coef(m)[2]
#the numbers are used to draw best fit line

df_algae %>%
  ggplot(aes(y = biomass,
             x = conductivity)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta)


# Standard errors and t-values --------------------------------------------

#t-value for regression slope beta
se <- sqrt(diag(vcov(m)))
t_value <- beta / se[2]

#p-value for slope
1 - pt(t_value, df = 48) + pt(-t_value, df = 48)
#df is 48 here because it is number of samples (50) minus number of parameters (in this case the parameters are intercept and slope)


# Coefficient of determination --------------------------------------------
#other term for 'R squared'
#residual error is the difference between the line and the data point along the y-axis (imagine bell curve) (?) 

eps <- resid(m)

#visualization
df_algae <- df_algae %>%
  mutate(eps = eps)

#draw figure
df_algae %>%
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) +
  geom_segment(aes(x = conductivity,
                   xend = conductivity, #start/end point of segment has same value and x-axis
                   y = biomass,
                   yend = biomass - eps),
               linetype = "dashed")

ss <- sum(eps^2)
ss0 <- sum(df_algae$biomass - mean(df_algae$biomass))^2

r_sq <- 1 - (ss / ss0)

#compare with output
summary(m)












