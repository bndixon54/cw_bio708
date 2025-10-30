#Stats07 Lab
pacman::p_load(tidyverse,
               patchwork,
               here)

#Develop a linear model examining the effect of supplement type (supp), dose (dose), and their interaction on tooth length (len). Assign the developed model to m_tooth

#develop linear model
m_tooth <- lm(len ~ supp * dose,
              data = ToothGrowth)


#to test the vailidty of normality assumption, the sharpio test should be applied to the residuals
eps <-resid(m_tooth)
shapiro.test(eps)
#greater than .05 is good in this case! less than .05 means your dada IS significantly deviating from normality 



# model interpretation ----------------------------------------------------
df_pred <- ToothGrowth %>%
  group_by(supp) %>%
  reframe(dose = seq(min(dose),
                     max(dose),
                     length = 100))

y_pred <- predict(m_tooth,
             newdata = df_pred)

df_pred <- df_pred %>%
  mutate(y = y_pred)
#use this dataframe for visualization 

ToothGrowth %>%
  ggplot(aes(x = dose,
             y = len,
             color = supp)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
              aes(y = y))

# multicollinearity ----------------------------------------------------------------
#variance-covariance matrix
mv <- rbind(c(1, 0.9),
            c(0.9, 1))

#true regression coefficients
b <- c(0.05, 1.00)

#produce simulated data
set.seed(523)
X <- MASS::mvrnorm(100,
                   mu = c(0, 0),
                   Sigma = mv)

df_y <- tibble(x1 = X[,1],
               x2 = X[,2]) %>% 
  mutate(y = rnorm(nrow(.),
                   mean = 1 + b[1] * x1 + b[2] * x2))

#plot x1
x1 <- df_y %>%
  ggplot(aes(x = x1, 
             y = y)) + 
  geom_point()

#plot x2
x2 <- df_y %>%
  ggplot(aes(x = x2, 
             y = y)) +
  geom_point()

#model for x1 and x2
m <- lm(y ~ x1 + x2,
        data = df_y)

summary(m)

#look at relationship between x1 and x2
df_y %>%
  ggplot(aes(x = x1,
             y = x2)) +
  geom_point()

with(df_y,
     cor(x1, x2))








