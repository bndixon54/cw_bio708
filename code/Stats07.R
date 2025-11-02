#Stats07 - 28 October 2025

library(tidyverse)
library(patchwork)
library(here)

df_fl <- read_csv(here("data_raw/data_fish_length.csv"))
print(df_fl)

m <- lm(length ~ lake,
        data = df_fl)

#calculate group means for lake length
v_mu <- df_fl %>%
  group_by(lake) %>%
  summarize(mu_l = mean(length)) %>%
  pull(mu_l)

#mean length for lake a:
v_mu[1]

#mean length for lake b:
v_mu[2]

#difference between the lakes:
v_mu[2] - v_mu[1]

#mean length for lake b (same as mean):
sum(coef(m))

#look into lm result
summary(m)

#compare with t-test
a <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

b <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

t.test(x = a, 
       y = b,
       var.equal = TRUE)
#results are the same


# ANOVA vs lm -------------------------------------------------------------
df_anova <- read_csv(here("data_raw/data_fish_length_anova.csv"))
print(df_anova)

#response variable length, predictor variable lake
m1 <- lm(length ~ lake,
         data = df_anova)

#get group means
v_mu_anova <- df_anova %>%
  group_by(lake) %>%
  summarize(mu_l = mean(length)) %>%
  pull(mu_l)

#corresponds to "intercept"
v_mu_anova[1]

#corresponds to "lake b"
v_mu_anova[2] - v_mu_anova[1]

#corresponds to "lake c"
v_mu_anova[3] - v_mu_anova[1]

#comparison with aov()
aov(length ~ lake,
    data = df_anova) %>%
    summary()


# ANCOVA ------------------------------------------------------------------
m2 <- lm(Sepal.Length ~ Sepal.Width + Species,
         data = iris)

#visualization of ANCOVA
#develop linear iris model with continuous and categorical values
m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)

summary(m_iris)

#what the model predicts: 
#make data frame with predictor values 
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = 100),
                                    times = 3), #times repeated across species
                  Species = rep(unique(iris$Species),
                                each = 100))

#get predicted values
y_pred <- predict(m_iris,
          newdata = df_pred)

#associate this data
df_pred <- df_pred %>%
  mutate(y_pred = y_pred)

#get visual output
iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred))



# interaction -------------------------------------------------------------

#how to include interaction
#shortcut version:
m_int <- lm(Petal.Length ~ Petal.Width * Species,
   data = iris)

#identical model with different expression:
#lm(Petal.Length ~ Petal.Width * Species + Petal.Width:Species,
#   data = iris)



