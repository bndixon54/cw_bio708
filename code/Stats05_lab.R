#09 October 2025 - Stats 05 Lab
#ANOVA

pacman::p_load(tidyverse,
               patchwork,
               here,
               pwr)

data("PlantGrowth")

df_growth <- as_tibble(PlantGrowth)


#11.5.1
#Question 1 - figure
df_growth %>% 
  ggplot(aes(x = group,
             y = weight)) +
  geom_violin(draw_quantiles = 0.5, 
              alpha = 0.3, 
              fill = "steelblue") + 
  geom_jitter(width = 0.1,
              alpha = 0.5) + 
  theme_bw()

#Question 2 - ANOVA
# Between group variability -----------------------------------------------
#estimate overall mean of response variable (plant weight)
mu <- mean(df_growth$weight)

#estimate group means and sample size each from global/overall mean 
s_b <- df_growth %>% 
  group_by(group) %>% 
  summarize(mu_g = mean(weight), #mean for each group
            n = n()) %>% #sample size for each group
  mutate(dev_g = (mu_g - mu)^2, #squared deviation for each group
         ss_g = dev_g * n) %>% #sum across groups
  pull(ss_g) %>%
  sum()

# Within group variability ------------------------------------------------
#group level mean vs individual plant weight

s_w <- df_growth %>%
  group_by(group) %>%
  mutate(mu_g = mean(weight)) %>% #use mutate() to retain individual rows
  ungroup() %>% #dont need group structure anymore
  mutate(dev_i = (weight - mu_g)^2) %>% #deviation from group mean for each fish
  pull(dev_i) %>%
  sum()

# summed square to values -------------------------------------------------
s2_b <- s_b / (n_distinct(df_growth$group) - 1) 
s2_w <- s_w / (nrow(df_growth) - n_distinct(df_growth$group)) #to get the number of groups

# calculate F statistic ---------------------------------------------------
(f_value <- s2_b / s2_w)

f_null <- seq(0, 10, by = 0.1)
y <- df(x = f_null,
        df1 = 1, #groups minus 1
        df2 = 28) #individuals minus # of groups (this would be 30 - 2, right?)

tibble(x = f_null,
       y = y) %>%
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_vline(xintercept = f_value,
             color = "red") 

p_value <- 1 - pf(q = f_value, 
                  df1 = 1, 
                  df2 = 28) 
#p-value is 0.036 ?

#OR, using built in function: 
m <- aov(weight ~ group,
         data = df_growth) 

summary(m) #p-value is 0.0159 ?

#I think I must have done question 2 wrong because the p-value is different between my calculation and the result I got from using the built-in function


#Question 3: Which values would we report in a scientific article? 
#between-group variance and within-group variability are both used to calculate the F-statistic (4.85)
#In order to determine if our f-statistic is significant, we need to calculate the p-value under the null hypothesis (0.036 according to my calculation...?) 

#11.5.2 Power analysis 
pwr::pwr.anova.test(
  k = 3,
  f = 0.5,
  sig.level = 0.05,
  power = 0.8)
  
#N = 13.89 (so 14 would be the appropriate sample size)

