#07 October 2025 - Stats 05
#ANOVA

pacman::p_load(tidyverse,
               patchwork,
               here)

df_anova <- read_csv(here("data_raw/data_fish_length_anova.csv"))
distinct(df_anova, lake)

#ANOVA is used to compare average between more than two groups
#Analysis of Variance (difference in mean)

#visualization helps to detect errors
#geom_violin() is function for violin plots
#geom_jitter() for jittered points

df_anova %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_violin(draw_quantiles = 0.5, #draw median horizontal line
              alpha = 0.2, #transparency
              fill = "steelblue") + #color inside violin shape
  geom_jitter(width = 0.1,
              alpha = 0.5) + #transparency
  theme_bw()
#violin plot gives us a better idea of the variation in samples


# how to perform ANOVA ----------------------------------------------------
#not recommended to use anova() function built in

#formula: y ~ x (response always on left, predictor always on right)
m <- aov(length ~ lake,
    data = df_anova) #specify data frame to pull data from

print(m) #doesn't give us enough information, so use summary function:
summary(m) #shows us p-value Pr(<F)


# Between group variability -----------------------------------------------

#estimate overall mean of response variable (fish length)
mu <- mean(df_anova$length)

#estimate group means and sample size each from global/overall mean 
s_b <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length), #mean for each group
            n = n()) %>% #sample size for each group
  mutate(dev_g = (mu_g - mu)^2, #squared deviation for each group
         ss_g = dev_g * n) %>% #sum across groups
  pull(ss_g) %>%
  sum()
            

# Within group variability ------------------------------------------------
#group level mean vs individual fish lengths

s_w <- df_anova %>%
  group_by(lake) %>%
  mutate(mu_g = mean(length)) %>% #use mutate() to retain individual rows
  ungroup() %>% #dont need group structure anymore
  mutate(dev_i = (length - mu_g)^2) %>% #deviation from group mean for each fish
  pull(dev_i) %>%
  sum()


# summed square to values -------------------------------------------------
s2_b <- s_b / (n_distinct(df_anova$lake) - 1) 
s2_w <- s_w / (nrow(df_anova) - n_distinct(df_anova$lake)) #to get the number of lakes (3)


# calculate F statistic ---------------------------------------------------
(f_value <- s2_b / s2_w)

f_null <- seq(0, 10, by = 0.1)
y <- df(x = f_null,
        df1 = 2, #lakes minus 1
        df2 = 147) #individuals minus # of groups

tibble(x = f_null,
        y = y) %>%
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_vline(xintercept = f_value,
             color = "red") 
#p-value is the area under the curve on the left side of the red line

p_value <- 1 - pf(q = f_value, 
                  df1 = 2, 
                  df2 = 147)
#calculates value of left side of red line












