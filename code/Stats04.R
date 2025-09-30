#30 September 2025

#difference in mean AND variance in data 

pacman::p_load(tidyverse,
               patchwork,
               here)


df_fl <- read_csv(here("data_raw/data_fish_length.csv"))
print(df_fl)

#unique function shows unique values as a vector
unique(df_fl$lake)

#distinct function shows unique values as a tibble
distinct(df_fl, lake)

#visualize 
#plot mean, standard deviation, and then overlay 
df_fl_mu <- df_fl %>% 
  group_by(lake) %>%
  summarize(mu_l = mean(length), #summarize by mean()
            sd_l = sd(length)) #summarize with sd()

df_fl %>%
  ggplot(aes(x = lake,
             y = length)) +
  geom_jitter(width = 0.1, #width how wide you scatter the data points
              alpha = 0.25) +
  geom_segment(data = df_fl_mu,
               aes(x = lake,
                   xend = lake, #defines starting and ending points
                   y = mu_l - sd_l,
                   yend = mu_l + sd_l)) +
  geom_point(data = df_fl_mu, #plots the mean
             aes(x = lake,
                 y = mu_l)) +
  labs(x = "Lake",
       y = "Fish length") 



# t-test in R -------------------------------------------------------------
x <- df_fl %>%
  filter(lake == "a") %>%  #subset lake a to create a vector
  pull(length) #pull function gets vector instead of tibble!

y <- df_fl %>%
  filter(lake == "b") %>%
  pull(length)

t.test(x, y, var.equal = TRUE) #will use FALSE in most cases



# t-statistic -------------------------------------------------------------

mu_x <- mean(x)
mu_y <- mean(y)
mu_x - mu_y #difference between each vector; average difference 


#get the group mean, variance, and sample size
df_t <- df_fl %>% 
  group_by(lake) %>% #grouping by lake a and b
  summarize(mu_l = mean(length), #summarize by mean length
            var_l = var(length), #summarize by sd length
            n = n()) #counts number of rows per group

print(df_t)


v_mu <- df_t %>%
  pull(mu_l)

v_var <- df_t %>%
  pull(var_l)

v_n <- df_t %>% 
  pull(n) #gets the samples size for each group



var_a <- ((v_n[1] - 1) / (v_n[1] + v_n[2] - 2)) * v_var[1]
#first samples size is the first element in the vector, which is lake a

var_b <- ((v_n[2] - 1) / (v_n[2] + v_n[2] - 2)) * v_var[2]
#second sample

var_p <- var_a + var_b 
#pooled variance 

#get the t-statistic
#numerator:
v_mu[1] - v_mu[2]

#denominator: 
sqrt(var_p * ((1 / v_n[1]) + (1 / v_n[2])))

#together: 
t_value <-c(v_mu[1] - v_mu[2]) / sqrt(var_p * ((1 / v_n[1]) + (1 / v_n[2])))




# null distribution - student t distribution ------------------------------
#null hypothesis is always that the two groups (and their means) are the same; there is no difference 

# produce 500 values from -5 to 5 with equal interval
x <- seq(-5, 5, length = 500)

# probability density of t-statistics with df = sum(v_n) - 2
y <- dt(x, df = 98)

#figure
tibble(x, y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  labs(y = "Probability density",
       x = "t-statistic") +
  geom_vline(xintercept = t_value) +
  geom_vline(xintercept = abs(t_value))


#calculate the area under the curve from -infinity to t_value
p_lower <- pt(q = t_value, df = 98)

#area under the curve from absolute value to infinity
p_higher <- 1 - pt(q = abs(t_value), df = 98)

#p value
p_value <- p_lower + p_higher
print(p_value)


