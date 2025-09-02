if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)


# refresher ---------------------------------------------------------------
#exercise 1
# filter iris_sub to those with Sepal.length greater than 5
#assign to 'df_gs"

df_g5 <- filter(iris_sub, Sepal.Length > 5)
#or
df_g5 <- iris_sub %>%
  filter(Sepal.Length > 5)
 
#exercise 2 
#select columns of Sepal.Length and Petal.Width from iris_sub
#assign df_sp 

df_sp <- select(iris_sub, c(Sepal.Length, Petal.Width))

#exercise 3
#arrange rows by petal-width in iris_sub 
#assign to df_arrange

df_arrange <- arrange(iris_sub, Petal.Width)
#OR
df_arrange <- iris_sub %>% 
  arrange(Petal.Width)

#exercise 4
#do 1-3 to once with pipes
#assign to df_master

df_master <- df_gs <- filter(iris_sub, Sepal.Length > 5) %>%
      select(c(Sepal.Length, Petal.Width)) %>%
      arrange(Petal.Width)

#extra 
#calculate mean Petal.Width for each species separately 
#use group_by() and summarize() functions 

iris_sub %>% 
  group_by(Species) %>%
  summarize(mean_pw = mean(Petal.Width))


# ggplot  -----------------------------------------------------------------
#basic syntax without pipe
g_example <- ggplot(data = iris,
      mapping = aes(x = Sepal.Length,
                    y = Sepal.Width)) +
      geom_point()
#first argument is the dataset you want to work with
#second argument often "mapping"

#with pipe
iris %>%
  ggplot(mapping = aes(x = Sepal.Length,
                       y = Sepal.Width)) +
  geom_point()

#color!!! <3
iris %>%
  ggplot(mapping = aes(x = Sepal.Length,
                       y = Sepal.Width,
                       color = Species)) +
  geom_point()

#pitfall, when you color points or anything 
# iris %>%
#  ggplot(mapping = aes(x = Sepal.Length,
#                       y = Sepal.Width),
#                       color = Species) +
#   geom_point()
  
g_col <- iris_sub %>%
  ggplot(mapping = aes(x = Sepal.Length,
                       y = Sepal.Width)) +
  geom_point(color = "salmon")


# line plot ---------------------------------------------------------------
df0 <- tibble(x = rep(1:50, 3),
              y = x * 2)
df0 %>%
  ggplot(aes(x = x,
             y = y)) +
  geom_line()


# histogram ---------------------------------------------------------------
# basic plot; bins = 30 by default
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram()

#create a historgram colored by species
iris %>% 
  ggplot(aes(x = Sepal.Length, 
             color = Species)) +
  geom_histogram()

#fill by species color 
iris %>% 
  ggplot(aes(x = Sepal.Length, 
             fill = Species)) +
  geom_histogram()



# boxplot -----------------------------------------------------------------
# basic plot
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length)) +
  geom_boxplot()

#boxplot filled by species 
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot()


#use multiple layers to plot different types of info 
iris %>%
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.5)

#other
iris %>%
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot() +
  
  
  
  
#data-to-viz.com
  



