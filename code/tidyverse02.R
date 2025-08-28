#28 August 2025
library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)


# group operation ---------------------------------------------------------

#combine group_by function with summarize function 
df_m_sd <- iris_sub %>%
  group_by(Species) %>%
  summarize(mean_sl = mean(Sepal.Length),
            sd = sd(Sepal.Length))
  
#will usually use a character vector in those parenthesis following "mean"
#will usually combine with summarize or mutate when using group_by function 

#combine group_by function with mutate 
  #to see how individual column might deviate from the rest of the group mean
df_eps <- iris_sub %>%
  group_by(Species) %>%
  mutate(mean_sl = mean(Sepal.Length)) %>%
  ungroup() %>%
  mutate(eps = abs(Sepal.Length - mean_sl))
#ungroup function will prevent confusing errors in the future if you arent being careful with the mutate function


# reshape -----------------------------------------------------------------
#this function just lets you reorganize the dataframe to view in a different arrangement. Might not always be useful. Wide format not always the best format for data organization. Akira suggests using long format bc you wont see what the values actually mean if using wide. 


iris_w <- iris_sub %>% 
  mutate(id = rep(1:3, 3)) %>% # add an ID column
  select(id, Sepal.Length, Species) %>% 
  pivot_wider(id_cols = "id", # unique row ID based on
              values_from = "Sepal.Length", # values in each cell from
              names_from = "Species") # new column names from

#pivot longer to change to long format
 iris_l <- iris_w %>%
   pivot_longer(cols = c("setosa", "versicolor", "virginica"), 
                names_to = "Species",
                values_to = "Sepal.Length")

