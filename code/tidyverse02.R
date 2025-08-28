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






