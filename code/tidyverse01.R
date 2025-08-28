library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

# 5.1.1 subset rows -------------------------------------------------------
#single match done with "=="
filter(iris_sub, Species == "virginica")

#multiple matches done with "%in%"
filter(iris_sub, Species %in% c("virginica", "versicolor"))

#except done with "!="
filter(iris_sub, Species != "virginica")

#except multiple !(_____ %in% c("_____", "_____"))
filter(iris_sub, !(Species %in% c("virginica", "versicolor")))

#greater than ">"
filter(iris_sub, Sepal.Length > 5)

#greater than and equal to ">="
filter(iris_sub, Sepal.Length >= 5)

#less than "<"
filter(iris_sub, Sepal.Length < 5)

#less than or equal to "<="
filter(iris_sub, Sepal.Length <= 5)

#multiple conditions (AND) & (or ,) 
#Sepal.Length is less than 5 AND Species equals "setosa"
filter(iris_sub,
       Sepal.Length < 5 & Species == "setosa")

#other option-  "," works like "&"
filter(iris_sub,
       Sepal.Length < 5, Species == "setosa")

#Multiple conditions (OR) |
# Either Sepal.Length is less than 5 OR Species equals "setosa"
  filter(iris_sub,
         Sepal.Length < 5 | Species == "setosa")

# 5.1.2 arrange rows ------------------------------------------------------
  
#Increasing/ascending order done with "arrange"
arrange(iris_sub, Sepal.Length)
  
#Decreasing/descending order includes "desc"
arrange(iris_sub, desc(Sepal.Length))


# 5.1.3 exercise ----------------------------------------------------------
#1 Sepal.Width is greater than 3.0 and assign the new dataframe to iris_3
iris_3 <- filter(iris_sub, Sepal.Length > 3)

#2 Species is "setosa" and assign the new dataframe to iris_setosa
iris_setosa <- filter(iris_sub, Species == "setosa")

#3 Sepal.Width is greater than 3.0 AND Species is "setosa", and assign the new dataframe to iris_3_setosa
iris_3_setosa <- filter(iris_sub,
                        Sepal.Width > 3 & Species == "setosa")

#5.2.1 Column manipulation------------------------------------------------------

#select multiple columns
select(iris_sub, c(Sepal.Length, Sepal.Width))

#remove one column 
select(iris_sub, -Sepal.Length)

#remove multiple columns 
select(iris_sub, -c(Sepal.Length, Sepal.Width))

#Select/Remove with "starts_with()"
  #select columns starting with "Sepal"
select(iris_sub, starts_with("Sepal"))

  #remove columns starting with "Sepal"
select(iris_sub, -starts_with("Sepal"))

#Select with "ends_with()"
  #select columns ending with "Sepal"
select(iris_sub, ends_with("Width"))

  #remove columns ending with "Sepal"
select(iris_sub, -ends_with("Width"))

#5.2.2 Add columns--------------------------------------------------------------
#nrow() returns the number of rows of the dataframe
(x_max <- nrow(iris_sub))

#modify existing columns 
  #twice `Sepal.Length` and add as a new column
mutate(iris_sub, sl_two_times = 2 * Sepal.Length)



#5.3.1 Piping 
#might want to pick one column and then add another one; might want to do multiple manipulations for one data frame 

df_sl <- select(iris_sub, Sepal.Length)
df_sl_2times <- mutate(df_sl, twice = 2 * Sepal.Length)

iris_sub %>% 
  select(Sepal.Length)
#when piping, skip the first thing you want to subset because it will be the data set you are piping from..... ?

iris_sub %>% 
  select(Sepal.Length) %>% 
  mutate(twice = 2 * Sepal.Length)
  