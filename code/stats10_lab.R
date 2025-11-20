#Stats 10 lab

pacman::p_load(tidyverse,
               patchwork,
               janitor,
               palmerpenguins,
               here)


# data manipulations ------------------------------------------------------
colnames(penguins_raw)

#1 clean column names using clean_name package to convert to lowercase, replace white spaces with underscores _, and remove parentheses
penguins_clean <- clean_names(penguins_raw)


#2 convert info in clutch_completion to binary values: 1 for Yes and 0 for No. Use ifelse() function in combination with mutate().
unique(penguins_clean$clutch_completion) #check values

penguins_clean <- penguins_clean %>%
  mutate(clutch_completion = ifelse(clutch_completion == "Yes",
                                    yes = 1,
                                    no = 0))

unique(penguins_clean$clutch_completion) #check again

#3 change species name input using case_when() function. combine with mutate()
unique(penguins_clean$species)

penguins_clean <- penguins_clean %>%
  mutate(species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
                             species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo",
                             species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap"))


#4 remove rows with NA values in columns Culmen Length (mm), Culmen Depth (mm), Flipper Length (mm), Body Mass (g), and Sex using drop_na() function. specify which column we want to maniplulate
#BE CAREFUL USING NA_OMIT() FUNCTION!!! it can remove way too much data because it doesn't specify which columns we are removing NA values from!!!

penguins_clean <- penguins_clean %>%
  drop_na(culmen_length_mm, 
          culmen_depth_mm, 
          flipper_length_mm, 
          body_mass_g,
          sex)


# model selection ---------------------------------------------------------
#1 Develop a statistical model that explains Clutch Completion using variables Species, Culmen Length (mm), Culmen Depth (mm), Flipper Length (mm), Body Mass (g), and Sex. Use an appropriate probability distribution for this model.
#clutch completion is the predictor value we are examining with this model 
m_full <- glm(clutch_completion ~ species + 
                culmen_length_mm + 
                culmen_depth_mm + 
                flipper_length_mm + 
                body_mass_g + 
                sex,
              data = penguins_clean,
              family = "binomial")


#2 Perform an AIC-based model selection using the dredge() function
library(MuMIn)
options(na.action = "na.fail") #magic spell
m_set <- dredge(m_full, rank = "AIC")

m_set

subset(m_set, delta < 2) #find top candidate models 
#body mass and culmen length are both important predictors for clutch completion
