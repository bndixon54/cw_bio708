# Submitting this assessment by the due date is worth 50 points.
# Each question is worth 5 points.
# Call suitable packages as needed.

pacman::p_load(tidyverse,
               patchwork,
               janitor,
               here)


# CO2 dataset -------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# The CO2 dataset in R records how grass plants from two origins, Quebec and
# Mississippi, respond to varying CO₂ concentrations and temperature treatments.

head(CO2)

# Each plant is identified by the `Plant` factor, with `Type` indicating its origin
# and `Treatment` showing whether it was chilled or nonchilled. `conc` gives the
# ambient CO₂ concentration (mL/L), and `uptake` measures the rate of CO₂
# assimilation (µmol/m²/sec). 

## DATA DESCRIPTION END ###################################################

# Q1
# CO2 dataframe is a base dataframe. Convert this to a class `tibble`
# then assign to `df_co2`
df_co2 <- as_tibble(CO2)

# Q2
# Convert column names to lowercase and reassign to `df_co2`
df_co2 <- clean_names(df_co2)

# Q3
# Create scatter plots of CO₂ uptake versus ambient CO₂ concentration using `df_co2`.
# - The x-axis should represent ambient CO₂ concentration
# - The y-axis should represent CO₂ assimilation rate
# - Color the points by treatment
# - Create separate panels for each plant type (Quebec vs Mississippi) and combine the plots
miss <- df_co2 %>%
  filter(type == "Mississippi")  


queb <- df_co2 %>%
  filter(type == "Quebec")


Mississippi <- ggplot(data = miss,
                      mapping = aes(x = conc,
                                    y = uptake,
                                    color = treatment)) + 
                    geom_point() +
                    labs(title = "Mississippi",
                         x = "CO2 concentration",
                         y = "CO2 assimilation rate") +
                    theme_classic() 

print(Mississippi)


Quebec <- ggplot(data = queb,
                 mapping = aes(x = conc,
                               y = uptake,
                               color = treatment)) + 
                geom_point() +
                labs(title = "Quebec",
                     x = "CO2 concentration",
                     y = "CO2 assimilation rate") +
                theme_classic() 

print(Quebec)


Mississippi + Quebec + plot_layout(guides = "collect") #I wanted to get rid of the key from one of the panels

# Q4
# Develop suitable statistical models to examine:
#   
# - The main effect of ambient CO₂ concentration (conc)
(CO2_m_c <- glm(uptake ~ conc,
             data = miss,
             family = "poisson"))

(CO2_q_c <- glm(uptake ~ conc,
             data = queb,
             family = "poisson"))

# - The main effect of treatment
(CO2_m_t <- glm(uptake ~ treatment,
             data = miss,
             family = "poisson"))

(CO2_q_t <- glm(uptake ~ treatment,
             data = queb,
             family = "poisson"))

# - The interaction between concentration and treatment
#for Mississippi orgin
(CO2_m_x <- glm(uptake ~ conc + treatment,
             data = miss,
             family = "poisson"))

summary(CO2_m_x)

#for Quebec orgin
(CO2_q_x <- glm(uptake ~ conc + treatment,
             data = queb,
             family = "poisson"))

summary(CO2_q_x)

# Q5
# Based on the models fitted in Q4 for Quebec and Mississippi plants, describe how CO2 assimilation rate responded to ambient CO2 concentration under different treatments (chilled vs non-chilled) for each plant origin. 
#Highlight the differences between Quebec and Mississippi plants, and use the model results to support your answers.
# ENTER YOUR ANSWER HERE as COMMENT:

#Initially I was just using the lm() function to make my models, but that didn't seem right, so I tried switching to the glm() function. If I use "poisson" as the family, I get error messages for each row. This makes me think that I should use binomial and convert the treatment type to 1 for chilled and 0 for non-cilled, but that doesnt seem quite right either. I procrastinated on starting this (stupid I know, I had all weekend to start it), but now I am running out of time so I am going to move on. 

#moving forward and coming back to this has me thinking that using lm() to begin with was correct, but I am getting so confused. 



# BCI data ----------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# BCI dataset:
# run the following code to get data.
if(!require(vegan)) install.packages("vegan")
library(vegan)
data("BCI")
data("BCI.env")

# BCI dataset:
# The BCI dataset contains tree species abundance data from 50 1-hectare plots on Barro Colorado Island (Panama). Each row represents a plot, and each column represents a tree species. Entries are counts of individuals of each species in that plot. This dataset is often used to study species richness, community composition, and diversity patterns in tropical forests.

print(BCI)

# The following code transforms the BCI dataset from wide to long format. 
# Originally, each plot was a row and each species a column. 
# After the transformation, each row represents a single species in a 
# single plot, with columns indicating the plot (plot), species, and 
# the corresponding count.

cnm <- colnames(BCI)

df_bci <- BCI %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  pivot_longer(cols = cnm[1]:cnm[length(cnm)], 
               names_to = "species", 
               values_to = "count")

# BCI.env dataset:
# This dataset contains environmental variables for the 50 plots in the BCI dataset.
# Key columns include:
# - UTM.EW, UTM.NS: spatial coordinates of each plot
# - Precipitation: mean annual rainfall (mm)
# - Elevation: plot elevation (m)
# - Age.cat: categorical forest age class
# - Geology: underlying geological formation type
# - Habitat: dominant habitat type in the plot
# - Stream: indicates presence of streamside (riparian) habitat
# - EnvHet: environmental heterogeneity (Simpson diversity of habitat subcells)
# These variables help explain variation in species composition and abundance 
# across plots and allow exploration of species–environment relationships.

print(BCI.env)

# The following code adds a new "plot" column.

df_env <- BCI.env %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  relocate(plot)

## DATA DESCRIPTION END ####################################################

# Q6
# Convert column names of `df_env` to lowercase and reassign to `df_env`
df_env <- clean_names(df_env)


# Q7
# In `df_env`, some environmental variables have no variation between plots
# (i.e., the same value for all plots). Identify these columns and remove them
# from the dataframe. Assign the resulting dataframe to `df_env_sub`.

df_env_sub <- subset(df_env, select = -c(precipitation,
                                         age_cat,
                                         elevation,
                                         geology))



# Q8
# Calculate summary statistics for each plot using `df_bci`.
# For each plot, compute:
# - n_sum: total count of all individuals across species
# - n1: count of the most dominant species (maximum count among species)
# - p: proportion of the most abundant species (n1 / n_sum)
# Assign the resulting dataframe to `df_n`.
df_n <- df_bci %>%
  summarise(n_sum = sum(count), #total individuals
            n1 = max(count), #max count among species
            p = n1 / n_sum) 


# Q9
# Combine the summary data (`df_n`) with the environmental variables
# (`df_env_sub`) for each plot. Assign the resulting dataframe to `df_m`.
df_m <- merge(df_n, df_env_sub)


# Q10
# Develop a statistical model to explain variation in the proportion of the dominant
# species in each plot. Use `EnvHet`, `Stream`, and `Habitat` as predictors.
# Fit a suitable statistical model to the data.
# Use model selection based on predictability (i.e., out-of-sample prediction) 
# rather than the goodness of fit, and report which variables are included in 
# the best predictive model as a comment.
m_var <- glm(p ~ env_het + 
                stream + 
                habitat,
              data = df_m,
              family = "poisson")
#something here isnt right, because the proportion category is the same for each plot, and the max species count (247) is now applied to each row. 
#also, I am getting the same issue with warnings on each row if I use poisson as the family, though I would think this is the right family to use here because the variables contain non-discrete values. 


#2 Perform an AIC-based model selection using the dredge() function
library(MuMIn)
options(na.action = "na.fail") #magic spell
m_set <- dredge(m_var, rank = "AIC")

m_set

subset(m_set, delta < 2) #find top candidate models 
#referencing previous calsses, I think this is how I would do this, but my previous work isn't correct yet, so I cannot get the results. 




#this test just kicked my ass and I still cannot figure out if I am on the right track with question 5. Not sure if you are allowing corrections on this one or not, but I'll do my best if it is an option. Thanks for the good semester!! 
