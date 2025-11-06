#Stats 08 Lab

pacman::p_load(tidyverse,
               patchwork,
               here)

url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)


#Fish Generalized Linear Model
#use poisson dist only if response variables are discrete and positive
m_fish <- glm(n_sp ~ distance + cat_area + hull_area, 
              data = df_fish,
              family = "poisson") 

summary(m_fish)


#Car Generalized Linear Model
#must use cbind function if upper limit is greater than 1
m_am <- glm(cbind(am, 1 - am) ~ mpg + hp + wt,
            data = mtcars,
            family = "binomial")
summary(m_am)
#weight is significant, heavier car less likely to be manual

#visualize
plot(am ~ wt,
     mtcars)


#gausian will get a difference result
m_am_gau <- glm(am ~ mpg + hp + wt,
            data = mtcars,
            family = "gaussian") %>%
  summary()


# Effect Size -------------------------------------------------------------
#should be easy, just develop three different models using the scale function.... use mutate function???

df_fish <- df_fish %>%
  mutate(std_dist = scale(distance),
         std_cat = scale(cat_area),
         std_hull = scale(hull_area))

m_fish_std <- glm(n_sp ~ std_dist + std_cat + std_hull,
                  data = df_fish,
                  family = "poisson")

#compare coefs
coef(m_fish)
coef(m_fish_std)



# Offset terms ------------------------------------------------------------
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_offset.csv"
df_offset <- read_csv(url)

#relationship between count and nitrate
g1 <- df_offset %>%
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point()

#relationship between count and area
g2 <- df_offset %>%
  ggplot(aes(x = count,
             y = area)) +
  geom_point()

#relationship between count/area and nitrate
g3 <- df_offset %>%
  mutate(density = count/area) %>%
  ggplot(aes(x = nitrate,
             y = density)) +
  geom_point()

g1 + g2 + g3
#density is continuous data, can take any number above zero

#gaussian trial- does not work 
df_offset <- df_offset %>% 
  mutate(density = count / area)

glm(density ~ nitrate,
    data = df_offset,
    family = "poisson")


#Develop a GLM with the response variable count and the predictor nitrate
#use offset term with poisson
m_count_wo_offset <- glm(count ~ nitrate, 
                         data = df_offset,
                         family = "poisson") 

summary(m_count_wo_offset)

#Develop a GLM with the response variable count, the predictor nitrate, and the offset term offset(log(area)) (~ nitrate + offset(log(area)) will work).
m_count_w_offset <- glm(count ~ nitrate + offset(log(area)),
             data = df_offset,
             family = "poisson") 

summary(m_count_w_offset)




# Overdispersion ----------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_tadpole.csv"
df_tadpole <- read_csv(url)

#Plot the relationships between (a) tadpole and aqveg and (b) tadpole and permanence.
tad_a <- df_tadpole %>%
  ggplot(aes(x = aqveg,
             y = tadpole)) +
  geom_point()

tad_b <- df_tadpole %>%
  ggplot(aes(x = permanence,
             y = tadpole)) + 
  geom_point()

tad_a + tad_b

#Discuss in a group that which variable may have significant influence.
#permanence has a slightly greater significant impact. tadpole is discrete with no upper limit, so poisson is best family to use


#Develop a GLM (choose an appropriate distribution) explaining tadpole with aqveg and permanence.
m_tad <- glm(tadpole ~ aqveg + permanence,
             data = df_tadpole,
             family = "poisson")


#TYPE 1 ERROR: data breaks the assumption of poisson dist, which assumes mean and variance are identical
mean(df_tadpole$tadpole)
var(df_tadpole$tadpole)

#negative binomial distribution is better to use when mean is greater
m_nb <- MASS::glm.nb(tadpole ~ aqveg + permanence,
             data = df_tadpole) #dont specify family, already set in function
summary(m_nb)
#p-value is approx 0.8, insignificant

#figure 14.6 in chapter 14 is useful for deciding which family to pick