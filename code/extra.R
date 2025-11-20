#November 20 2025

#data cleaning tips in R
pacman::p_load(tidyverse,
               patchwork,
               janitor,
               stringdist,
               here)

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_messy.csv"
df_messy <- read_csv(url)


#check data entries before you analyze data ----------------------------

#check class of each column - sapply & class
sapply(df_messy,
       FUN = class) #check the data type

#check unique elements in each column - sapply & unique
sapply(df_messy,
       FUN = unique)

#check possible type - stringdistmatrix()
collector <- unique(df_messy$collector)
stringdistmatrix(collector)

# text cleaning in R --------------------------------------------------

#remove white space - str_squish()
a <- c("a ", "a  b", "b ", "a", "b")
str_squish(a)

#align text case - str_to_lower() & str_to_upper()
b <- c("A", "a", "bB", "BB")
str_to_lower(b) #makes everything lowercase
str_to_upper(b) #makes everything uppercase

#replace text - str_replace() & str_replace_all()
v <- c("a b", "a.b", "a b.c")
str_replace(v, "\\s", "_") #first quotations is the characters used to represent a blank space. in the second quotations, specify what character to replace with

str_replace(v, "\\s", "_") %>%
  str_replace_all("\\.", "_")

#this one only replaces the first instance
str_replace(v, "\\s|\\.", "_")

#this function replaces all of them - probably more useful 
str_replace_all(v, "\\s|\\.", "_")

#remove text - str_remove() & str_remove_all()
x <- c("abc", "dd", "abd")
str_remove_all(x, "ab")

#extract text - str_extract
z <- c("abc", "dd", "abd")
str_extract(z, "ab")

#detect text - str_detect()
  #useful in combination with filter() or ifelse()
str_detect(z, "a")
ifelse(str_detect(z, "a"),
       yes = 1,
       no = 0)


# date object -------------------------------------------------------------
d <- c("2024/06/01", "June 4 2024")
lubridate::parse_date_time(d,
                           tz = "EST",
                           orders = c("Y/m/d",
                                      "B d Y"))

#full year is capital Y, last two digits in lowercase y
#string month is capital B

y <- c("ab", "Ab")
str_detect(y, "^[Aa]")


# clean data --------------------------------------------------------------

#clean each column one by one
#collector
df_messy %>%
  mutate(collector = str_to_lower(collector),
         species = str_squish(species) %>% 
           str_to_lower() %>%
           str_replace_all("\\s|\\.", "_") %>%
           str_remove("_$"),
          length_mm = str_replace(length_mm,
                                   ",",
                                   ".") %>%
           str_remove("\\smm") %>%
           as.numeric(),
         sample_date = parse_date_time(sample_date,
                                       tz = "EST",
                                       orders = c("Y/m/d",
                                                  "B d Y",
                                                  "d B Y",
                                                  "Y.m.d")) %>%
           as.Date(),
         recaptured = ifelse(str_detect(recaptured, "^[Yy]"),
                             yes = 1,
                             no = 0))

  

#underscore at the end of the string is dollar sign
#^ specifies the beginning of the string





# column manipulation based on column type --------------------------------
#mutate(), across(), where()

# example code ------------------------------------------------------------

chr_clean <- function(x) {
  x %>%
     str_squish() %>% 
     str_to_lower() %>% 
     str_replace_all("\\.|\\s", "_") %>% 
     str_remove_all("^_|_$")
 }
# 
# df_messy %>% 
#   mutate(collector = chr_clean(collector),
#          species = chr_clean(species),
#          length_mm = str_squish(length_mm) %>% 
#            str_replace(",", "\\.") %>% 
#            str_extract("\\d{1,}") %>% 
#            as.numeric(),
#          sample_date = parse_date_time(sample_date,
#                                        tz = "EST",
#                                        order = c("Y/m/d",
#                                                  "B d Y",
#                                                  "d B Y")),
#          recaptured = str_to_lower(recaptured) %>% 
#            str_sub(start = 1L,
#                    end = 1L)
#   )








#check data entries before you analyze data

#text cleaning in  R

#coumn manipulation based on column type