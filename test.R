roll <- function(faces = 1:6, number_of_dice = 1) {
  dice <- sample(x = faces, size = number_of_dice, 
                 replace = TRUE, 
                 prob = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5))
  sum(dice)
}

# You can run the function 100 times, store the results and plot a histogram to verify your function
results <- replicate(n = 100, expr = roll(), simplify=TRUE)
hist(results)


library("tidyverse")
# Alternatively, load just magrittr:
library("magrittr")

library("googledrive")

library("haven")
library("readxl")
library(lubridate)


iris %>% head(n=3)

tibble(
  x = 1:50,
  y = runif(50), 
  z = x + y^2,
  outcome = rnorm(50)
)

class(cars)
## "data.frame"

cars_tbl <- as_tibble(cars)
class(cars_tbl)


# This way applies to dataframes and tibbles
vehicles <- as_tibble(cars[1:5,])
vehicles[['speed']]
vehicles[[1]]
vehicles$speed

# Using placeholders with the pipe
vehicles %>% .$dist
vehicles %>% .[['dist']]
vehicles %>% .[[2]]



library(tidyverse)
diamonds2 <- readRDS("diamonds2.rds")

diamonds2 %>% head(n = 5)

diamonds2 %>% 
  pivot_longer(cols      = c("2008", "2009"), 
               names_to  = 'year', 
               values_to = 'price') %>% 
  head(n = 50)


tibble(
  x = 1:50,
  y = runif(50), 
  z = x + y^2,
  outcome = rnorm(50)
)

model <- lm(price ~ ., data = diamonds2_long)
model

diamonds3 <- readRDS("diamonds3.rds")

diamonds3 %>% head(n = 5)

diamonds3 %>% 
  pivot_wider(names_from  = "dimension",
              values_from = "measurement") %>% 
  head(n = 5)

diamonds4 <- readRDS("diamonds4.rds")

diamonds4

diamonds4 %>% 
  separate(col = dim,
           into = c("x", "y", "z"),
           sep = "/",
           convert = T)

diamonds5 <- readRDS("diamonds5.rds")

diamonds5

diamonds5 %>% 
  unite(clarity, clarity_prefix, clarity_suffix, sep = '')

library(ggplot2) # To load the diamonds dataset
library(dplyr)
diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  head(10)

diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  slice(3:3)

diamonds %>% 
  arrange(cut, carat, desc(price))

diamonds %>% 
  select(color, clarity, table:z) %>% 
  head(n = 5)

diamonds %>% 
  select(-(x:z)) %>% 
  head(n = 5)

diamonds %>% 
  select(x:z, everything()) %>% 
  head(n = 5)

diamonds %>% 
  rename(var_x = x) %>% 
  head(n = 5)

diamonds %>% 
  mutate(p = x + z, q = p + y) %>% 
  select(-(depth:price)) %>% 
  head(n = 5)

diamonds %>% 
  transmute(carat, cut, sum = x + y + z) %>% 
  head(n = 5)


diamonds %>% 
  group_by(cut) %>% 
  summarize(max_price  = max(price),
            mean_price = mean(price),
            min_price  = min(price))

glimpse(diamonds)


ymd(20101215)
## "2010-12-15"
mdy("4/1/17")

bday <- dmy("14/10/1979")
month(bday)
## 10

year(bday)
## 1979

day(bday)
