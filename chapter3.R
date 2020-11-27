library(RSQLite)
library(tidyverse)
library(readxl)
library(lubridate)
library(glue)
library(httr)
library(jsonlite)
library(rvest)
library(stringr)
library(purrr)
library(stringi)
library(xopen) 
library(furrr) 
library(RSelenium)


################################################################################################
###############################################################################################
##challenge 1
uni_api <- function(path) {
  url <- glue('http://universities.hipolabs.com/search?country={path}')
  resp <- fromJSON(url)
}

resp <- uni_api("germany")

uni_data_tbl <- as_tibble(resp) 
uni_data_tbl <- uni_data_tbl %>% 
  select(name, country)
uni_data_tbl

uni_data_tbl <- tibble()
################################################################################################
###############################################################################################
##challenge 2

url  <- "https://www.rosebikes.de/fahrr%C3%A4der/rennrad"
html <- url %>% 
  read_html()

price <-  html %>% 
  html_nodes(css = ".catalog-category-bikes__price-title") %>% 
  html_text() %>%
  stringr::str_replace_all(pattern = ",|\\.|(\u20AC)|\\n|ab| ", replacement = "")
  

title <-  html %>% 
  html_nodes(css = ".catalog-category-bikes__title-text") %>% 
  html_text() %>%
  stringr::str_replace_all(pattern = "\\n", replacement = "")


rosebikes_bike_tbl <- tibble(title, price)
rosebikes_bike_tbl

saveRDS(rosebikes_bike_tbl, "rosebikes_bike_tbl.rds")

