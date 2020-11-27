
library(tidyverse)
library(lubridate)
library(data.table)
library(vroom)
library(tictoc)
library(vroom)
library(ggplot2)
library(ggrepel)
library(maps)



url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)

class(covid_data_dt)

colnames(covid_data_dt)
setnames(covid_data_dt, "dateRep", "date")
setnames(covid_data_dt, "countriesAndTerritories", "country")
setnames(covid_data_dt, "continentExp", "continent")


##### challenge 1
data <- covid_data_dt[(country == "United_States_of_America" | 
                         country == "Germany" | 
                         country == "United_Kingdom" | 
                         country == "France" | 
                         country == "Spain") & year == "2020",
                      .(total_cases = sum(cases)) , by = .(country, month, year)][order(-country, month, year)]
data <- data %>% group_by(country) %>% 
  mutate(accumulate_sum = cumsum(total_cases)) %>% 
  mutate(month = lubridate::month(month, label = T, abbr = F))

class(data)
setDT(data)

data %>%
  ggplot(aes(month, accumulate_sum, group=country, color = country)) +
  geom_line() + 
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "As of 11/02/2020, Europe had more cases than USA",
    x = "Year 2020",
    y = "Cumulative Cases"
  ) +
  theme (
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.background  = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"),
    legend.title = element_text(size = 15),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic"),
    panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.background = element_rect(fill = "#BFD5E3")
  ) + 
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                    preix = "",
                                                    suffix = "M")) +
  geom_label_repel (
    aes(label = accumulate_sum), data = data[month == max(month)],
    size  = 4,
    nudge_x      = -1,
    direction    = "y",
    segment.size = 0.2
  )

##### challenge 2
world <- map_data("world")

geo_data <- covid_data_dt[year == "2020"]


geo_data <- geo_data %>% 
  mutate(country = case_when(
    country == "United_Kingdom" ~ "UK",
    country == "United_States_of_America" ~ "USA",
    country == "Czechia" ~ "Czech Republic",
    country == "Bonaire, Saint Eustatius and Saba" ~ "Bonaire",
    TRUE ~ country
    
  ))

geo_data <- geo_data[,.(total_death = sum(deaths),population = mean(popData2019)), by = .(country)]
geo_data <- geo_data[,.(country_new = str_replace_all(country, "_", " ") , Mortality_Rate = (total_death / population) * 100), ]


world <- dplyr::left_join(world, geo_data, by=c("region" = "country_new"))


world %>%
ggplot(aes(map_id = region)) +
  geom_map(aes(fill = Mortality_Rate), map = world, color = "white") +
  expand_limits(x = world$long, y = world$lat) +
  scale_fill_gradient(high = "#541e2b", low = "#ff8282", na.value = "grey50",  guide = "colorbar")








