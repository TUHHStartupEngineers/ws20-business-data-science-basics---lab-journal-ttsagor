library(tidyverse)
library(readxl)
library(lubridate)
library(writexl)
library(lubridate)

bikes_tbl      <- read_excel(path = "C:/Users/sagor/Documents/GitHub/ws20-business-data-science-basics---lab-journal-ttsagor/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("C:/Users/sagor/Documents/GitHub/ws20-business-data-science-basics---lab-journal-ttsagor/DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")

bikeshops_tbl  <- read_excel("C:/Users/sagor/Documents/GitHub/ws20-business-data-science-basics---lab-journal-ttsagor/DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  mutate(total.price = price * quantity) %>%
  select(-...1, -gender) %>%
  select(-ends_with(".id")) %>%
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))




#Challenge 1
new_bike_orderlines_wrangled_tbl <- bike_orderlines_wrangled_tbl %>%
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ")

sales_by_state_tbl <- new_bike_orderlines_wrangled_tbl %>%
  select(state, total_price) %>%
  group_by(state) %>% 
  summarize(sales = sum(total_price))
sales_by_state_tbl # North Rhine-Westphalia has the highest revenue

sales_by_state_tbl %>%
  ggplot(aes(x = state, y = sales)) +
  geom_col(fill = "#2DC6D6") + 
  geom_label(aes(label = sales)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by State",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

bike_orderlines_wrangled_tbl %>%
  write_xlsx("C:/Users/sagor/Documents/GitHub/ws20-business-data-science-basics---lab-journal-ttsagor/DS_101/00_data/01_bike_sales/02_wrangled_data/state_bike_orderlines.xlsx")

bike_orderlines_wrangled_tbl %>% 
  write_csv("C:/Users/sagor/Documents/GitHub/ws20-business-data-science-basics---lab-journal-ttsagor/DS_101/00_data/01_bike_sales/02_wrangled_data/state_bike_orderlines.csv")

bike_orderlines_wrangled_tbl %>% 
  write_rds("C:/Users/sagor/Documents/GitHub/ws20-business-data-science-basics---lab-journal-ttsagor/DS_101/00_data/01_bike_sales/02_wrangled_data/state_bike_orderlines.rds")


#challenge 2
sales_by_year_state_tbl <- new_bike_orderlines_wrangled_tbl %>%
  
  select(order_date, total_price, state) %>%
  mutate(year = year(order_date)) %>%
  group_by(year, state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_state_tbl  

sales_by_year_state_tbl %>%
  ggplot(aes(x = year, y = sales, fill = state)) +
  geom_col() +
  facet_wrap(~ state) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and state",
    subtitle = "Each state has an upward trend",
    fill = "States" # Changes the legend name
  ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
