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
library(data.table)
library(magrittr)
library(vroom)
library(tictoc)
library(vroom)

########################
#####challenge 1
col_types_assignee <- list(
  id = col_character(),
  type = col_integer(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_names  = names(col_types_assignee),
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)


col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_names  = names(col_types_patent_assignee),
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
)

col_types_location <- list(
  id = col_character(),
  city = col_character(),
  state = col_character(),
  country = col_character()
)

location_tbl <- vroom(
  file       = "location.tsv", 
  delim      = "\t", 
  col_names  = names(col_types_location),
  col_types  = col_types_location,
  na         = c("", "NA", "NULL")
)


setDT(assignee_tbl)
setDT(patent_assignee_tbl)
setDT(location_tbl)

tic()
patent_assignee_and_assignee_tbl <- dplyr::left_join(patent_assignee_tbl, assignee_tbl, by=c("assignee_id" = "id"))
toc()

tic()
patent_assignee_and_assignee_tbl <- dplyr::left_join(patent_assignee_and_assignee_tbl, location_tbl, by=c("location_id" = "id"))
toc()


patent_assignee_and_assignee_tbl[(!is.na(organization) & country == "US"), .(total_row = .N) , by = organization][order(-total_row)][1:10]
# International Business Machines Corporation has most patents \\139054//
# List the 10 US companies with the most assigned/granted patents.
# 1: International Business Machines Corporation    139054
# 2:                    General Electric Company     46866
# 3:                           Intel Corporation     42121
# 4:   Hewlett-Packard Development Company, L.P.     35531
# 5:                       Microsoft Corporation     30074
# 6:                     Micron Technology, Inc.     27990
# 7:                       QUALCOMM Incorporated     24664
# 8:              Texas Instruments Incorporated     24178
# 9:                           Xerox Corporation     23162
# 10:                                  Apple Inc.    21801

#####challenge 2
# col_types_patent <- list(
#   id = col_character(),
#   type = col_character(),
#   number = col_character(),
#   country = col_character(),
#   date = col_date("%Y-%m-%d"),
#   abstract = col_character(),
#   title = col_character(),
#   kind = col_character(),
#   num_claims = col_double(),
#   filename = col_character(),
#   withdrawn = col_double()
# )
# 
# patent_tbl <- vroom(
#   file       = "patent.tsv", 
#   delim      = "\t", 
#   col_names  = names(col_types_patent),
#   col_types  = col_types_patent,
#   na         = c("", "NA", "NULL")
# )
# 
# setDT(patent_tbl)
# 
# tic()
# patent_assignee_and_assignee_and_patent_tbl <- dplyr::left_join(patent_tbl, patent_assignee_and_assignee_tbl, by=c("id" = "patent_id"))
# toc()
# 
# patent_assignee_and_assignee_and_patent_tbl[ (year(date) == "2019" & country.y == "US" & !is.na(organization)), 
#                                             .(total_row = .N) ,
#                                             by = organization][order(-total_row)][1:10]

# International Business Machines Corporation had most patents granted in 2019
# List the top 10 companies with the most new granted patents for 2019.
# 1: International Business Machines Corporation      9262
# 2:                           Intel Corporation      3518
# 3:         Microsoft Technology Licensing, LLC      3103
# 4:                                  Apple Inc.      2809
# 5:               Ford Global Technologies, LLC      2623
# 6:                   Amazon Technologies, Inc.      2532
# 7:                       QUALCOMM Incorporated      2354
# 8:                                 Google Inc.      2286
# 9:                    General Electric Company      1857
# 10:   Hewlett-Packard Development Company, L.P.     1589


#####challenge 3
col_types_uspc <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_integer()
)

uspc_tbl <- vroom(
  file       = "uspc.tsv", 
  delim      = "\t", 
  col_names  = names(col_types_uspc),
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
)

col_types_mainclass <- list(
  id = col_character()
)

setDT(uspc_tbl)

tic()
patent_assignee_and_assignee_and_uspc <- dplyr::left_join(patent_assignee_and_assignee_tbl, uspc_tbl, by=c("patent_id" = "patent_id"))
toc()


# For the top 10 companies (worldwide) with the most patents
patent_assignee_and_assignee_and_uspc[!is.na(organization), .(total_row = .N) , by = organization][order(-total_row)][1:10]

# 1: International Business Machines Corporation    345118
# 2:               Samsung Electronics Co., Ltd.    204838
# 3:                      Canon Kabushiki Kaisha    187338
# 4:                    General Electric Company    145473
# 5:                               Hitachi, Ltd.    140677
# 6:                    Kabushiki Kaisha Toshiba    139423
# 7:                            Sony Corporation    138638
# 8:                             Fujitsu Limited    103374
# 9:                           Intel Corporation     98653
# 10:    Matsushita Electric Industrial Co., Ltd.    98038



patent_assignee_and_assignee_and_uspc[!is.na(mainclass_id), .(total_row = .N) , by = mainclass_id][order(-total_row)][1:5]

# 257 is the most innovative tech sector
# what are the top 5 USPTO tech main classes?
# mainclass_id total_row
# 1:          257    444181
# 2:          428    412856
# 3:          435    374457
# 4:          514    326452
# 5:          438    286881
