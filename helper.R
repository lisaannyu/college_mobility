library(tidyverse)
library(shiny)
library(shinydashboard)

# Parameters
# Table 1: Baaseline Cross-Sectional Estimates by College
table_2_file_path <- 
  "http://www.equality-of-opportunity.org/data/college/mrc_table2.dta"

table_2 <- haven::read_dta(table_2_file_path) %>% 
  arrange(name)

# Table 10: College-level Characteristics
table_10_file_path <-
  "https://github.com/lisaannyu/college_mobility/raw/master/mrc_table10.csv"
  # "/Users/lisaannyu/GitHub/college_mobility/mrc_table10.csv"

table_10 <- read_csv(table_10_file_path) %>% 
  arrange(name)

# Add some college-level characteristics to table_2
table_2 <- table_2 %>%
  left_join(table_10 %>% select(name, sat_avg_2013, sticker_price_2013, 
                                matches("pct_[A-Za-z]*_2000"),
                                ends_with("_share_fall_2000")), 
            by = "name")

# Colors
COLOR_TIER <- "#00BFC4"
COLOR_OTHER_TIERS <- "#F8766D"
COLOR_STATE <- "#F03B20"
COLOR_OTHER_STATES <- "#FFFF00"