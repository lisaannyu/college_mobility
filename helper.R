library(tidyverse)
library(shiny)
library(shinydashboard)

# Parameters
# Table 1: Baaseline Cross-Sectional Estimates by College
table_2_file_path <- 
  "http://www.equality-of-opportunity.org/data/college/mrc_table2.dta"

table_2 <- haven::read_dta(table_2_file_path) %>% 
  arrange(name)

# Colors
COLOR_TIER <- "#00BFC4"
COLOR_OTHER_TIERS <- "#F8766D"
COLOR_STATE <- "#F03B20"
COLOR_OTHER_STATES <- "#FFFF00"