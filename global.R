library(shiny)
library(bslib)
library(tidyverse)
# library(plotly)

psy <- read_csv("../data_appended/psy_4a.csv") %>%
  mutate(month = lubridate::month(dt))
