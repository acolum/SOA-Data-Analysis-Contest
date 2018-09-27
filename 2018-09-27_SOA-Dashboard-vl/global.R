library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(plotly)

salesdata <- readRDS("SOAdata_vl.rds")
salesdata <- salesdata %>%
  mutate_if(is.character, factor)