library(tidyverse)
library(lubridate)
library(plotly)
library(shinythemes)
library(shiny)
library(tsibble)



clean_arrival_syn <- read_csv("data/clean_arrival_syn.csv")
ad_syn <- read_csv("data/advert_synthetic.csv")
keyword_syn <- read_csv("data/keyword_synthetic.csv")

goal_completion_syn <- read_csv("data/goal_completion_synthetic.csv")
event_date_syn <- read_csv("data/event_date_synthetic.csv")

sessions_and_exits_syn <- read_csv("data/sessions_exits_synthetic.csv")

