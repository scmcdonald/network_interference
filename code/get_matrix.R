rm(list=ls())


library(here)
library(tidyverse)
library(lubridate)
library(MASS, exclude = "select")
library(vars)
library(BigVAR, exclude = "filter")
library(RColorBrewer)

cb_colors <- brewer.pal(n = 8, name = "Dark2")

raw_df <- read.csv(here("data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")) %>%
  select(submission_date, state, new_case)%>%
  mutate(new_case = ifelse(new_case <0, 0, new_case)) %>%
  mutate(new_case = log(new_case + 1))%>%
  mutate(date = mdy(submission_date)) %>%
  select(date, state, cases = new_case)

df <- raw_df %>%
  pivot_wider(names_from = state, values_from = cases) 


### BIGVAR

prediction_dates <- as.Date("2021-10-07")
states <- sort(colnames(df)[colnames(df) != "date"])
final <- data.frame()
data = df
lag = 5
h = 7
recursive = T
methods <- c("HLAGC")


j <- as.Date("2021-10-07")
  # get prediction date, start and end dates of actual data
  actual_end = j - h
  actual_start = actual_end - 365
  train_dates <- seq(actual_start, actual_end, 1)
  
  # subset actual data for training var
  train <- data %>%
    filter(date %in% train_dates) %>%
    arrange(date) %>%
    select(date, sort(colnames(data)[colnames(data) != "date"])) %>%
    column_to_rownames("date") %>% 
    as.matrix()
  
  predictions <- data.frame(date = j, state = states)
  
  i = "HLAGC"
    
    mod <-constructModel(train,
                         gran = c(50, 10),
                         h = h, 
                         p = lag,
                         struct = i,
                         recursive = recursive,
                         verbose=TRUE,
                         IC=TRUE,
                         model.controls=list(intercept=TRUE), 
                         dates = as.character(train_dates))
    
    result <- cv.BigVAR(mod)
    
    coefficients <- coef(result)
    
    rownames(coefficients) <- states
    
    colnames(coefficients) <- c("intercept", paste(rep(states, 5), rep(c("L1", "L2", "L3", "L4", "L5"), each = 60), sep = ""))
    
    SparsityPlot.BigVAR.results(result)
    
    
    va <- coefficients["VA", ]
    va_df <- as.data.frame(t(va))
    
    
    
