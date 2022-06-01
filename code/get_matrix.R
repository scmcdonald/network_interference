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

prediction_dates <- as.Date("2021-10-15")
states <- sort(colnames(df)[colnames(df) != "date"])
final <- data.frame()
data = df
lag = 10
h = 7
recursive = T
methods <- c("HLAGC")


j <- as.Date("2021-10-15")
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
    
    colnames(coefficients) <- c("intercept", paste(rep(states, 10), rep(c("L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9", "L10"), each = 60), sep = ""))
    
    
    #saveRDS(coefficients, file = "data/coefficients/coef_10_15_2021_L10_HLAGC.rds")
    coef_matrix <- readRDS("data/coefficients/coef_10_15_2021_L10_HLAGC.rds")
    
    SparsityPlot.BigVAR.results(result)
    
    
    va <- coefficients["VA", ]
    va_df <- as.data.frame(t(va))
    
    
    coefs <- readRDS(file = "data/coefficients/coef_10_15_2021_L10_HLAGC.rds")
    
    coefs_no_intercept <- coefs[, -1]
    
    coefs_threshold <- coefs_no_intercept
    coefs_threshold[abs(coefs_threshold) < 0.05] <- NA
    

    
    coefs_threshold[, grep(pattern = "L1$", colnames(coefs_threshold))] %>%
      as.matrix() %>%
      melt() %>%
      rename(state = Var1, lag1 = Var2) %>%
      mutate(lag1 = str_remove(lag1, "L1")) %>%
      ggplot(aes(x = lag1, y = state)) +
      geom_tile(aes(fill = value), color = "white") +
      scale_fill_gradient2(low = cb_colors[2],
                           mid = "white",
                           high = cb_colors[1], 
                           na.value = "grey70"
                          ) +
      theme_minimal() + 
      labs(y = "State", x = "L1", caption = "Threshold: 0.05")
    
   t <-  coefs_threshold[, grep(pattern = "L2$", colnames(coefs_threshold))] %>%
      as.matrix() %>%
      melt() %>%
      rename(state = Var1, lag1 = Var2) %>%
      mutate(lag1 = str_remove(lag1, "L2")) %>%
      ggplot(aes(x = lag1, y = state)) +
      geom_tile(aes(fill = value), color = "white") +
      scale_fill_gradient2(low = cb_colors[2],
                           mid = "white",
                           high = cb_colors[1], 
                           na.value = "grey70"
      ) +
      theme_minimal() + 
      labs(y = "State", x = "L1", caption = "Threshold: 0.05")  



   ggplotly(t) 
    