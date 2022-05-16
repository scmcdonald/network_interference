rm(list=ls())

library(here)
library(tidyverse)
library(lubridate)
library(MASS, exclude = "select")
library(vars)
library(BigVAR)

raw_df <- read.csv(here("data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")) %>%
  select(submission_date, state, new_case)%>%
  mutate(new_case = ifelse(new_case <0, 0, new_case)) %>%
  mutate(new_case = log(new_case + 1))%>%
  mutate(date = mdy(submission_date)) %>%
  select(date, state, cases = new_case)

df <- raw_df %>%
  pivot_wider(names_from = state, values_from = cases) 

# write csv with the actual values for 10-07-2022 trhough 11-06-2022
#actual_benchmark <- df %>%
#  filter(date %in% prediction_dates) %>%
#  pivot_longer(!date, names_to = "state", values_to = "actual")

#write.csv(actual_benchmark, "data/actual_benchmark.csv", row.names = F)

# write csv with average predictions for 10-07-2022 trhough 11-06-2022

#week_means <- data.frame()
#for(j in prediction_dates){
  
#  j = as.Date(j)
  
#  week_start = j- 13
#  week_end = j - 7
#  one_week <- seq(week_start, week_end, 1)
  
#  means <- data %>%
#    filter(date %in% one_week) %>%
#    summarize(across(!date, mean )) %>%
#    pivot_longer(everything(), names_to = "state", values_to = "pred_week_means") %>%
#    arrange(state) %>%
#    mutate(date = j, week_start = week_start, week_end = week_end)
  
#  week_means <- rbind(week_means, means)
#}

#write.csv(week_means, "data/week_mean_predictions.csv", row.names = F)

actual_benchmark <- read.csv(here("data/actual_benchmark.csv"))
week_means <- read.csv(here("data/week_mean_predictions.csv"))

### BIGVAR
prediction_dates <- seq(as.Date("2021-10-07"), 
                        as.Date("2021-11-06"), 1)

states <- sort(colnames(df)[colnames(df) != "date"])
final <- data.frame()
data = df
lag = 5
h = 7
recursive = T
methods <- c("Basic", "HLAGC", "HLAGOO", "HLAGELEM")

for(j in prediction_dates){

  j = as.Date(j)  
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
  
  # iterate over BigVar Methods
  for(i in methods){
    
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
    
    pred <- predict(result, n.ahead = h)%>%
      as.data.frame() %>%
      mutate(state = states,
             date = j)
    colnames(pred)[colnames(pred) == "V1"] <- paste("pred", i, sep = "_")
    
    
    
    predictions <- left_join(predictions, pred, by = c("state", "date"))
    
    #assign(paste("mod", i, j, sep = "_"), mod, envir = .GlobalEnv)
    #assign(paste("result", i, j, sep = "_"), result, envir = .GlobalEnv)
    #assign(paste("pred", i, j, sep = "_"), pred, envir = .GlobalEnv)
    
    print(paste(j, i, "complete"))
  }
  
  actual_df <- actual_benchmark %>%
    filter(date == j) %>%
    mutate(date =as.Date(date)) %>%
    arrange(state)
  
  week_means_pred <- week_means %>%
    filter(date == j) %>%
    mutate(date =as.Date(date)) %>%
    arrange(state) 
  
  comparison <- list(actual_df, predictions, week_means_pred) %>%
    reduce(left_join, by = c("date", "state")) %>%
    mutate(diff_pred_Basic = (actual - pred_Basic)^2, 
           diff_pred_HLAGC = (actual - pred_HLAGC)^2, 
           diff_pred_HLAGOO = (actual - pred_HLAGOO)^2, 
           diff_pred_HLAGELEM = (actual - pred_HLAGELEM)^2, 
           diff_pred_week_means = (actual - pred_week_means)^2) 
  
  # calculate RMSE
  rmse_pred_Basic = sqrt(mean(comparison$diff_pred_Basic))
  rmse_pred_HLAGC = sqrt(mean(comparison$diff_pred_HLAGC))
  rmse_pred_HLAGOO = sqrt(mean(comparison$diff_pred_HLAGOO))
  rmse_pred_HLAGELEM = sqrt(mean(comparison$diff_pred_HLAGELEM))
  rmse_pred_week_means = sqrt(mean(comparison$diff_pred_week_means))
  
  
  out <- list(prediction_date = j, 
                      actual_start = actual_start, 
                      actual_end = actual_end, 
                      week_start = unique(comparison$week_start),
                      week_end = unique(comparison$week_end),
                      lag = lag, 
                      period = h, 
                      rmse_pred_Basic = rmse_pred_Basic, 
                      rmse_pred_HLAGC =   rmse_pred_HLAGC, 
                      rmse_pred_HLAGOO = rmse_pred_HLAGOO,
                      rmse_pred_HLAGELEM = rmse_pred_HLAGELEM,
                      rmse_pred_week_means = rmse_pred_week_means)
  
  
  final <- rbind(final, out)
  assign(paste("comparison",j, sep = "_"), comparison, envir = .GlobalEnv)
}
  




#write.csv(final, "data/rmse_out/out_10_07_2021_10_17_2021.csv")
#comparison_list <- mget(ls(pattern="comparison_2021-"), ifnotfound = "Not Found")
#lapply(1:length(comparison_list), function(x) write.csv(comparison_list[[x]], 
    #                                                    paste("data/rmse_out/comparison", unique(comparison_list[[x]]$date), ".csv", sep = ""), 
     #                                                   row.names = F))

final$best_method <- str_remove(colnames(final[, c("rmse_pred_Basic", "rmse_pred_HLAGC", 
                   "rmse_pred_HLAGOO", "rmse_pred_HLAGELEM", 
                   "rmse_pred_week_means")])[apply(final[, c("rmse_pred_Basic", "rmse_pred_HLAGC", 
                                                             "rmse_pred_HLAGOO", "rmse_pred_HLAGELEM", 
                                                             "rmse_pred_week_means")],1,which.min)], 
           "rmse_pred_")


#write.csv(final, "data/rmse_out/out_10_07_2021_10_17_2021.csv")

