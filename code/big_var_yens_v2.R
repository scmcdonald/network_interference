rm(list=ls())


library(here)
library(tidyverse)
library(lubridate)
library(MASS, exclude = "select")
library(vars)
library(BigVAR, exclude = "filter")
library(RColorBrewer)

raw_df <- read.csv(here("data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")) %>%
  select(submission_date, state, new_case)%>%
  mutate(new_case = ifelse(new_case <0, 0, new_case)) %>%
  mutate(new_case = log(new_case + 1))%>%
  mutate(date = mdy(submission_date)) %>%
  select(date, state, cases = new_case)

df <- raw_df %>%
  pivot_wider(names_from = state, values_from = cases) 

actual_benchmark <- read.csv(here("data/actual_benchmark.csv"), colClasses = c("Date", "character", "numeric"))

week_means <- read.csv(here("data/week_mean_predictions.csv"), colClasses = c("character", "numeric", "Date", "Date", "Date"))

### BIGVAR

prediction_dates <- seq(as.Date(Sys.getenv('START_DATE'), format = "%Y-%m-%d"), 
                        as.Date(Sys.getenv('END_DATE'), format = "%Y-%m-%d"), 1)
states <- sort(colnames(df)[colnames(df) != "date"])
final <- data.frame()
					      			
data = df
lag = as.numeric(Sys.getenv('LAG'))
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
    
    print(paste(j, i, "complete"))
    
    saveRDS(result, file = here( paste("data/result/result_", j, "_L", lag, "_" , i, ".rds", sep = "")))
    
    coefficients <- coef(result)
    
    rownames(coefficients) <- states
    
    colnames(coefficients) <- c("intercept", paste(rep(states, lag), 
                                                   rep(paste("L", 1:lag, sep = ""), each = 60), sep = ""))
    
    
    saveRDS(coefficients, file = here( paste("data/coefficients/coef_", j, "_L", lag, "_" , i, ".rds", sep = "")))
    
  }
  print("bigvar done")

  actual_df <- actual_benchmark %>%
    filter(date == j) %>%
    arrange(state)
  
  week_means_pred <- week_means %>%
    filter(date == j) %>%
    arrange(state) 
  
  comparison <- list(actual_df, predictions, week_means_pred) %>%
    reduce(left_join, by = c("date", "state")) %>%
    mutate(diff_pred_Basic = (actual - pred_Basic)^2, 
           diff_pred_HLAGC = (actual - pred_HLAGC)^2, 
           diff_pred_HLAGOO = (actual - pred_HLAGOO)^2, 
           diff_pred_HLAGELEM = (actual - pred_HLAGELEM)^2, 
           diff_pred_week_means = (actual - pred_week_means)^2) 
  print("comparison df done")

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
  print("outlist done")
  
  final <- rbind(final, out)

  print("rbind to final df done")
  assign(paste("comparison",j, sep = "_"), comparison, envir = .GlobalEnv)
}

final$best_method <- str_remove(colnames(final[, c("rmse_pred_Basic", "rmse_pred_HLAGC", 
                                                   "rmse_pred_HLAGOO", "rmse_pred_HLAGELEM", 
                                                   "rmse_pred_week_means")])[apply(final[, c("rmse_pred_Basic", "rmse_pred_HLAGC", 
                                                                                             "rmse_pred_HLAGOO", "rmse_pred_HLAGELEM", 
                                                                                             "rmse_pred_week_means")],1,which.min)], 
                                "rmse_pred_")





write.csv(final, here(paste("data/rmse_out/out_", min(prediction_dates),"_", max(prediction_dates), "_", lag,".csv", sep = "")))




comparison_list <- mget(ls(pattern="comparison_2021-"), ifnotfound = "Not Found")
lapply(1:length(comparison_list), function(x) write.csv(comparison_list[[x]],
                                                        here(paste("data/rmse_out/comparison", unique(comparison_list[[x]]$date), "_", lag, ".csv", sep = "")),
                                                         row.names = F))


# one example coefs










