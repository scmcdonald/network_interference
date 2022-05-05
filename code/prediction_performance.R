library(tidyverse)
library(here)
library(lubridate)
library(MASS, exclude = "select")
library(vars)


# get rmse for prediction date, with specified data, lag and period
rmse <- function(data, prediction_date, lag, period){
  
  # get prediction date, start and end dates of actual data
  prediction_date = as.Date(prediction_date)
  actual_end = prediction_date - period
  actual_start = actual_end - 365
  
  # subset actual data for training var
  actual_data_train <- data %>%
    filter(date >= actual_start & date <= actual_end)
  
  # make each individual state a time series
  ts_list <- lapply(X = setNames( colnames(data) [!(colnames(data) %in% "date")],  colnames(data) [!(colnames(data) %in% "date")]), FUN =  function(x) {
    state_ts <- ts(actual_data_train[[x]], 
                   frequency = 1)
    state_ts
  })
  
  var_data <- do.call(ts.union, ts_list)
  
  # train the model
  model <- VAR(var_data, p = lag)
  
  # predict (7) periods ahead
  prediction <- predict(model, n.ahead = period)
  
  # get predict periods ahead for each state
  predicted_t_7 <-lapply(prediction$fcst, "[[", 7)  %>%
    as.data.frame() %>%
    pivot_longer(everything(), names_to = c("state"), values_to = "predicted_t_7")
  
  # get actual value for (7) days ahead
  actual_t_7 <- data %>%
    filter(date == prediction_date) %>%
    select(!date) %>%
    pivot_longer(everything(), names_to = "state", values_to = "actual_t_7")
  
  # put together predicted and actual values
  # calculate difference and squared difference
  comparison <- merge(predicted_t_7, actual_t_7, by = "state", all = T) %>%
    mutate(diff = actual_t_7- predicted_t_7, 
           diff_squared = diff^2)
  
  # calcualte RMSE
  rmse = sqrt(mean(comparison$diff_squared))
  
  # final output
  final <- list(prediction_date =prediction_date, 
                actual_start = actual_start, 
                actual_end = actual_end, 
                lag = lag, 
                period = period, 
                rmse = rmse)
  
  return(final)
}

# apply rmse function to each date in series of dates
# df output
prediction_performance <- function(data, prediction_dates, lag, period){
  out <- lapply(X = prediction_dates, 
                FUN = function(x) rmse(data = data, 
                                       prediction_date = x, 
                                       lag = lag, 
                                       period = period))
  return(bind_rows(out))
  
}
  

df <- read.csv(here("data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")) %>%
  select(submission_date, state, new_case) %>%
  mutate(date = mdy(submission_date)) %>%
  select(date, state, cases = new_case) %>%
  pivot_wider(names_from = state, values_from = cases) 



rmse_out <- prediction_performance(data = df, 
                                   prediction_dates = seq(as.Date("2021-10-07"), 
                                                          as.Date("2021-11-06"), 1), 
                                   lag = 4, 
                                   period = 7)
#==================#

ggplot(rmse_out, aes(x = as.numeric(prediction_date), y = rmse)) + 
  geom_bar(stat = "identity") +
  scale_x_reverse(labels = as.character(format(rmse_out$prediction_date, format = "%b %d")),                     
                  breaks = as.numeric(rmse_out$prediction_date)) + 
  #scale_x_date(breaks = "1 day", date_labels = "%b %d") + 
  theme_minimal() +
  coord_flip()  +
  labs(x = "Date (2021)", 
       y = "RMSE", 
       title = "RMSE by Prediction Date")



