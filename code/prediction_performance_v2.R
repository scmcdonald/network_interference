library(tidyverse)
library(here)
library(lubridate)
library(MASS, exclude = "select")
library(vars)


prediction_dates = seq(as.Date("2021-10-07"), 
                       as.Date("2021-11-06"), 1)

# get rmse for prediction date, with specified data, lag and period
rmse <- function(data, prediction_date, lag, period){
  
  # get prediction date, start and end dates of actual data

  actual_end = prediction_dates - period
  actual_start = actual_end - 365
  
  # subset actual data for training var
  actual_data_train <- mapply(x = actual_start, y = actual_end,
                 FUN = function(x, y, z){
                   d <- data %>%
                     filter(date >= x & date <= y);
                   return(d)
                 }, SIMPLIFY = F)
  
  names(actual_data_train) = as.character(prediction_dates)
  
  
  
  # make each individual state a time series
  ts_list <- lapply(X = actual_data_train, FUN =  function(x) {
    state_ts <- ts(x, frequency = 1)
    
    state_ts
  })
  
  #var_data <- do.call(ts.union, ts_list)
  
  # train the model
    model <- lapply(ts_list, 
                      FUN = function(x){
                        mod <- VAR(x, p = lag)})
  
  # predict (7) periods ahead
    prediction <-lapply(model, 
                           FUN = function(x){
                             pred <- predict(x, n.ahead = period)})

  
  
  #prediction <- predict(model, n.ahead = period)
  
  # get predict periods ahead for each state
  fcst <- lapply(prediction, "[[", 1)
    
    
  predicted_t_7 <-lapply(fcst, "[[", 7)  %>%
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
  mutate(new_case = ifelse(new_case <0, 0, new_case)) %>%
  mutate(new_case = log(new_case + 1))%>%
  mutate(date = mdy(submission_date)) %>%
  select(date, state, cases = new_case) %>%
  pivot_wider(names_from = state, values_from = cases) 


start_time <- Sys.time()
rmse_out <- prediction_performance(data = df, 
                                   prediction_dates = seq(as.Date("2021-10-07"), 
                                                          as.Date("2021-11-06"), 1), 
                                   lag = 4, 
                                   period = 7)
end_time <- Sys.time()

total_time <- end_time - start_time


write.table(total_time, here("code/runtime.txt"), append = FALSE, sep = ",", dec = ".",
            row.names = F, col.names = F)

#==================#

ggplot(rmse_out, aes(x = as.numeric(prediction_date), y = rmse)) + 
  geom_bar(stat = "identity") +
  scale_x_reverse(labels = as.character(format(rmse_out$prediction_date, format = "%b %d")),                     
                  breaks = as.numeric(rmse_out$prediction_date)) + 
  theme_minimal() +
  coord_flip()  +
  labs(x = "Date (2021)", 
       y = "RMSE", 
       title = "RMSE by Prediction Date")
