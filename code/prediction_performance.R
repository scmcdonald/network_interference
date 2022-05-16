library(tidyverse)
library(here)
library(lubridate)
library(MASS, exclude = "select")
library(vars)
library(RColorBrewer)
cb_colors <- brewer.pal(n = 8, name = "Dark2")

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

  # get one week before (seven days)
  week_start = prediction_date- 13
  week_end = prediction_date - 7
  
 # week_start = prediction_date- 4
#  week_end = prediction_date - 1
  
  one_week <- seq(week_start, week_end, 1)
  
  week_means <- data %>%
    filter(date %in% one_week) %>%
    select(!date) %>%
    summarize(across(everything(), mean )) %>%
    pivot_longer(everything(), names_to = "state", values_to = "week_mean")
  
  # put together predicted and actual values
  # calculate difference and squared difference
  comparison <- merge(predicted_t_7, actual_t_7, by = "state", all = T) %>%
    merge(week_means, by = "state",all = T) %>%
    mutate(diff_pred = actual_t_7- predicted_t_7, 
           diff_pred_squared = diff_pred^2, 
           diff_ave = actual_t_7 - week_mean, 
           diff_ave_squared = diff_ave^2) 
  
  
  # calcualte RMSE
  rmse_pred = sqrt(mean(comparison$diff_pred_squared))
  rmse_ave = sqrt(mean(comparison$diff_ave_squared))
  
  # final output
  final <- list(prediction_date =prediction_date, 
                actual_start = actual_start, 
                actual_end = actual_end, 
                week_start = week_start,
                week_end = week_end,
                lag = lag, 
                period = period, 
                rmse_pred = rmse_pred, 
                rmse_ave = rmse_ave)
  
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
  
raw_df <- read.csv(here("data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")) %>%
  select(submission_date, state, new_case)%>%
  mutate(new_case = ifelse(new_case <0, 0, new_case)) %>%
  mutate(new_case = log(new_case + 1))%>%
  mutate(date = mdy(submission_date)) %>%
  select(date, state, cases = new_case)

df <- raw_df %>%
  pivot_wider(names_from = state, values_from = cases) 



rmse_out_1 <- prediction_performance(data = df, 
                                     prediction_dates = seq(as.Date("2021-10-07"), 
                                                            as.Date("2021-11-06"), 1), 
                                     lag = 1, 
                                     period = 7)


rmse_out_2 <- prediction_performance(data = df, 
                                     prediction_dates = seq(as.Date("2021-10-07"), 
                                                            as.Date("2021-11-06"), 1), 
                                     lag = 2, 
                                     period = 7)


rmse_out_3 <- prediction_performance(data = df, 
                                     prediction_dates = seq(as.Date("2021-10-07"), 
                                                            as.Date("2021-11-06"), 1), 
                                     lag = 3, 
                                     period = 7)


rmse_out_4 <- prediction_performance(data = df, 
                                   prediction_dates = seq(as.Date("2021-10-07"), 
                                                          as.Date("2021-11-06"), 1), 
                                   lag = 4, 
                                   period = 7)


rmse_out_5 <- prediction_performance(data = df, 
                                     prediction_dates = seq(as.Date("2021-10-07"), 
                                                            as.Date("2021-11-06"), 1), 
                                     lag = 5, 
                                     period = 7)

rmse_out_6 <- prediction_performance(data = df, 
                                     prediction_dates = seq(as.Date("2021-10-07"), 
                                                            as.Date("2021-11-06"), 1), 
                                     lag = 6, 
                                     period = 7)



# compare lags
colnames(rmse_out_1)[colnames(rmse_out_1)== "rmse_pred"] <- "rmse_pred_1"
colnames(rmse_out_2)[colnames(rmse_out_2)== "rmse_pred"] <- "rmse_pred_2"
colnames(rmse_out_3)[colnames(rmse_out_3)== "rmse_pred"] <- "rmse_pred_3"
colnames(rmse_out_4)[colnames(rmse_out_4)== "rmse_pred"] <- "rmse_pred_4"
colnames(rmse_out_5)[colnames(rmse_out_5)== "rmse_pred"] <- "rmse_pred_5"




lag_compare <- list(rmse_out_1[, c("prediction_date", "rmse_pred_1")], 
           rmse_out_2[, c("prediction_date", "rmse_pred_2")], 
           rmse_out_3[, c("prediction_date", "rmse_pred_3")], 
     rmse_out_4[, c("prediction_date", "rmse_pred_4")], 
     rmse_out_5[, c("prediction_date", "rmse_pred_5", "rmse_ave")]) %>% 
  reduce(left_join, by = "prediction_date")



lag_compare %>%
  ggplot(aes(x = as.numeric(prediction_date))) +
  #geom_segment(aes(x = as.numeric(prediction_date), xend = as.numeric(prediction_date), y = rmse_ave, yend = rmse_pred)) +
  geom_point(aes(y = rmse_ave, color = "rmse_ave"), size = 3)+
  geom_point(aes(y = rmse_pred_1, color = "rmse_pred_1"), size =3, alpha = .4)+
  geom_point(aes(y = rmse_pred_2, color = "rmse_pred_2"), size =3, alpha = .4)+
  geom_point(aes(y = rmse_pred_3, color = "rmse_pred_3"), size =3, alpha = .4)+
  geom_point(aes(y = rmse_pred_4, color = "rmse_pred_4"), size =3, alpha = .4)+
  geom_point(aes(y = rmse_pred_5, color = "rmse_pred_5"), size =3, alpha = .4)+
  scale_color_manual(name = "RMSE", values = c("black", cb_colors[1:5]), labels = c("Average", "VAR(1)", 
                                                                                    "VAR(2)", 
                                                                                    "VAR(3)", 
                                                                                    "VAR(4)", 
                                                                                    "VAR(5)"
                                                                                    )) + 
  scale_x_reverse(labels = as.character(format(lag_compare$prediction_date, format = "%b %d")),                     
                  breaks = as.numeric(lag_compare$prediction_date)) + 
  theme_minimal() +
  coord_flip()  +
  labs(x = "Date (2021)", 
       y = "RMSE", 
       title = "RMSE by Prediction Date")



# plot one lag
rmse_out_4 %>%
  rename(rmse_pred =rmse_pred_4 ) %>%
  select(prediction_date, rmse_pred, rmse_ave) %>%
  ggplot(aes(x = as.numeric(prediction_date))) +
  geom_segment(aes(x = as.numeric(prediction_date), xend = as.numeric(prediction_date), y = rmse_ave, yend = rmse_pred)) +
  geom_point(aes(y = rmse_ave, color = "rmse_ave"), size = 3)+
  geom_point(aes(y = rmse_pred, color = "rmse_pred"), size =3)+
  scale_color_manual(name = "RMSE", values = c("green", "purple"), labels = c("Average", "VAR(p = 4)")) + 
  scale_x_reverse(labels = as.character(format(rmse_out_4$prediction_date, format = "%b %d")),                     
                  breaks = as.numeric(rmse_out_4$prediction_date)) + 
  theme_minimal() +
  coord_flip()  +
  labs(x = "Date (2021)", 
       y = "RMSE", 
       title = "RMSE by Prediction Date")




###  Northeast states only
# make table of states/regions
states <- data.frame(state.name, state.abb)
northeast_states_df <- states %>%
  filter(state.region == "Northeast")
northeast_states <- northeast_states_df %>%
  select(state.abb) %>%
  rename("state" = "state.abb") %>%
  pull(state)

northeast_states <- c(northeast_states, "NYC")

# make northeast df
northeast_df <- df %>%
  # select northeast states only
  select(all_of(c("date", northeast_states))) 


for(i in 1:10){
  rmse_out_ne <- prediction_performance(data = northeast_df, 
                                          prediction_dates = seq(as.Date("2021-10-07"), 
                                                                 as.Date("2021-11-06"), 1), 
                                          lag = i, 
                                          period = 7) 
  colnames(rmse_out_ne)[colnames(rmse_out_ne) == "rmse_pred"] <- paste("rmse_pred", i, sep = "_")
  assign(paste("rmse_out_ne", i, sep = "_"), rmse_out_ne, envir = .GlobalEnv)

}


lag_compare_ne <- list(rmse_out_ne_1[, c("prediction_date", "rmse_pred_1")], 
                    rmse_out_ne_2[, c("prediction_date", "rmse_pred_2")], 
                    rmse_out_ne_3[, c("prediction_date", "rmse_pred_3")], 
                    rmse_out_ne_4[, c("prediction_date", "rmse_pred_4")], 
                    rmse_out_ne_5[, c("prediction_date", "rmse_pred_5")], 
                    rmse_out_ne_6[, c("prediction_date", "rmse_pred_6")], 
                    rmse_out_ne_7[, c("prediction_date", "rmse_pred_7")], 
                    rmse_out_ne_8[, c("prediction_date", "rmse_pred_8")],
                    rmse_out_ne_9[, c("prediction_date", "rmse_pred_9")],
                    rmse_out_ne_10[, c("prediction_date", "rmse_pred_10", "rmse_ave")]) %>% 
  reduce(left_join, by = "prediction_date")



lag_compare_ne %>%
  ggplot(aes(x = as.numeric(prediction_date))) +
  #geom_segment(aes(x = as.numeric(prediction_date), xend = as.numeric(prediction_date), y = rmse_ave, yend = rmse_pred)) +
  geom_point(aes(y = rmse_ave, color = "rmse_ave"), size = 3)+
  geom_point(aes(y = rmse_pred_1, color = "rmse_pred_1"), size =3, alpha = .4)+
  geom_point(aes(y = rmse_pred_2, color = "rmse_pred_2"), size =3, alpha = .4)+
  geom_point(aes(y = rmse_pred_3, color = "rmse_pred_3"), size =3, alpha = .4)+
  geom_point(aes(y = rmse_pred_4, color = "rmse_pred_4"), size =3, alpha = .4)+
  geom_point(aes(y = rmse_pred_5, color = "rmse_pred_5"), size =3, alpha = .4)+
  geom_point(aes(y = rmse_pred_6, color = "rmse_pred_6"), size =3, alpha = .4)+
  geom_point(aes(y = rmse_pred_7, color = "rmse_pred_7"), size =3, alpha = .4)+
  geom_point(aes(y = rmse_pred_8, color = "rmse_pred_8"), size =3, alpha = .4)+
  geom_point(aes(y = rmse_pred_9, color = "rmse_pred_9"), size =3, alpha = .4)+
  geom_point(aes(y = rmse_pred_10, color = "rmse_pred_10"), size =3, alpha = .4)+
  scale_color_manual(name = "RMSE", values = c("black", cb_colors[1:8], "blue", "green"), labels = c("Average", "VAR(1)", 
                                                                                    "VAR(2)", 
                                                                                    "VAR(3)", 
                                                                                    "VAR(4)", 
                                                                                    "VAR(5)", 
                                                                                    "VAR(6)", 
                                                                                    "VAR(7)", 
                                                                                    "VAR(8)", 
                                                                                    "VAR(9)", 
                                                                                    "VAR(10)"
  )) + 
  scale_x_reverse(labels = as.character(format(lag_compare_ne$prediction_date, format = "%b %d")),                     
                  breaks = as.numeric(lag_compare_ne$prediction_date)) + 
  theme_minimal() +
  coord_flip()  +
  labs(x = "Date (2021)", 
       y = "RMSE", 
       title = "RMSE by Prediction Date")





# std deviation comparison from meeting
tbl <- raw_df %>%
  filter(date%in% seq(as.Date("2021-10-07"), 
                      as.Date("2021-11-06"), 1)) %>%
  group_by(date) %>%
  summarize(sd = sd(cases)) %>%
  mutate(date = as.character(date))


compare <- rmse_out %>%
  select(prediction_date, rmse) %>%
  mutate(prediction_date = as.character(prediction_date)) %>%
  merge(tbl, by.x = "prediction_date", by.y = "date", all= T) %>%
  mutate(ratio = rmse/sd)


hist(compare$ratio)



# example model

prediction_date = "2021-10-07"
df = data
lag = 4 
period = 7

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

model <- VAR(var_data, p = lag)


summary(model$varresult$CA)
# restrict model takes a long time 30+ minutes for one time prediction
restrict_model <- restrict(model)

summary(restrict_model$varresult$CA)

#why is CA not significant?

restrict_prediction <- predict(restrict_model, n.ahead = period)

# get predict periods ahead for each state
predicted_t_7 <-lapply(restrict_prediction$fcst, "[[", 7)  %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = c("state"), values_to = "predicted_t_7")

# get actual value for (7) days ahead
actual_t_7 <- data %>%
  filter(date == prediction_date) %>%
  select(!date) %>%
  pivot_longer(everything(), names_to = "state", values_to = "actual_t_7")

# get one week before (seven days)
week_start = prediction_date- 13
week_end = prediction_date - 7

# week_start = prediction_date- 4
#  week_end = prediction_date - 1

one_week <- seq(week_start, week_end, 1)

week_means <- data %>%
  filter(date %in% one_week) %>%
  select(!date) %>%
  summarize(across(everything(), mean )) %>%
  pivot_longer(everything(), names_to = "state", values_to = "week_mean")

# put together predicted and actual values
# calculate difference and squared difference
comparison <- merge(predicted_t_7, actual_t_7, by = "state", all = T) %>%
  merge(week_means, by = "state",all = T) %>%
  mutate(diff_pred = actual_t_7- predicted_t_7, 
         diff_pred_squared = diff_pred^2, 
         diff_ave = actual_t_7 - week_mean, 
         diff_ave_squared = diff_ave^2) 


# calculate RMSE
rmse_pred = sqrt(mean(comparison$diff_pred_squared))
rmse_ave = sqrt(mean(comparison$diff_ave_squared))

# final output
final <- list(prediction_date =prediction_date, 
              actual_start = actual_start, 
              actual_end = actual_end, 
              week_start = week_start,
              week_end = week_end,
              lag = lag, 
              period = period, 
              rmse_pred = rmse_pred, 
              rmse_ave = rmse_ave)

# final comparison. rmse_pred > rmse_ave
final


### BIGVAR
library(BigVAR)

mod1<-constructModel(var_data, gran = c(50, 10),
                     p=20,"HLAGC",verbose=TRUE,IC=TRUE,model.controls=list(intercept=TRUE))
results=cv.BigVAR(mod1)
results
plot(results)

big_var_predict <- predict(results,n.ahead=7)


rownames(big_var_predict) <- colnames(var_data)
big_var_predict_out <- as.data.frame(big_var_predict) %>%
  rownames_to_column(var = "state")

coef(results)

# get actual value for (7) days ahead
actual_t_7 <- data %>%
  filter(date == prediction_date) %>%
  select(!date) %>%
  pivot_longer(everything(), names_to = "state", values_to = "actual_t_7")

# get one week before (seven days)
week_start = prediction_date- 13
week_end = prediction_date - 7

# week_start = prediction_date- 4
#  week_end = prediction_date - 1

one_week <- seq(week_start, week_end, 1)

week_means <- data %>%
  filter(date %in% one_week) %>%
  select(!date) %>%
  summarize(across(everything(), mean )) %>%
  pivot_longer(everything(), names_to = "state", values_to = "week_mean")

# put together predicted and actual values
# calculate difference and squared difference
comparison <- merge(big_var_predict_out, actual_t_7, by = "state", all = T) %>%
  merge(week_means, by = "state",all = T) %>%
  mutate(diff_pred = actual_t_7- V1, 
         diff_pred_squared = diff_pred^2, 
         diff_ave = actual_t_7 - week_mean, 
         diff_ave_squared = diff_ave^2) 


# calculate RMSE
rmse_pred = sqrt(mean(comparison$diff_pred_squared))
rmse_ave = sqrt(mean(comparison$diff_ave_squared))


final_hlagc <- list(prediction_date =prediction_date, 
                    actual_start = actual_start, 
                    actual_end = actual_end, 
                    week_start = week_start,
                    week_end = week_end,
                    lag = lag, 
                    period = period, 
                    rmse_pred = rmse_pred, 
                    rmse_ave = rmse_ave)

# final output
#final_lasso <- list(prediction_date =prediction_date, 
 #             actual_start = actual_start, 
  #            actual_end = actual_end, 
   #           week_start = week_start,
    #          week_end = week_end,
     #         lag = lag, 
      #        period = period, 
       #       rmse_pred = rmse_pred, 
        #      rmse_ave = rmse_ave)

# these are much closer
final

coefs <- coef(results)

names(coefs)

rownames(coefs) <- colnames(var_data)


names(coefs) <- c("intercept", paste(rep(colnames(var_data), 20), rep(c("L1", "L2", "L3", "L4", 
                                                                        "L5", "L6", "L7", "L8", 
                                                                        "L9", "L10", "L11", "L12", 
                                                                        "L13", "L14", "L15", "L16", 
                                                                        "L17", "L18", "L19", "L20"), each = 60), sep = ""))


coefs["PA", ]
coefs["CA", ]
coefs["NY", ]
coefs["NYC", ]
coefs["VA", ]

# only states with non zero coeffs
coefs["KS", ]
coefs["LA", ]
coefs["CT", ]



