library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(here)
cb_colors <- brewer.pal(n = 8, name = "Dark2")
# custom ggplot2 theme
custom_theme <- theme_minimal() + 
  theme(strip.text = element_text(size = 16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        panel.spacing = unit(1, "lines"), 
        legend.title = element_text(size=16,face="bold"), 
        legend.text = element_text(size=16), 
        plot.title = element_text(size = 20, face = "bold"), 
        plot.title.position = "plot")
# lag = 5
# lag = 10

lag5 <- do.call(rbind, lapply(list.files("data/rmse_out", pattern="^out_.*_L5\\.csv", full.names = T),function(x)
  read.csv(x))) %>%
  mutate(prediction_date = as.Date(x = prediction_date, origin = "1970-01-01"))



text_color <- lag5 %>%
  select(prediction_date, rmse_pred_Basic, rmse_pred_HLAGC, rmse_pred_HLAGELEM, rmse_pred_HLAGOO, rmse_pred_week_means) %>%
  pivot_longer(!prediction_date, names_to = "rmse_name", values_to = "rmse_value") %>%
  group_by(prediction_date) %>%
  mutate(rmse_min= ifelse(rmse_value == min(rmse_value), 1, 0), 
         best_rmse =rmse_name[which.max(rmse_min)]) %>%
  ungroup() %>%
  select(prediction_date, best_rmse) %>%
  mutate(color = case_when(best_rmse == "rmse_pred_week_means" ~ "black",
                           best_rmse == "rmse_pred_Basic" ~ cb_colors[1],
                           best_rmse == "rmse_pred_HLAGC" ~ cb_colors[2],
                           best_rmse == "rmse_pred_HLAGELEM" ~ cb_colors[3],
                           best_rmse == "rmse_pred_HLAGOO" ~ cb_colors[4],
                           TRUE ~ "red")) %>%
  distinct()


lag5 %>%
  select(prediction_date, rmse_pred_Basic, rmse_pred_HLAGC, rmse_pred_HLAGELEM, rmse_pred_HLAGOO, rmse_pred_week_means) %>%
  pivot_longer(!prediction_date, names_to = "rmse_name", values_to = "rmse_value") %>%
  group_by(prediction_date) %>%
  mutate(rmse_min= ifelse(rmse_value == min(rmse_value), 1, 0), 
         best_rmse =rmse_name[which.max(rmse_min)]) %>%
  ggplot(aes(x = as.numeric(prediction_date), y = rmse_value, color = rmse_name)) +
  geom_point(size = 3) + 
  scale_color_manual(name = "RMSE", values = c(cb_colors[1:4], "black"), 
                     labels = c("Basic", "HLAGC", "HALGELEM", "HLAGOO", "Week Means")) +
  scale_x_reverse(labels = as.character(format(lag5$prediction_date, format = "%b %d")),                     
                  breaks = as.numeric(lag5$prediction_date)) + 
  scale_y_continuous(limits = c(0,5))+ 
  theme_minimal() +
  coord_flip()  +
  labs(x = "Date (2021)", 
       y = "RMSE", 
       title = "RMSE for VAR(5) by Prediction Date") +
  theme(axis.text.y = element_text(color =text_color$color, face = "bold"), 
        plot.title.position = "plot")
ggsave("output/bigvar_rmse_5.png", width = 7, height = 12)



# lag = 10

lag10 <- do.call(rbind, lapply(list.files("data/rmse_out", pattern="^out_.*_L10\\.csv", full.names = T),function(x)
  read.csv(x))) %>%
  mutate(prediction_date = as.Date(x = prediction_date, origin = "1970-01-01"))

text_color <- lag10 %>%
  select(prediction_date, best_method) %>%
  mutate(color = case_when(best_method == "week_means" ~ "black",
                           best_method == "Basic" ~ cb_colors[1],
                           best_method == "HLAGC" ~ cb_colors[2],
                           best_method == "HLAGELEM" ~ cb_colors[3],
                           best_method == "HLAGOO" ~ cb_colors[4],
                           TRUE ~ "red")) %>%
  distinct()

lag10 %>%
  select(prediction_date, rmse_pred_Basic, rmse_pred_HLAGC, rmse_pred_HLAGELEM, rmse_pred_HLAGOO, rmse_pred_week_means) %>%
  pivot_longer(!prediction_date, names_to = "rmse_name", values_to = "rmse_value") %>%
  group_by(prediction_date) %>%
  mutate(rmse_min= ifelse(rmse_value == min(rmse_value), 1, 0), 
         best_rmse =rmse_name[which.max(rmse_min)]) %>%
  ggplot(aes(x = as.numeric(prediction_date), y = rmse_value, color = rmse_name)) +
  geom_point(size = 3) + 
  scale_color_manual(name = "RMSE", values = c(cb_colors[1:4], "black"), 
                     labels = c("Basic", "HLAGC", "HALGELEM", "HLAGOO", "Week Means")) +
  scale_x_reverse(labels = as.character(format(lag10$prediction_date, format = "%b %d")),                     
                  breaks = as.numeric(lag10$prediction_date)) + 
  scale_y_continuous(limits = c(0,5))+ 
  theme_minimal() +
  coord_flip()  +
  labs(x = "Date (2021)", 
       y = "RMSE", 
       title = "RMSE for VAR(10) by Prediction Date") +
  theme(axis.text.y = element_text(color =text_color$color, face = "bold"), 
        plot.title.position = "plot")
ggsave("output/bigvar_rmse_10.png", width = 7, height = 12)


# lag = 20

lag20 <- do.call(rbind, lapply(list.files("data/rmse_out", pattern="^out_.*_L20\\.csv", full.names = T),function(x)
       read.csv(x))) %>%
  mutate(week_end = as.Date(x = week_end, origin = "1970-01-01"),
         prediction_date = as.Date(x = prediction_date, origin = "1970-01-01"))

text_color <- lag20 %>%
  select(prediction_date, best_method) %>%
  mutate(color = case_when(best_method == "week_means" ~ "black",
                           best_method == "Basic" ~ cb_colors[1],
                           best_method == "HLAGC" ~ cb_colors[2],
                           best_method == "HLAGELEM" ~ cb_colors[3],
                           best_method == "HLAGOO" ~ cb_colors[4],
                           TRUE ~ "red")) %>%
  distinct()

lag20 %>%
  select(prediction_date, rmse_pred_Basic, rmse_pred_HLAGC, rmse_pred_HLAGELEM, rmse_pred_HLAGOO, rmse_pred_week_means) %>%
  pivot_longer(!prediction_date, names_to = "rmse_name", values_to = "rmse_value") %>%
  group_by(prediction_date) %>%
  mutate(rmse_min= ifelse(rmse_value == min(rmse_value), 1, 0), 
         best_rmse =rmse_name[which.max(rmse_min)]) %>%
  ggplot(aes(x = as.numeric(prediction_date), y = rmse_value, color = rmse_name)) +
  geom_point(size = 3) + 
  scale_color_manual(name = "RMSE", values = c(cb_colors[1:4], "black"), 
                     labels = c("Basic", "HLAGC", "HALGELEM", "HLAGOO", "Week Means")) +
  scale_x_reverse(labels = as.character(format(lag20$prediction_date, format = "%b %d")),                     
                  breaks = as.numeric(lag20$prediction_date)) + 
  scale_y_continuous(limits = c(0,5))+ 
  theme_minimal() +
  coord_flip()  +
  labs(x = "Date (2021)", 
       y = "RMSE", 
       title = "RMSE for VAR(20) by Prediction Date") +
  theme(axis.text.y = element_text(color =text_color$color, face = "bold"), 
        plot.title.position = "plot")
ggsave("output/bigvar_rmse_20.png", width = 7, height = 12)




# best method by lag

as.data.frame(rbind(c(table(lag5$best_method), lag = 5),
      c(table(lag10$best_method), lag = 10),
      c(table(lag20$best_method), lag = 20))) %>%
  pivot_longer(cols = !lag, names_to = "best_method", values_to = "count")  %>%
  ggplot(aes(x = as.factor(lag), y = count,  fill = best_method)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(cb_colors[1], cb_colors[2],
                               cb_colors[3], cb_colors[4], "black")) +
  coord_flip() +
  scale_x_discrete(labels = c("Lag 5", "Lag 10", "Lag 20"))+
  theme_minimal() + 
  labs(x = "", y = "Count", fill = "Best Method") + 
  scale_y_continuous(breaks = seq(0, 55, 5)) +
  custom_theme

ggsave("output/best_method_by_lag.png", width = 12, height = 5)


average_rmses <- as.data.frame(rbind(
  c(lag5 %>%
    select(rmse_pred_Basic, rmse_pred_HLAGC, rmse_pred_HLAGELEM, rmse_pred_HLAGOO, rmse_pred_week_means) %>%
    colMeans, lag = 5),
  
  c(lag10 %>%
    select(rmse_pred_Basic, rmse_pred_HLAGC, rmse_pred_HLAGELEM, rmse_pred_HLAGOO, rmse_pred_week_means) %>%
    colMeans, lag = 10),
  
  c(lag20 %>%
    select(rmse_pred_Basic, rmse_pred_HLAGC, rmse_pred_HLAGELEM, rmse_pred_HLAGOO, rmse_pred_week_means) %>%
    colMeans, lag = 20)
)) %>%
  pivot_longer(!lag, names_to = "method", values_to = "avg_rmse")


average_rmses %>%
  ggplot(aes(x = lag, y = avg_rmse, color = method)) + 
  geom_point(size = 3) + 

  scale_color_manual(values = c(cb_colors[1], cb_colors[2],
                               cb_colors[3], cb_colors[4], "black")) +
  geom_line() +
  scale_y_continuous(breaks = seq(1.4, 2, .1), limits = c(NA, 1.9)) + 
  labs(y = "Average RMSE", title = "Average RMSE Across all Days", 
       subtitle = "For each Maximum Model Lag and BigVar Method", x ="")+
  geom_text(data = average_rmses[average_rmses$lag == 20,], 
    aes(x = lag +.5, y = avg_rmse, color = method, label = str_remove(method, "rmse_pred_")), hjust = "left", 
    fontface = "bold", size = 16/.pt) +
  custom_theme + 
  theme( plot.margin = margin(20, 40, 20, 40), 
         legend.position = "none") +
  scale_x_continuous(
    limits = c(5, 22),
    breaks = c(5, 10, 20),
    labels = c("Lag 5", "Lag 10", "Lag 20")) +
  coord_cartesian(
    clip = "off"
  ) 
ggsave("output/avg_rmse.png", width = 9, height = 5)

