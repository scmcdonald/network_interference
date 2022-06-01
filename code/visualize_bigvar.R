library(tidyverse)
library(ggplot2)
library(RColorBrewer)

cb_colors <- brewer.pal(n = 8, name = "Dark2")

# lag = 5
compare_1 <- read.csv(here("data", "rmse_out", "out_10_07_2021_10_17_2021.csv"))
compare_2 <- read.csv(here("data", "rmse_out", "out_10_18_2021_11_06_2021.csv"))
compare_3 <- read.csv(here("data", "rmse_out", "out_11_07_2021_11_18_2021.csv"))
compare_4 <- read.csv(here("data", "rmse_out", "out_11_19_2021_11_30_2021.csv"))

comparison <- list(compare_1, compare_2, compare_3, compare_4) %>%
  do.call(rbind, .) %>%
  arrange(prediction_date)


comparison$prediction_date <- as.Date(comparison$week_end) + 7


text_color <- comparison %>%
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


comparison %>%
  select(prediction_date, rmse_pred_Basic, rmse_pred_HLAGC, rmse_pred_HLAGELEM, rmse_pred_HLAGOO, rmse_pred_week_means) %>%
  pivot_longer(!prediction_date, names_to = "rmse_name", values_to = "rmse_value") %>%
  group_by(prediction_date) %>%
  mutate(rmse_min= ifelse(rmse_value == min(rmse_value), 1, 0), 
         best_rmse =rmse_name[which.max(rmse_min)]) %>%
  ggplot(aes(x = as.numeric(prediction_date), y = rmse_value, color = rmse_name)) +
  geom_point(size = 3) + 
  scale_color_manual(name = "RMSE", values = c(cb_colors[1:4], "black"), 
                     labels = c("Basic", "HLAGC", "HALGELEM", "HLAGOO", "Week Means")) +
  scale_x_reverse(labels = as.character(format(comparison$prediction_date, format = "%b %d")),                     
                  breaks = as.numeric(comparison$prediction_date)) + 
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
compare_10 <- read.csv(here("data", "rmse_out", "out_2021-10-07_2021-10-31_10.csv"))
compare_10_2 <- read.csv(here("data", "rmse_out", "out_2021-11-01_2021-11-30_10.csv"))
compare_10$week_end <- as.Date(x = compare_10$week_end, origin = "1970-01-01")  

compare_10_2$week_end <- as.Date(x = compare_10_2$week_end, origin = "1970-01-01")  

comparison_2 <- rbind(compare_10, compare_10_2)

comparison_2$prediction_date <- as.Date(comparison_2$week_end) + 7


text_color <- comparison_2 %>%
  select(prediction_date, best_method) %>%
  mutate(color = case_when(best_method == "week_means" ~ "black",
                           best_method == "Basic" ~ cb_colors[1],
                           best_method == "HLAGC" ~ cb_colors[2],
                           best_method == "HLAGELEM" ~ cb_colors[3],
                           best_method == "HLAGOO" ~ cb_colors[4],
                           TRUE ~ "red")) %>%
  distinct()

comparison_2 %>%
  select(prediction_date, rmse_pred_Basic, rmse_pred_HLAGC, rmse_pred_HLAGELEM, rmse_pred_HLAGOO, rmse_pred_week_means) %>%
  pivot_longer(!prediction_date, names_to = "rmse_name", values_to = "rmse_value") %>%
  group_by(prediction_date) %>%
  mutate(rmse_min= ifelse(rmse_value == min(rmse_value), 1, 0), 
         best_rmse =rmse_name[which.max(rmse_min)]) %>%
  ggplot(aes(x = as.numeric(prediction_date), y = rmse_value, color = rmse_name)) +
  geom_point(size = 3) + 
  scale_color_manual(name = "RMSE", values = c(cb_colors[1:4], "black"), 
                     labels = c("Basic", "HLAGC", "HALGELEM", "HLAGOO", "Week Means")) +
  scale_x_reverse(labels = as.character(format(comparison_2$prediction_date, format = "%b %d")),                     
                  breaks = as.numeric(comparison_2$prediction_date)) + 
  scale_y_continuous(limits = c(0,5))+ 
  theme_minimal() +
  coord_flip()  +
  labs(x = "Date (2021)", 
       y = "RMSE", 
       title = "RMSE for VAR(10) by Prediction Date") +
  theme(axis.text.y = element_text(color =text_color$color, face = "bold"), 
        plot.title.position = "plot")
ggsave("output/bigvar_rmse_10.png", width = 7, height = 12)


#650x850