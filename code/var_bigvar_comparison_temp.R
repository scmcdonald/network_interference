Y1 = Y[, "1"]; Y1
Y2 = Y[, "2"]; Y2
rho_1

epsilon

y2 = rho_1 * Y1 + epsilon; y2
```{r, warning=F, message=F}
N_list <- c(4, 10, 20, 25)
p_num_list <- c(1, 3)

rho_1_list <- c("unif_0.7_1.3", "unif_0.95_1.05", "unif_-1_1", "std_normal")
t_type_list <- c("N", "N-squared")

df <- expand.grid(N = N_list,
                  rho_1 = rho_1_list, 
                  p_num = p_num_list,
                  t_type = t_type_list, 
                  t = NA, 
                  NA_cols = NA, 
                  large_coefs = NA, 
                  NA_constant_cols = NA,
                  rho_2_mod=0,  s_alpha_input = 0, s_epsilon_input = 0)%>%
  mutate(t = case_when(t_type== "N" ~ N + 2,
                       t_type== "N-squared" ~ N^2 + N+ 1))



for(i in 1:nrow(df)){
  set.seed(5)
  
  # compute rho1 values
  rho_1_value <- if(df[i, "rho_1"] == "unif_-1_1"){
    runif(df[i, "N"], -1, 1)
  } else if(df[i, "rho_1"] == "unif_0.7_1.3"){
    runif(df[i, "N"], 0.7, 1.3)
  } else if(df[i, "rho_1"] == "unif_0.95_1.05"){
    runif(df[i, "N"], 0.95, 1.05)
  } else if(df[i, "rho_1"] =="std_normal"){
    rnorm(df[i, "N"])
  }
  
  
  # get model
  m <- models(n = df[i, "N"], t = df[i, "t"], 
              rho_1_mod = rho_1_value, 
              rho_2_mod=df[i, "rho_2_mod"], 
              p_num = df[i, "p_num"], 
              s_alpha_input = df[i, "s_alpha_input"], 
              s_epsilon_input = df[i, "s_epsilon_input"])[[4]][[2]]
  
  class(m) <- "numeric"
  
  # number of large coefficients
  df[i, "large_coefs"] <- sum(abs(m)> 10, na.rm = T)
  
  # number of NA columns
  df[i, "NA_cols"] <- length(colnames(m)[colSums(is.na(m)) > 0])
  
  # number of NA columns
  df[i, "NA_constant_cols"] <- sum(is.na(m[, "const"])) == length(m[, "const"])
  
}

df <- df %>% 
  relocate(N, rho_1, rho_2_mod, t_type, t, p_num, s_epsilon_input, s_alpha_input, NA_cols, large_coefs) %>%
  arrange(N, rho_1, p_num, t_type, t, rho_2_mod,  s_epsilon_input)

kable(df) %>%
  kable_styling() %>%
  row_spec(which(df$NA_cols == 0 & df$large_coefs == 0), background = "lightblue") %>%
  scroll_box(height = "500px")
```