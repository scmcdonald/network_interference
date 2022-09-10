
output <- dgp(N = 5, 
              periods_type = "plus2", 
              p_num = "low", 
              s_epsilon = 0,  
              s_alpha =  0, 
              s_beta = 0, 
              rho_1 = 0.3,  
              rho_2 = "depends on rho_1", 
              y_initial = "standard normal", 
              seed = 1
)

output


output$rho_1 + (output$d_max * output$rho_2)

out <- ridge(output, lambda = 0.0001, theta = 0.1)

out



new_matrix = out$A - (as.numeric(output$rho_1) * diag(5)) / as.numeric(output$rho_2)

ifelse(abs(new_matrix ) > 0.1, 1, 0)

out$comparison_matrix

disagreements(out$comparison_matrix, ifelse(abs(new_matrix ) > 0.1, 1, 0)
, 5)

estimated = out$A 

original = as.numeric(output$rho_1) * diag(5) + as.numeric(output$rho_2) * output$G

sum(estimated - original)/25





