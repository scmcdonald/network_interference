library(igraph)
library(tidyverse)
library(BigVAR)
library(vars)
set.seed(4)

N = 10
s_epsilon = 0
s_alpha = 0
s_beta = 0

p = 1/N

rho_1 = runif(N, min = 0.7, max = 1.3)
rho_2 = 0


y_0 = rep(0, N)


alpha = rnorm(N, mean = 0, sd = s_alpha)
beta = rnorm(N, mean = 0, sd = s_beta)
epsilon = rnorm(N, mean = 0, sd = s_epsilon)


G <- erdos.renyi.game(N, p, type=c("gnp"), directed = FALSE, loops = F) %>%
  as_adjacency_matrix(sparse = F)


periods = 200
Y <- matrix(nrow = N, ncol = periods + 1)
colnames(Y) <- 0:periods

Y[, "0"] <- rnorm(N, mean = 0, sd = 1)

for(t1 in 1:periods){
  
  t = t1 - 1
  
  y_t <- Y[, paste(t)]
  
  
  y_t1 <- alpha + beta + rho_1 * y_t + (rho_2 * G %*% y_t )+ epsilon
  
  Y[, paste(t1)] <- y_t1
  
}

identity = diag(N)

comparison_matrix = ((rho_1 * identity) +( rho_2 *G))

# big var
big_var_model=BigVAR.fit(t(Y),p = 1,
                 struct = "Basic",
                 lambda=1e-20, 
                 intercept=F)

big_var_mod_subset <- big_var_model[,,1]



# var
model <- VAR(t(Y), p = 1)

model_matrix <-  t(as.data.frame(lapply(model$varresult, `[[`, 1)))



rownames(model_matrix) <- paste("y", 1:N, sep = "")

rho_1
comparison_matrix
model_matrix
round(model_matrix, 10)

big_var_mod_subset
round(big_var_mod_subset, 10)




