# Average all matrices across all dates for each specification

library(here)
#specifications
lag = c(5, 10, 15, 20)
method = c("Basic", "HLAGC", "HLAGELEM", "HLAGOO")


for(l in lag){
  for(m in method){
    x <- lapply(list.files(path = here("data", "coefficients"), 
                           pattern = paste("*L", l, "_", m, ".rds", sep = ""), full.names = T), function(x)readRDS(x))
    
    y <- Reduce("+", x) / length(x)
    
    saveRDS(y, here("data", "coefficients", paste("avg", "L", l, "_", m, ".rds", sep = "")))
  }
}






