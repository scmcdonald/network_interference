state = "CA"
method = "HLAGC"
threshold = 0.01
lag = 20


files <- list.files(path = "data/coefficients", pattern = paste("L", lag, "_", method, "\\.rds", sep = ""), full.names = T)
names <- str_extract(pattern = "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]", files)

matrices <- lapply(setNames(files,names),  readRDS)


new_list <- lapply(matrices, FUN = function(x)x[rownames(x) == state, ])

test <- purrr::map_df(new_list, data.frame, .id = 'name') %>%
  select(!intercept)
  rownames(test) <- NULL
test <-   test %>%column_to_rownames("name")
test[abs(test) < threshold] = NA
test2 <- janitor::remove_empty(test, which = "cols")



ggplot(test2, aes(x = ))






