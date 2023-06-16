
rm(list = ls())
graphics.off()

library(dplyr)
library(ggplot2)

repeated <- FALSE # or FALSE for single
multi.2cls.switch <- "2cls" # "2cls" or "multi"

oml <- switch(
  multi.2cls.switch, 
  "multi" = "6-revision/openml-benchmark/openml-benchmark-multi-cls-repeated.RDS", 
  "2cls"  = "6-revision/openml-benchmark/openml-benchmark-2-cls-repeated.RDS") %>% 
  readRDS()
if (!repeated) oml <- oml[1]

n.1 <- length(oml[[1]])
n.2 <- length(oml)

mean.results <- list()
for (i in 1:n.1) {
  lst <- list()
  for (j in 1:n.2) {
    lst <- c(lst, list(oml[[j]][[i]]$df))
  }
  lst.df <- do.call(rbind, lst)
  mean.df <- aggregate(as.numeric(bound) ~ task.id + method + n.classes, lst.df, mean) %>% 
    rename(bound = "as.numeric(bound)")
  mabt <- mean.df$bound[mean.df$method == "mabt"] %>% as.numeric
  mean.df$diff <- mabt - mean.df$bound
  mean.df$max.diff <- mabt - max(mean.df$bound[mean.df$method != "mabt"])
  mean.results <- c(mean.results, list(mean.df))
}

mean.results.df <- do.call(rbind, mean.results)

aggregate(diff ~ method, mean.results.df, summary)
subset(mean.results.df, method != "mabt") %>% 
  ggplot(aes(x = method, y = diff)) + geom_point()

mean.results.count <- lapply(mean.results, function(mr) {
  mr$rank <- rank(1-mr$bound, ties.method = "max")
  y <- expand.grid(mr$rank, mr$rank)
  methods <- mr$method
  n.methods <- length(methods)
  y$method1 <- rep(methods, each = n.methods)
  y$method2 <- rep(methods, times = n.methods)
  y <- y[y$method1 != y$method2, ]
  y$lbtr <- (y$Var1 > y$Var2) * 1.0
  return(y)
}) %>% do.call(rbind, .)

mean.results.count$comp <- paste0(
  mean.results.count$method1, " > ", mean.results.count$method2)
subset(mean.results.count, method1 == "mabt") %>% 
  aggregate(lbtr ~ comp, ., mean)




