
rm(list = ls())
graphics.off()

library(dplyr)
library(ggplot2)

OMLResultsDF <- function(lst) {
  df <- lapply(lst, function(rl) {
    rl <- rl$df
    mabt <- rl$bound[rl$method == "mabt"] %>% as.numeric
    rl$diff <- mabt - as.numeric(rl$bound)
    rl$max.diff <- mabt - as.numeric(max(rl$bound[rl$method != "mabt"]))
    return(rl)}) %>% do.call(rbind, .)
  return(df)
}

GenCountTable <- function(lelem) {
  lelem <- lelem$df
  lelem$rank <- rank(1-as.numeric(lelem$bound), ties.method = "max")
  Y <- expand.grid(lelem$rank, lelem$rank)
  methods <- lelem$method
  n_methods <- length(methods)
  Y$method1 <- rep(methods, each = n_methods)
  Y$method2 <- rep(methods, times = n_methods)
  Y <- Y[Y$method1 != Y$method2, ]
  Y$lbtr <- (Y$Var1 > Y$Var2) * 1.0
  return(Y)
}

# multiclass results ----

oml.results.multicls <- readRDS("6-revision/openml-benchmark-multi-cls.RDS")
oml.multi.df <- OMLResultsDF(oml.results.multicls)
aggregate(diff ~ method, oml.multi.df, summary)
subset(oml.multi.df, method != "mabt") %>% 
  ggplot(aes(x = method, y = diff)) + geom_point()

oml.multi.count.df <- lapply(oml.results.multicls, GenCountTable) %>% 
  do.call(rbind, .)
oml.multi.count.df$comp <- paste0(
  oml.multi.count.df$method1, " > ", oml.multi.count.df$method2)
subset(oml.multi.count.df, method1 == "mabt") %>% 
  aggregate(lbtr ~ comp, ., sum)

# two class results ----

oml.results.2cls <- readRDS("6-revision/openml-benchmark-2-cls.RDS")
oml.2cls.df <- OMLResultsDF(oml.results.2cls)
aggregate(diff ~ method, oml.2cls.df, summary)
subset(oml.2cls.df, method != "mabt") %>% 
  ggplot(aes(x = method, y = diff)) + geom_point()

oml.2cls.count.df <- lapply(oml.results.2cls, GenCountTable) %>% 
  do.call(rbind, .)
oml.2cls.count.df$comp <- paste0(
  oml.2cls.count.df$method1, " > ", oml.2cls.count.df$method2)
subset(oml.2cls.count.df, method1 == "mabt") %>% 
  aggregate(lbtr ~ comp, ., sum)

