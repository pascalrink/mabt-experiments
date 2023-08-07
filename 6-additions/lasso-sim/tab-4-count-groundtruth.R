
# This script produces the numbers in Table 4 in the main document (percentage 
# of final models from the proposed pipeline that outperform the final model 
# from the default pipeline in a per-data set comparison in the lasso 
# simulations)

rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(ggplot2)
library(ggpattern)

cis_path <- "1-savefiles/cis/"
cis_files <- list.files(cis_path, recursive = TRUE)

df_list <- lapply(cis_files, function(.fn) {
  .df <- paste0(cis_path, .fn) %>% 
    readRDS
  # .df <- .df[which(.df$covers), ]
  .df <- subset(.df, cv == TRUE & (method == "mabt" | method == "delong" & rule == "best") & n_eval == 150)  # specify method ("wilson" or "delong") and n_eval (50, 100, 150) here
  .df <- subset(.df, measure == "auc")  # specify measure here ("class" or "auc")
  if (nrow(.df) == 0) {return(NULL)}
  .df <- subset(.df, select = c(method, rule, groundtruth, n_eval, cv, measure, feats_type))
  return(.df)
})

.GenCompTable <- function(.lelem) {
  .lelem$rank <- rank(1-.lelem$groundtruth, ties.method = "max")
  Y <- expand.grid(.lelem$rank, .lelem$rank)
  methods <- paste(.lelem$method, .lelem$rule, .lelem$n_eval, .lelem$cv, .lelem$measure, sep = "+")
  n_methods <- length(methods)
  Y$method1 <- rep(methods, each = n_methods)
  Y$method2 <- rep(methods, times = n_methods)
  Y <- Y[Y$method1 != Y$method2, ]
  Y$lbtr <- (Y$Var1 > Y$Var2) * 1.0   # > or >=
  return(Y)
}

table_list <- lapply(df_list, .GenCompTable)
table_df <- do.call(rbind, table_list)
table_df$comparison <- paste0(table_df$method1, " > ", table_df$method2)

table_sum <- aggregate(lbtr ~ comparison, table_df, sum)
table_mean <- aggregate(lbtr ~ comparison, table_df, mean)

mts <- table_sum[substr(table_sum$comparison, 1, 4) == "mabt", ]
mtm <- table_mean[substr(table_mean$comparison, 1, 4) == "mabt", ]

mts[order(mts$lbtr), ]
mtm[order(mtm$lbtr), ]
