
# This script produces the numbers in Table 3 in the main document (percentage 
# of MABT bounds that outperform the comparators in a per-data set comparison in 
# the lasso simulations)

rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(ggplot2)
library(ggpattern)

cis_path <- "1-savefiles/cis/"
cis_files <- list.files(cis_path, recursive = TRUE)

GenTable <- function(tbl_measure, tbl_feats_type, tbl_n_eval, tbl_rule, tbl_cv) {
  
  paste(tbl_measure, tbl_feats_type, "n_eval =", tbl_n_eval, "rule =", tbl_rule, "cv =", tbl_cv)
  
  df_list <- lapply(cis_files, function(.fn) {
    .df <- paste0(cis_path, .fn) %>% 
      readRDS
    .df <- .df[which(.df$covers), ]
    .df <- subset(.df, feats_type == tbl_feats_type & cv == tbl_cv & (method == "mabt" & rule == tbl_rule | rule == "best") & n_eval == tbl_n_eval)
    .df <- subset(.df, measure == tbl_measure)
    if (nrow(.df) == 0) {return(NULL)}
    .df <- subset(.df, select = c(method, rule, bound, n_eval, cv, measure, feats_type))
    return(.df)
  })
  
  .GenCompTable <- function(.lelem) {
    .lelem$rank <- rank(1-.lelem$bound, ties.method = "max")
    Y <- expand.grid(.lelem$rank, .lelem$rank)
    methods <- paste(.lelem$method, .lelem$rule, .lelem$n_eval, .lelem$cv, .lelem$measure, sep = "+")
    n_methods <- length(methods)
    Y$method1 <- rep(methods, each = n_methods)
    Y$method2 <- rep(methods, times = n_methods)
    Y <- Y[Y$method1 != Y$method2, ]
    Y$lbtr <- (Y$Var1 > Y$Var2) * 1.0 
    return(Y)
  }
  
  table_list <- lapply(df_list, .GenCompTable)
  table_df <- do.call(rbind, table_list)
  table_df$comparison <- paste0(table_df$method1, " > ", table_df$method2)
  
  table_sum <- aggregate(lbtr ~ comparison, table_df, sum)
  table_mean <- aggregate(lbtr ~ comparison, table_df, mean)
  
  mts <- table_sum[substr(table_sum$comparison, 1, 4) == "mabt", ]
  mtm <- table_mean[substr(table_mean$comparison, 1, 4) == "mabt", ]
  
  # aufsteigend ordnen
  # mts[order(mts$lbtr), ]
  return(mtm[order(mtm$lbtr), ])
  
}

GenTable("class", "normal", 50, "se", TRUE)
#                                                    comparison      lbtr
# 11            mabt+se+50+TRUE+class > wald+best+50+TRUE+class 0.3974082
# 9               mabt+se+50+TRUE+class > bt+best+50+TRUE+class 0.4904255
# 12          mabt+se+50+TRUE+class > wilson+best+50+TRUE+class 0.5113709
# 10 mabt+se+50+TRUE+class > clopper-pearson+best+50+TRUE+class 0.6940582


GenTable("class", "normal", 100, "se", TRUE)
# 11            mabt+se+100+TRUE+class > wald+best+100+TRUE+class 0.3136236
# 9               mabt+se+100+TRUE+class > bt+best+100+TRUE+class 0.4636170
# 12          mabt+se+100+TRUE+class > wilson+best+100+TRUE+class 0.5424448
# 10 mabt+se+100+TRUE+class > clopper-pearson+best+100+TRUE+class 0.6651163

GenTable("class", "caret", 50, "se", TRUE)
# 11            mabt+se+50+TRUE+class > wald+best+50+TRUE+class 0.4144222
# 9               mabt+se+50+TRUE+class > bt+best+50+TRUE+class 0.4864403
# 12          mabt+se+50+TRUE+class > wilson+best+50+TRUE+class 0.4945594
# 10 mabt+se+50+TRUE+class > clopper-pearson+best+50+TRUE+class 0.7052342

GenTable("class", "caret", 100, "se", TRUE)
# 11            mabt+se+100+TRUE+class > wald+best+100+TRUE+class 0.3355517
# 9               mabt+se+100+TRUE+class > bt+best+100+TRUE+class 0.4687039
# 12          mabt+se+100+TRUE+class > wilson+best+100+TRUE+class 0.5374579
# 10 mabt+se+100+TRUE+class > clopper-pearson+best+100+TRUE+class 0.6624764
