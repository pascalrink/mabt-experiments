
# This script produces the numbers in Section 3.6.2 regarding the percentage of 
# MABT bounds that outperform the comparators in a per-data set comparison in 
# the random forest simulations

rm(list = ls())
graphics.off()

library(dplyr)
library(ggplot2)
library(ggpattern)

source("4-plots/myggsave-function.R")

rl_200 <- readRDS("6-additions/tuneranger-sim/tuneranger-sim-n200.RDS")
rl_400 <- readRDS("6-additions/tuneranger-sim/tuneranger-sim-n400.RDS")
rl_600 <- readRDS("6-additions/tuneranger-sim/tuneranger-sim-n600.RDS")

# ----

GenCompTable <- function(result_list) {
  
  .FormatResults <- function(.lelem) {
    .lelem$default <- data.frame(.lelem$default)
    .lelem$proposed <- data.frame(.lelem$proposed)
    .lelem$sidak <- data.frame(.lelem$sidak)
    df <- do.call(rbind, .lelem)
    df$covers <- df$ci.lwr < df$gt
    # df <- df[which(df$covers), ]
    if (nrow(df) == 0) {return(NULL)}
    df <- select(df, c(ci.method, ci.lwr, gt))
    return(df)
  }
  
  .GenCompTable <- function(.lelem) {
    .lelem$rank <- rank(1-.lelem$gt, ties.method = "max")
    Y <- expand.grid(.lelem$rank, .lelem$rank)
    methods <- .lelem$ci.method
    n_methods <- length(methods)
    Y$method1 <- rep(methods, each = n_methods)
    Y$method2 <- rep(methods, times = n_methods)
    Y <- Y[Y$method1 != Y$method2, ]
    Y$lbtr <- (Y$Var1 > Y$Var2) * 1.0  # > and >= yield same results
    return(Y)
  }
  
  df_list <- lapply(result_list, function(rl) {
    df <- .FormatResults(rl)
    if (is.null(df)) return(NULL)
    df$diff <- NA
    if ("mabt" %in% df$ci.method) df$diff <- df$gt[df$ci.method == "mabt"] - df$gt
    return(df)
  })
  table_list <- lapply(df_list, .GenCompTable)
  table_df <- do.call(rbind, table_list)
  table_df$comparison <- paste0(table_df$method1, " > ", table_df$method2)
  
  return(list(table_sum  = aggregate(lbtr ~ comparison, table_df, sum), 
              table_mean = aggregate(lbtr ~ comparison, table_df, mean),
              cis = df_list))
}

comp_tab_200 <- GenCompTable(rl_200)
comp_tab_400 <- GenCompTable(rl_400)
comp_tab_600 <- GenCompTable(rl_600)

comp_tab_200$table_sum[substr(comp_tab_200$table_sum$comparison, 1, 4) == "mabt", ]
comp_tab_400$table_sum[substr(comp_tab_400$table_sum$comparison, 1, 4) == "mabt", ]
comp_tab_600$table_sum[substr(comp_tab_600$table_sum$comparison, 1, 4) == "mabt", ]

comp_tab_200$table_mean[substr(comp_tab_200$table_mean$comparison, 1, 4) == "mabt", ]
comp_tab_400$table_mean[substr(comp_tab_400$table_mean$comparison, 1, 4) == "mabt", ]
comp_tab_600$table_mean[substr(comp_tab_600$table_mean$comparison, 1, 4) == "mabt", ]
