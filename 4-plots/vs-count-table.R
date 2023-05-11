
# This script ...

rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(ggplot2)

cis_path_list <- list("1-savefiles/cis/class/normal/n200/cv/", 
                      # "1-savefiles/cis/class/normal/n200/nocv/", 
                      "1-savefiles/cis/class/normal/n400/cv/", 
                      # "1-savefiles/cis/class/normal/n400/nocv/", 
                      "1-savefiles/cis/class/caret/n200/cv/", 
                      # "1-savefiles/cis/class/caret/n200/nocv/", 
                      "1-savefiles/cis/class/caret/n400/cv/", 
                      # "1-savefiles/cis/class/caret/n400/nocv/", 
                      "1-savefiles/cis/auc/normal/n400/cv/", 
                      "1-savefiles/cis/auc/normal/n600/cv/", 
                      "1-savefiles/cis/auc/caret/n400/cv/", 
                      "1-savefiles/cis/auc/caret/n600/cv/")

TableFn <- function(lelem) {
  lelem <- subset(lelem, (rule == "best" | (method == "mabt" & rule == "se")))
  
  lelem$covers <- (lelem$bound < lelem$groundtruth) * 1.0
  lelem$copy_of_bound <- lelem$bound
  lelem$copy_of_bound[lelem$covers == 0] <- 0
  lelem$rank <- rank(1-lelem$copy_of_bound, ties.method = "max")
  lelem$copy_of_bound <- NULL
  
  Y <- expand.grid(lelem$rank, lelem$rank)
  methods <- lelem$method
  n_methods <- length(methods)
  Y$method1 <- rep(methods, each = n_methods)
  Y$method2 <- rep(methods, times = n_methods)
  Y <- Y[Y$method1 != Y$method2, ]
  Y$lbtr <- (Y$Var1 > Y$Var2) * 1.0
  
  return(Y)
}

GG <- function(i) {
  
  cis_path <- cis_path_list[[i]]
  cis_files <- list.files(cis_path)
  
  df_list <- lapply(cis_files, function(.fn) {paste0(cis_path, .fn) %>% 
      readRDS})
  
  vs_tab <- lapply(df_list, TableFn) %>% do.call(rbind, .)
  vs_tab$mvm <- paste0(vs_tab$method1, " > ", vs_tab$method2)
  count_tab <- aggregate(lbtr ~ mvm, vs_tab, sum)
  count_tab$perc <- count_tab$lbtr / 5000
  return(count_tab)
}

count_mat_list <- seq(cis_path_list) %>% lapply(GG)



