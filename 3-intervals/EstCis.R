
rm(list = ls())
graphics.off()

library(dplyr)
library(doParallel)

# Specify simulation parameters here
alpha <- 0.05
n_cores <- 2

EstCis <- function(fpath, alpha, n_boots, n_cores) {
  
  filenames <- paste0("1-savefiles/data/", fpath) %>% list.files
  
  par_clust <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(par_clust)
  
  foreach::foreach(fn = filenames, .packages = c("dplyr")) %dopar% {
    
    source("3-intervals/bootstrap.R")
    source("3-intervals/default-bounds.R")
    source("3-intervals/default-tilting.R")
    source("3-intervals/proposed-tilting.R")
    
    fn_length <- nchar(fn)
    fn_split_id <- 9
    prefix <- substr(fn, 1, fn_length - fn_split_id)
    suffix <- substr(fn, fn_length - fn_split_id + 1, fn_length)
    
    t0 <- Sys.time()
    inst <- paste0("1-savefiles/data/", fpath, fn) %>% readRDS
    set.seed(inst$params$seed)
    boot <- Bootstrap(inst, n_boots)
    rules <- names(inst$select$eval)
    names(rules) <- rules
    
    std <- lapply(
      rules, function(.rule) StandardBounds(inst, .rule, alpha)) %>% 
      do.call(rbind, .)
    default_tilt <- lapply(
      rules, function(.rule) DefaultTiltBound(inst, boot, .rule, alpha)) %>% 
      do.call(rbind, .)
    proposed_tilt <- rules[which(names(rules) != "best")] %>% 
      lapply(function(.rule) ProposedTiltBound(
        inst, boot, .rule, alpha, fpath, prefix, suffix)) %>% 
      do.call(rbind, .)
    t1 <- Sys.time()
    
    params <- list(full_alpha = alpha, n_boots = n_boots, 
                   ci_runtime = lubridate::second(t1) - lubridate::second(t0))
    inst$params <- c(inst$params, params)
    bounds <- rbind(std, default_tilt, proposed_tilt)
    this <- data.frame(bounds, inst$params)
    rownames(this) <- NULL
    this$covers <- this$bound < this$groundtruth
    
    this_fn <- paste0(fpath, prefix, "cis-", suffix)
    paste0("1-savefiles/cis/", this_fn) %>% saveRDS(this, .)
    
  }
  
  parallel::stopCluster(par_clust)
}


####################################################
#################### DO NOT RUN ####################
####################################################

# # 1 ----
# EstCis(fpath = "class/normal/n200/nocv/", alpha = alpha, n_boots = 10000, n_cores = n_cores)
# 
# # 2 ----
# EstCis(fpath = "class/normal/n200/cv/", alpha = alpha, n_boots = 10000, n_cores = n_cores)
# 
# # 3 ----
# EstCis(fpath = "class/normal/n400/nocv/", alpha = alpha, n_boots = 10000, n_cores = n_cores)
# 
# # 4 ----
# EstCis(fpath = "class/normal/n400/cv/", alpha = alpha, n_boots = 10000, n_cores = n_cores)
# 
# # 5 ----
# EstCis(fpath = "auc/normal/n400/cv/", alpha = alpha, n_boots = 2000, n_cores = n_cores)
# 
# # 6 ----
# EstCis(fpath = "class/caret/n200/nocv/", alpha = alpha, n_boots = 10000, n_cores = n_cores)
# 
# # 7 ----
# EstCis(fpath = "class/caret/n200/cv/", alpha = alpha, n_boots = 10000, n_cores = n_cores)
# 
# # 8 ----
# EstCis(fpath = "class/caret/n400/nocv/", alpha = alpha, n_boots = 10000, n_cores = n_cores)
# 
# # 9 ----
# EstCis(fpath = "class/caret/n400/cv/", alpha = alpha, n_boots = 10000, n_cores = n_cores)
# 
# # 10 ----
# EstCis(fpath = "auc/caret/n400/cv/", alpha = alpha, n_boots = 2000, n_cores = n_cores)
# 
# # 11 ----
# EstCis(fpath = "auc/normal/n600/cv/", alpha = alpha, n_boots = 2000, n_cores = n_cores)
# 
# # 12 ----
# EstCis(fpath = "auc/caret/n600/cv/", alpha = alpha, n_boots = 2000, n_cores = n_cores)


