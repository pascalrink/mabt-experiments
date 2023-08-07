
rm(list = ls())
graphics.off()

library(dplyr)
library(doParallel)

n_cores <- 60
n_runs <- 5000

par_clust <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(par_clust)

n_obs_range <- c(200, 400)
sapply(n_obs_range, function(n_obs) {
  
  feats_type <- "caret"
  n_groundtruth <- 20000
  n_feats <- 1000
  n_models <- 100
  measure <- "class"
  n_folds <- 10
  alpha <- 0.05
  
  bbccv_Sim <- function(seed, n_obs) {
    
    set.seed(seed)
    
    n_train <- n_obs/2
    n_valid <- n_obs/4
    n_eval <- n_obs/4
    
    source("2-models/data-generation.R")
    dat <- GenerateData(
      feats_type, n_train, n_valid, n_eval, n_groundtruth, n_feats)
    X <- rbind(dat$feats$train, dat$feats$valid, dat$feats$eval) %>% as.matrix
    Y <- c(dat$labs$train, dat$labs$valid, dat$labs$eval)
    
    init_fit <- glmnet::glmnet(X, Y, family = "binomial", nlambda = n_models)
    lambdas <- init_fit$lambda
    
    fit <- glmnet::cv.glmnet(X, Y, family = "binomial", lambda = lambdas, 
                             type.measure = measure, nfolds = n_folds)
    gt_preds <- as.matrix(dat$feats$groundtruth) %>% 
      predict(fit, ., s = lambdas, type = "class")
    gt_similar <- gt_preds == dat$labs$groundtruth
    gt_performs <- colMeans(gt_similar) %>% as.numeric
    
    source("6-additions/bbccv-comparison/BBCCV-function.R")
    bbccv_result <- BBCCV(nrow(X), gt_similar, 10000)
    bbccv_lwr_bound <- bbccv_result$out_samples_performances %>% quantile(alpha)
    select_gt_perf <- gt_performs[match(fit$lambda.min, lambdas)]
    
    return(list(true_performance = select_gt_perf, lwr_bound = bbccv_lwr_bound)) 
  }
  
  t0 <- Sys.time()
  bbccv_sim_results <- foreach::foreach(
    seed = seq(n_runs), .packages = c("dplyr")) %dopar% {
      bbccv_Sim(seed, n_obs)
    }
  t1 <- Sys.time()
  t1-t0
  paste0("6-additions/bbccv-comparison/bbccv-sim-n", n_obs, ".RDS") %>% 
    saveRDS(bbccv_sim_results, .)})
parallel::stopCluster(par_clust)
