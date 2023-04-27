
rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(doParallel)

dat <- read.csv2("~/Downloads/bank-additional/bank-additional-full.csv")
dat <- select(dat, -c(contact, month, day_of_week, duration))
dat$y <- factor((dat$y == "yes") * 1.0)

dat$job <- factor(dat$job)
dat$marital <- factor(dat$marital)
dat$education <- factor(dat$education)
dat$default <- factor(dat$default)
dat$housing <- factor(dat$housing)
dat$loan <- factor(dat$loan)
dat$pdays <- factor((dat$pdays < 999) * 1.0)
dat$previous <- factor((dat$previous > 0) * 1.0)
dat$poutcome <- factor(dat$poutcome)

y0_ids <- which(dat$y == 0)
y1_ids <- which(dat$y == 1)

n0 <- 500
n1 <- 500

BankDataSim <- function(seed) {
  set.seed(seed)
  
  learn_0_ids <- sample(y0_ids, n0)
  learn_1_ids <- sample(y1_ids, n1)
  
  learn_ids <- c(learn_0_ids, learn_1_ids)
  learn_dat <- dat[learn_ids, ]
  
  eval_0_ids <- setdiff(y0_ids, learn_0_ids) %>% sample(n0)
  eval_1_ids <- setdiff(y1_ids, learn_1_ids) %>% sample(n1)
  eval_ids <- c(eval_0_ids, eval_1_ids)
  eval_dat <- dat[eval_ids, ]
  
  tune_grid <- expand.grid(mtry = c(2, 4, 6, 8, 10, 12), 
                           ntree = c(10, 50, 100, 200), 
                           replace = c(TRUE, FALSE))
  n_tune <- nrow(tune_grid)
  
  learn_models <- list()
  learn_sensi <- rep(NA, n_tune)
  for (t in seq(n_tune)) {
    rfm <- randomForest::randomForest(
      y ~ ., learn_dat, mtry = tune_grid$mtry[t], ntree = tune_grid$ntree[t],
      replace = tune_grid$replace[t])
    learn_models[[t]] <- rfm
    learn_sensi[t] <- 1-rfm$confusion[2, 3]
  }
  
  top_10p_select <- which(learn_sensi >= quantile(learn_sensi, 0.9))
  best_learn_model_id <- which.max(learn_sensi)
  best_learn_model_relative_id <- which(top_10p_select == best_learn_model_id)
  
  eval_labs <- eval_dat[, "y"] %>% factor
  
  top_10p_eval_results <- lapply((top_10p_select), function(i) {
    rfm <- learn_models[[i]]
    preds <- predict(rfm, eval_dat)
    sensi <- caret::sensitivity(preds, eval_labs, positive = "1")
    return(list(sensi = sensi, preds = preds))})
  
  eval_sensi <- sapply(top_10p_eval_results, function(lelem) lelem$sensi)
  eval_preds <- (sapply(
    top_10p_eval_results, function(lelem) lelem$preds) == 1) * 1.0
  
  best_eval_model_relative_id <- which.max(eval_sensi)
  best_eval_model_id <- top_10p_select[best_eval_model_relative_id]
  best_eval_model_eval_sensi <- eval_sensi[best_eval_model_relative_id]
  best_learn_model_eval_sensi <- eval_sensi[best_learn_model_relative_id]
  
  ###
  test_ids <- seq(nrow(dat)) %>% setdiff(c(learn_ids, eval_ids))
  test_dat <- dat[test_ids, ]
  test_labs <- test_dat[, "y"] %>% factor
  
  best_learn_model_test_sensi <- predict(
    learn_models[[best_learn_model_id]], test_dat) %>% 
    caret::sensitivity(test_labs, positive = "1")
  best_eval_model_test_sensi <- predict(
    learn_models[[best_eval_model_id]], test_dat) %>% 
    caret::sensitivity(test_labs, positive = "1")
  
  ##### 
  eval_preds <- eval_preds[eval_labs == 1, ]
  eval_boot <- boot::boot(eval_preds, function(x, i) colMeans(x[i, ]), R = 10000)
  alpha <- 0.05
  measure <- "class"
  cls_thresh <- 0.5
  
  source("5-example/MabtCi-function.R")
  labs <- eval_labs[eval_labs == 1]
  mabt_ci <- MabtCi(eval_boot, alpha, measure, best_eval_model_relative_id, eval_preds, cls_thresh, labs)
  mabt_result <- data.frame(seed = seed, 
                            method = "mabt", 
                            lwr_bound = mabt_ci, 
                            est = best_eval_model_eval_sensi, 
                            truth = best_eval_model_test_sensi)
  
  
  # default intervals
  
  n_success <- eval_preds[, best_learn_model_relative_id] %>% sum
  default_cis <- DescTools::BinomCI(
    n_success, length(labs), conf.level = 0.95, sides = c("left"), 
    method = c("wilson", "wald", "clopper-pearson"))
  default_result <- data.frame(seed = seed, 
                               method = rownames(default_cis), 
                               lwr_bound = unname(default_cis[, 2]), 
                               est = best_learn_model_eval_sensi, 
                               truth = best_learn_model_test_sensi)
  
  print(seed)
  return(rbind(mabt_result, default_result))
}

n_cores <- 50
n_runs <- 1000
par_clust <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(par_clust)

t0 <- Sys.time()
result_list <- foreach::foreach(seed = seq(n_runs), .packages = c("dplyr")) %dopar% {
  return(BankDataSim(seed))
}
saveRDS(result_list, "bank-sim-result.RDS")
t1 <- Sys.time()
t1-t0

parallel::stopCluster(par_clust)

