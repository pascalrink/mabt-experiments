
rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(doParallel)

source("5-example/heart-example-helper-functions.R")
source("5-example/GenCompTable-function.R")

n_cores <- 50
n_runs <- 500

dat <- GetHeartData()

HeartExampleAccuracy <- function(seed) {
  set.seed(seed)

  learn_ids <- nrow(dat) %>% sample(2/3 * nrow(dat))
  learn_dat <- dat[learn_ids, ]
  eval_dat <- dat[-learn_ids, ]
  folds <- caret::createFolds(learn_dat$num, list = FALSE)
  tune_grid <- expand.grid(
    mtry = 1:5 * 2, ntree = 1:5 * 100, replace = c(TRUE, FALSE))

  acc_cv <- apply(tune_grid, 1, function(tg) {
    acc <- rep(NA, 10)
    for (fold in 1:10) {
      rf <- randomForest::randomForest(
        num ~ ., learn_dat[folds!=fold, ],
        mtry = tg[1], ntree = tg[2], replace = tg[3])
      prd <- predict(rf, learn_dat[folds==fold, ], "class")
      acc[fold] <- mean(learn_dat[folds==fold, "num"] == prd)
    }
    return(mean(acc))})

  best_learn_model_id <- which.max(acc_cv)
  top_10p_selection <- which(acc_cv >= quantile(acc_cv, 0.9))

  learn_results <- data.frame(
    id = top_10p_selection, learn_acc = acc_cv[top_10p_selection])
  learn_results$learn_rank <- rank(
    1-learn_results$learn_acc, ties.method = "first")

  preds_mat <- apply(tune_grid[top_10p_selection, ], 1, function(tg) {
    rf_model <- randomForest::randomForest(
      num ~ ., learn_dat, mtry = tg[1], ntree = tg[2], replace = tg[3])
    preds <- predict(rf_model, newdata = eval_dat, type = "class")
    return(preds)})

  eval_labs <- eval_dat[, "num"]
  similar_mat <- (preds_mat == eval_labs) * 1.0
  accs <- colMeans(similar_mat)
  best_eval_model_id <- which.max(accs)
  eval_results <- data.frame(
    eval_acc = accs, eval_rank = rank(1-accs, ties.method = "first"))
  result_mat <- cbind(learn_results, eval_results)
  result_mat$delta_rank <- result_mat$learn_rank - result_mat$eval_rank
  # result_mat

  source("5-example/MabtCi-function.R")

  # bootstrap
  eval_boot <- boot::boot(
    similar_mat, function(x, i) colMeans(x[i, ]), R = 10000)
  alpha <- 0.05
  measure <- "class"
  cls_thresh <- 0.5

  mabt_ci <- MabtCi(eval_boot, alpha, measure, best_eval_model_id,
                    preds_mat, cls_thresh, eval_labs)

  n_test <- nrow(preds_mat)
  n_success <- accs[which(best_learn_model_id == top_10p_selection)] * n_test
  default_cis <- DescTools::BinomCI(
    n_success, n_test, conf.level = 1-alpha,
    sides = "left", method = c("wilson", "clopper-pearson", "wald"))

  rt_obj <- list(mat = result_mat, default = default_cis, mabt = mabt_ci)
  return(rt_obj)
}

# par_clust <- parallel::makeCluster(n_cores)
# doParallel::registerDoParallel(par_clust)
# seeds <- seq(n_runs)
# t0 <- Sys.time()
# result_list <- foreach::foreach(seed = seeds, .packages = c("dplyr")) %dopar% {
#   return(HeartExampleAccuracy(seed))
# }
# t1 <- Sys.time()
# t1-t0
# parallel::stopCluster(par_clust)
# saveRDS(result_list, "5-example/heart-example-accuracy-sim-results.RDS")

## runtime = 5.5 mins


# Comparison of results ----
result_list <- readRDS("5-example/heart-example-accuracy-sim-results.RDS")
comp_results <- GenCompTable(result_list)
comp_results$table
do.call(rbind, comp_results$ci) %>% aggregate(ci ~ method, ., summary)

HeartExampleAccuracy(28)


