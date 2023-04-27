
rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)

set.seed(28)
dat <- read.csv("5-example/processed.cleveland.data", header = FALSE)
col_names <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", 
               "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")
names(dat) <- col_names
dat$num <- (dat$num > 0) * 1.0
factor_cols <- which(
  col_names %in% c("sex", "cp", "fbs", "restecg", 
                   "exang", "slope", "thal", "num"))
dat[factor_cols] <- lapply(dat[factor_cols], factor)

learn_ids <- nrow(dat) %>% sample(2/3 * nrow(dat))
learn_dat <- dat[learn_ids, ]
eval_dat <- dat[-learn_ids, ]
folds <- caret::createFolds(learn_dat$num, list = FALSE)
tune_grid <- expand.grid(mtry = 1:5 * 2, ntree = 1:5 * 100, replace = c(T, F))

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
result_mat

source("5-example/MabtCi-function.R")

# bootstrap
eval_boot <- boot::boot(similar_mat, function(x, i) colMeans(x[i, ]), R = 10000)
alpha <- 0.05
measure <- "class"
cls_thresh <- 0.5

mabt_ci <- MabtCi(eval_boot, alpha, measure, best_eval_model_id, 
                  preds_mat, cls_thresh, eval_labs)

# standard cis

n_test <- nrow(preds_mat)
n_success <- accs[which(best_learn_model_id == top_10p_selection)] * n_test
default_cis <- DescTools::BinomCI(
  n_success, n_test, conf.level = 1-alpha, 
  sides = "left", method = c("wilson", "clopper-pearson", "wald"))

default_cis
mabt_ci

