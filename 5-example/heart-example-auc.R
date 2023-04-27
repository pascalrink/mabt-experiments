
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

auc_cv <- apply(tune_grid, 1, function(tg) {
  auc <- rep(NA, 10)
  for (fold in 1:10) {
    rf <- randomForest::randomForest(
      num ~ ., learn_dat[folds!=fold, ], 
      mtry = tg[1], ntree = tg[2], replace = tg[3])
    prd <- predict(rf, learn_dat[folds==fold, ], "prob")[, 2]
    auc[fold] <- pROC::auc(learn_dat[folds==fold, "num"], prd)
  }
  return(mean(auc))})

best_learn_model_id <- which.max(auc_cv)
top_10p_selection <- which(auc_cv >= quantile(auc_cv, 0.9))

learn_results <- data.frame(
  id = top_10p_selection, learn_auc = auc_cv[top_10p_selection])
learn_results$learn_rank <- rank(
  1-learn_results$learn_auc, ties.method = "first")

preds_mat <- apply(tune_grid[top_10p_selection, ], 1, function(tg) {
  rf_model <- randomForest::randomForest(
    num ~ ., learn_dat, mtry = tg[1], ntree = tg[2], replace = tg[3])
  preds <- predict(rf_model, newdata = eval_dat, type = "prob")[, 2]
  return(preds)})

eval_labs <- eval_dat[, "num"]
aucs <- apply(
  preds_mat, 2, function(.preds) pROC::auc(eval_labs, .preds))
best_eval_model_id <- which.max(aucs)
eval_results <- data.frame(
  eval_auc = aucs, eval_rank = rank(1-aucs, ties.method = "first"))
result_mat <- cbind(learn_results, eval_results)
result_mat$delta_rank <- result_mat$learn_rank - result_mat$eval_rank
result_mat

source("5-example/MabtCi-function.R")

# MABT CI
aucs_boot <- boot::boot(
  preds_mat, function(d, i) {as.matrix(d[i, ]) %>% 
      apply(2, function(.preds) pROC::auc(eval_labs[i], .preds))}, 2000)
alpha <- 0.05
measure <- "prob"
cls_thresh <- 0.5
mabt_ci <- MabtCi(
  aucs_boot, alpha, "auc", best_eval_model_id, preds_mat, cls_thresh, eval_labs)

# standard CIs for single best model
best_learn_model_id_relative <- which(best_learn_model_id == top_10p_selection)
# DeLong
delong_ci <- pROC::ci.auc(
  eval_labs, preds_mat[, best_learn_model_id_relative], 
  conf.level = 1 - 2*alpha)[1]
# Hanley-McNeil
auc <- aucs[best_learn_model_id_relative]
q1 <- auc / (2 - auc)
q2 <- 2 * auc^2 / (1 + auc)
n_success <- ((preds_mat[, best_learn_model_id_relative] > cls_thresh) * 1.0 == 
                eval_labs) %>% sum 
n_fail <- nrow(eval_dat) - n_success
numerator <- auc * (1-auc) + (n_success + 1)*(q1 - auc^2) + 
  (n_fail -1)*(q2 - auc^2)
denominator <- n_success * n_fail
hanley_ci <- auc - qnorm(1-alpha) * sqrt(numerator / denominator)


mabt_ci
delong_ci
hanley_ci



