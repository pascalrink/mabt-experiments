
rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
source("5-example/MabtCi-function.R")
source("5-example/TiltCi-function.R")

set.seed(2022)
dat <- read.csv("5-example/breast-cancer-wisconsin.csv") %>% data.matrix

alpha <- 0.05
n_models <- 100
n_folds <- 10
cls_thresh <- 0.5

feats <- dat[, -c(1, 11)]
labs <- as.factor(dat[, 11])
levels(labs) <- c(0, 1)
n_obs <- length(labs)

learn_ids <- sample(1:n_obs, 0.75 * n_obs)
eval_ids <- setdiff(1:n_obs, learn_ids)
n_eval <- length(eval_ids)

learn_feats <- feats[learn_ids, ]
learn_labs <- labs[learn_ids]

eval_feats <- feats[eval_ids, ]
eval_labs <- labs[eval_ids]

max_lambda <- glmnet::glmnet(learn_feats, learn_labs, family = "binomial")$lambda[1]
learn_models <- glmnet::cv.glmnet(
  learn_feats, learn_labs, family = "binomial", 
  lambda = seq(max_lambda, 0, length.out = n_models), type.measure = "class", 
  nfolds = n_folds)

valid_performs <- 1-learn_models$cvm
best_model <- which.max(valid_performs)
top_10p_models <- which(valid_performs >= quantile(valid_performs, 0.9))
within_1se_models <- which(valid_performs >= valid_performs[best_model] - 
                             learn_models$cvsd[best_model] / sqrt(n_folds))

eval_preds <- predict(
  learn_models, eval_feats, s = learn_models$lambda, type = "response")
eval_similar <- (eval_preds > cls_thresh) * 1.0 == eval_labs
eval_accs <- colMeans(eval_similar) %>% unname

# standard CIs for single best model ----
lower_bounds <- DescTools::BinomCI(
  sum(eval_similar[, best_model]), n_eval, conf.level = 1-alpha, 
  sides = "left", method = c("wald", "wilson", "clopper-pearson"))[, 2]
default_best_df <- data.frame(
  method = names(lower_bounds), bound = as.vector(lower_bounds))

# standard CIs for top 10% selection rule ----
sidak_alpha <- 1 - (1-alpha)^(1/length(top_10p_models))
final_model_10p <- eval_accs[top_10p_models] %>% which.max
lower_bounds <- DescTools::BinomCI(
  sum(eval_similar[, top_10p_models[final_model_10p]]), n_eval, 
  conf.level = 1-sidak_alpha, sides = "left", 
  method = c("wald", "wilson", "clopper-pearson"))[, 2]
default_10p_df <- data.frame(
  method = names(lower_bounds), bound = as.vector(lower_bounds))

# standard CIs for within 1 se selection rule ----
sidak_alpha <- 1 - (1-alpha)^(1/length(within_1se_models))
final_model_1se <- eval_accs[within_1se_models] %>% which.max
lower_bounds <- DescTools::BinomCI(
  sum(eval_similar[, within_1se_models[final_model_1se]]), n_eval, 
  conf.level = 1-sidak_alpha, sides = "left", 
  method = c("wald", "wilson", "clopper-pearson"))[, 2]
default_1se_df <- data.frame(
  method = names(lower_bounds), bound = as.vector(lower_bounds))

# MABT CIs for top 10% selection rule ----
accs_boot <- boot::boot(
  eval_similar[, top_10p_models], function(d, i) {
    as.matrix(d[i, ]) %>% colMeans}, 10000)
mabt_10p_df <- data.frame(
  method = "mabt", bound = MabtCi(
    accs_boot, alpha, "class", final_model_10p, eval_preds[, top_10p_models], 
    cls_thresh, eval_labs))

# MABT CIs for within 1 SE selection rule ----
accs_boot <- boot::boot(
  eval_similar[, within_1se_models], function(d, i) {
    as.matrix(d[i, ]) %>% colMeans}, 10000)
mabt_1se_df <- data.frame(
  method = "mabt", bound = MabtCi(
    accs_boot, alpha, "class", final_model_1se, eval_preds[, within_1se_models], 
    cls_thresh, eval_labs))

# BT CIs for single best model ----
bt_boot <- boot::boot(
  eval_similar[, best_model], function(d, i) {mean(d[i])}, R = 10000)
bt_best_df <- data.frame(
  method = "bt", bound = TiltCi(
    bt_boot, alpha, "class", eval_preds[, best_model], cls_thresh, eval_labs))

# BT CIs for top 10% selection rule ----
sidak_alpha <- 1 - (1-alpha)^(1/length(top_10p_models))
bt_boot <- boot::boot(
  eval_similar[, top_10p_models[final_model_10p]], 
  function(d, i) {mean(d[i])}, R = 10000)
bt_10p_df <- data.frame(
  method = "bt", bound = TiltCi(
    bt_boot, sidak_alpha, "class", eval_preds[, top_10p_models[final_model_10p]], 
    cls_thresh, eval_labs))

# BT CIs for within 1 SE selection rule ----
sidak_alpha <- 1 - (1-alpha)^(1/length(within_1se_models))
bt_boot <- boot::boot(
  eval_similar[, within_1se_models[final_model_1se]], 
  function(d, i) {mean(d[i])}, R = 10000)
bt_1se_df <- data.frame(
  method = "bt", bound = TiltCi(
    bt_boot, sidak_alpha, "class", eval_preds[, within_1se_models[final_model_1se]], 
    cls_thresh, eval_labs))

# Comparison of results ----
default_best_df
default_10p_df
default_1se_df

bt_best_df
bt_10p_df
bt_1se_df

mabt_10p_df
mabt_1se_df


# AUC ----

learn_models <- glmnet::cv.glmnet(
  learn_feats, learn_labs, family = "binomial", 
  lambda = seq(max_lambda, 0, length.out = n_models), type.measure = "auc", 
  nfolds = n_folds)

valid_performs <- learn_models$cvm
best_model <- which.max(valid_performs)
top_10p_models <- which(valid_performs >= quantile(valid_performs, 0.9))
within_1se_models <- which(valid_performs >= valid_performs[best_model] - 
                             learn_models$cvsd[best_model] / sqrt(n_folds))

eval_preds <- predict(
  learn_models, eval_feats, s = learn_models$lambda, type = "response")
eval_aucs <- apply(eval_preds, 2, function(.preds) {
  pROC::auc(c(eval_labs), .preds)}) %>% unname

# standard CIs for single best model ----
# DeLong
delong_bound <- pROC::ci.auc(
  eval_labs, eval_preds[, best_model], conf.level = 1 - 2*alpha)[1]
delong_best_df <- data.frame(method = "DeLong", bound = delong_bound)
# Hanley-McNeil
auc <- eval_aucs[best_model]
q1 <- auc / (2 - auc)
q2 <- 2 * auc^2 / (1 + auc)
n_success <- ((eval_preds[, best_model] > cls_thresh) * 1.0 == 
                eval_labs) %>% sum 
n_fail <- n_obs - n_success
numerator <- auc * (1-auc) + (n_success + 1)*(q1 - auc^2) + 
  (n_fail -1)*(q2 - auc^2)
denominator <- n_success * n_fail
hanley_bound <- auc - qnorm(1-alpha) * sqrt(numerator / denominator)
hanley_best_df <- data.frame(method = "Hanley-McNeil", bound = hanley_bound)

# standard CIs for top 10% selection rule ----
# DeLong
sidak_alpha <- 1 - (1-alpha)^(1/length(top_10p_models))
final_model_10p <- eval_aucs[top_10p_models] %>% which.max
delong_bound <- pROC::ci.auc(
  eval_labs, eval_preds[, top_10p_models[final_model_10p]], conf.level = 1 - 2*sidak_alpha)[1]
delong_10p_df <- data.frame(method = "DeLong", bound = delong_bound)
# Hanley-McNeil
auc <- eval_aucs[top_10p_models[final_model_10p]]
q1 <- auc / (2 - auc)
q2 <- 2 * auc^2 / (1 + auc)
n_success <- ((eval_preds[, top_10p_models[final_model_10p]] > cls_thresh) * 1.0 == 
                eval_labs) %>% sum 
n_fail <- n_obs - n_success
numerator <- auc * (1-auc) + (n_success + 1)*(q1 - auc^2) + 
  (n_fail -1)*(q2 - auc^2)
denominator <- n_success * n_fail
hanley_bound <- auc - qnorm(1-alpha) * sqrt(numerator / denominator)
hanley_10p_df <- data.frame(method = "Hanley-McNeil", bound = hanley_bound)

# standard CIs for within 1 SE selection rule ----
# DeLong
sidak_alpha <- 1 - (1-alpha)^(1/length(within_1se_models))
final_model_1se <- eval_aucs[within_1se_models] %>% which.max
delong_bound <- pROC::ci.auc(
  eval_labs, eval_preds[, within_1se_models[final_model_1se]], conf.level = 1 - 2*alpha)[1]
delong_1se_df <- data.frame(method = "DeLong", bound = delong_bound)
# Hanley-McNeil
auc <- eval_aucs[within_1se_models[final_model_1se]]
q1 <- auc / (2 - auc)
q2 <- 2 * auc^2 / (1 + auc)
n_success <- ((eval_preds[, within_1se_models[final_model_1se]] > cls_thresh) * 1.0 == 
                eval_labs) %>% sum 
n_fail <- n_obs - n_success
numerator <- auc * (1-auc) + (n_success + 1)*(q1 - auc^2) + 
  (n_fail -1)*(q2 - auc^2)
denominator <- n_success * n_fail
hanley_bound <- auc - qnorm(1-alpha) * sqrt(numerator / denominator)
hanley_1se_df <- data.frame(method = "Hanley-McNeil", bound = hanley_bound)

# MABT CIs for top 10% selection rule ----
aucs_boot <- boot::boot(
  eval_preds[, top_10p_models], function(d, i) {
    as.matrix(d[i, ]) %>% 
      apply(2, function(.preds) pROC::auc(eval_labs[i], .preds))}, 2000)
mabt_10p_df <- data.frame(
  method = "mabt", bound = MabtCi(
    aucs_boot, alpha, "auc", final_model_10p, eval_preds[, top_10p_models], 
    cls_thresh, eval_labs))

# MABT CIs for within 1 se selection rule ----
aucs_boot <- boot::boot(
  eval_preds[, within_1se_models], function(d, i) {
    as.matrix(d[i, ]) %>% 
      apply(2, function(.preds) pROC::auc(eval_labs[i], .preds))}, 2000)
mabt_1se_df <- data.frame(
  method = "mabt", bound = MabtCi(
    aucs_boot, alpha, "auc", final_model_1se, eval_preds[, within_1se_models], 
    cls_thresh, eval_labs))

# BT CIs for single best model ----
bt_boot <- boot::boot(
  eval_preds[, best_model], function(d, i) {pROC::auc(eval_labs[i], d[i])}, R = 2000)
bt_best_df <- data.frame(
  method = "bt", bound = TiltCi(
    bt_boot, alpha, "auc", eval_preds[, best_model], cls_thresh, eval_labs))

# BT CIs for top 10% selection rule ----
sidak_alpha <- 1 - (1-alpha)^(1/length(top_10p_models))
bt_boot <- boot::boot(
  eval_preds[, top_10p_models[final_model_10p]], 
  function(d, i) {pROC::auc(eval_labs[i], d[i])}, R = 2000)
bt_10p_df <- data.frame(
  method = "bt", bound = TiltCi(
    bt_boot, sidak_alpha, "auc", eval_preds[, top_10p_models[final_model_10p]], 
    cls_thresh, eval_labs))

# BT CIs for within 1 SE selection rule ----
sidak_alpha <- 1 - (1-alpha)^(1/length(within_1se_models))
bt_boot <- boot::boot(
  eval_preds[, within_1se_models[final_model_1se]], 
  function(d, i) {pROC::auc(eval_labs[i], d[i])}, R = 2000)
bt_1se_df <- data.frame(
  method = "bt", bound = TiltCi(
    bt_boot, sidak_alpha, "auc", eval_preds[, within_1se_models[final_model_1se]], 
    cls_thresh, eval_labs))

# Comparison of results ----
delong_best_df
hanley_best_df
bt_best_df

delong_10p_df
hanley_10p_df
bt_10p_df

delong_1se_df
hanley_1se_df
bt_1se_df

mabt_10p_df
mabt_1se_df
