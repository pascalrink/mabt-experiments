
rm(list = ls())
graphics.off()

library(doParallel)
library(dplyr)

set.seed(123)

n_learn <- 360
n_feats <- 1000

n_active_feats <- n_feats * 0.1
true_coeffs <- c(rep(1, n_active_feats), rep(0, n_feats - n_active_feats))

learn_feats <- rnorm(n_learn * n_feats) %>% matrix(ncol = n_feats)
learn_labs <- (boot::inv.logit(
  learn_feats %*% true_coeffs) > runif(n_learn)) * 1.0

n_models <- 100
n_folds <- 10

alpha <- 0.05

n_boot_reps <- 10000

learn_cv_models <- glmnet::cv.glmnet(
  learn_feats, learn_labs, family = "binomial", nlambda = n_models, 
  type.measure = "class", nfolds = n_folds)
learn_cv_accs <- 1-learn_cv_models$cvm

lambdas <- learn_cv_models$lambda
learn_best_lambda <- learn_cv_models$lambda.min
learn_best_index <- learn_cv_models$index[
  rownames(learn_cv_models$index) == "min"]
learn_best_acc <- learn_cv_accs[learn_best_index]
preselected_index <- which(
  learn_cv_accs >= learn_best_acc - learn_cv_models$cvsd[learn_best_index] / 
    sqrt(n_folds))
preselected_lambdas <- lambdas[preselected_index]

n_eval <- 40

n_gt <- 20000
gt_feats <- rnorm(n_gt * n_feats) %>% matrix(ncol = n_feats)
gt_labs <- (boot::inv.logit(
  gt_feats %*% true_coeffs) > runif(n_gt)) %>% as.numeric

gt_learn_preds <- ((predict(
  learn_cv_models, newx = gt_feats, s = preselected_lambdas, 
  type = "response") > 0.5) * 1.0)
gt_learn_accs <- (gt_learn_preds == gt_labs) %>% colMeans

n_runs <- 100000
n_cores <- 60

BBCCV <- function(sample_size, predictions, num_bootstraps_BBC_CV) {
  
  out_samples_performances = rep(NA, num_bootstraps_BBC_CV)
  boot_out = rep(NA, num_bootstraps_BBC_CV)
  
  for (current_bootstrap in 1:num_bootstraps_BBC_CV) {
    in_samples_ids <- sample(sample_size, sample_size, replace = TRUE)
    in_samples <- predictions[in_samples_ids, ]
    out_samples <- predictions[-in_samples_ids, ]
    
    best_index <- colMeans(in_samples) %>% which.max
    out_samples_perf <- mean(out_samples[, best_index])
    
    out_samples_performances[current_bootstrap] <- out_samples_perf
    boot_out[current_bootstrap] <- max(colMeans(out_samples)) - out_samples_perf
  }
  
  corrected_performance <- mean(out_samples_performances)
  test_boot <- mean(boot_out)
  
  return(list(corrected_performance = corrected_performance, 
              out_samples_performances = out_samples_performances, 
              test_boot = test_boot))
}

.GlmnetCVFold <- function(i, feats, labs, folds, lambdas) {
  train_feats <- feats[folds != i, ] %>% as.matrix
  train_labs  <- labs[ folds != i  ] %>% as.vector
  
  test_feats <- feats[folds == i, ] %>% as.matrix
  test_labs  <- labs[ folds == i] %>% as.vector
  
  fit <- glmnet::glmnet(
    train_feats, train_labs, family = "binomial", lambda = lambdas, 
    type.measure = "class")
  test_preds <- predict(fit, test_feats, s = lambdas, type = "class")
  similar_mat <- (test_preds == test_labs) * 1.0
  accs <- colMeans(similar_mat)
  
  return(list(similar_mat = similar_mat, accs = accs))
  
}

GlmnetCV <- function(feats, labs, n_folds, n_models, lambdas) {
  folds <- caret::createFolds(labs, k = n_folds, list = FALSE)
  cv_out <- lapply(
    1:n_folds, .GlmnetCVFold, feats, labs, folds, lambdas)
  similar_mat <- lapply(
    cv_out, function(lst) lst$similar_mat) %>% do.call(rbind, .)
  accs <- lapply(
    cv_out, function(lst) lst$accs) %>% do.call(rbind, .) %>% colMeans
  
  one_minus_acc <- 1-accs
  min_index <- which.min(one_minus_acc)
  lambda_min <- lambdas[min_index]
  glmnet_fit <- glmnet::glmnet(
    x = feats, y = labs, family = "binomial", lambda = lambdas, 
    type.measure = "class")
  
  return(list(
    lambda = lambdas, cvm = one_minus_acc, glmnet.fit = glmnet_fit, 
    index = min_index, lambda.min = lambda_min, similar_mat = similar_mat))
}

LassoFitter <- function(feats, labs, ids = NA, funcs_params = NA) {
  if(sum(is.na(ids)) > 0) {ids <- 1:nrow(feats)}
  fit <- glmnet::glmnet(feats[ids, ], labs[ids], lambda = funcs_params$lambdas) 
  return(fit)
}

LassoPredictor <- function(fit, feats, funcs_params = NA) {
  probs <- predict(
    fit, feats, s = funcs_params$best_lambda, type.measure = "class")
  preds <- (probs > 0.5) * 1.0
  return(preds)
}

AccuracyLoss <- function(preds, labs, funcs_params = NA) {
  return(preds != labs)
}

lasso_funcs <- list(
  fitter = LassoFitter, predictor = LassoPredictor, 
  loss = AccuracyLoss, name = "lasso")

BootColMeans <- function(d, i) {
  means <- as.matrix(d[i, ]) %>% colMeans
  return(means)
}

MabtCi <- function(
    boot, # bootstrap object as from the boot::boot function
    alpha, # significance level
    measure, # either 'class' for prediction accuracy or 'auc'
    select_id, # row# of final selected model in 'preds'
    preds, # matrix of class predictions
    cls_thresh, # classification probability threshold, e.g. 0.5
    labs # true class labels
) {
  
  unif_transfd <- copula::pobs(boot$t, ties.method = "max")
  .MaxEcdf <- unif_transfd %>% apply(1, max) %>% (stats::ecdf)
  
  t0 <- boot$t0[select_id]
  t <- boot$t[, select_id]
  
  select_preds <- preds[, select_id]
  if (measure == "class") {
    if (data.table::between(select_preds, 0, 1, incbounds = FALSE) %>% min) {
      select_preds <- ((select_preds > cls_thresh) * 1.0 == labs) * 1.0
    } else {
      select_preds <- (select_preds == labs) * 1.0
    }
  }
  
  tau_range <- switch(measure, class = c(-10, 0), auc = c(-20, 0))
  
  .EstPval <- function(.tau) {
    if (is.na(.tau)) {
      p <- NA
      tilt_weights <- NA
    } else {
      eivs <- switch(measure, 
                     class = select_preds, 
                     auc = boot::empinf(boot, index = select_id)) %>% as.matrix
      tilt_weights <- boot::exp.tilt(eivs, lambda = .tau * nrow(eivs))$p
      imp_weights <- rep(
        tilt_weights * length(select_preds), boot$R)^t(boot::boot.array(boot)) %>% 
        apply(2, prod)
      tilt_perform <- spatstat.univar::ewcdf(t, imp_weights)(t0)
      p <- 1 - .MaxEcdf(tilt_perform)
    }
    return(list(
      tau = .tau, p = p, weights = tilt_weights, success = !is.na(p) * 1.0))
  }
  
  .CalibTau <- function(.tau) {
    p <- .EstPval(.tau)$p
    obj <- (p - alpha) + 1000 * (p > alpha)  # conservative estimation
    return(obj)
  }
  
  .EstBound <- function(.tilt) {
    if (! .tilt$success) {
      bound <- NA
    } else {
      .weights <- .tilt$weights
      if (measure == "class") {
        bound <- weighted.mean(select_preds, .weights)
      }
      if (measure == "auc") {
        bound_boot <- boot$data[, select_id] %>% 
          boot::boot(function(d, i) {
            pROC::auc(labs[i], d[i])}, length(t), stype = "i", 
            strata = labs, weights = .weights)
        bound <- mean(bound_boot$t)
      }
    }
    return(bound)
  }
  
  min_tau <- 0  
  min_p <- 1
  while (min_p > alpha/2 & min_tau > tau_range[1]) {
    min_tau <- min_tau - 0.1
    min_p <- .EstPval(min_tau)$p
  }
  tau_range[1] <- min_tau
  
  max_p <- .EstPval(tau_range[2])$p
  feasible <- ifelse(min_p < alpha/2 & max_p > 2*alpha, 1, 0)
  
  tau <- ifelse(feasible, stats::uniroot(.CalibTau, tau_range)$root, NA)
  tilt <- .EstPval(tau)
  bound <- .EstBound(tilt)
  
  return(bound)
}

par_clust <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(par_clust)

time_0 <- Sys.time()

sim_out <- foreach::foreach(
  run = 1:n_runs, .packages = c("dplyr", "glmnet")) %dopar% {
    
    eval_feats <- rnorm(n_eval * n_feats) %>% matrix(ncol = n_feats)
    eval_labs <- (boot::inv.logit(
      eval_feats %*% true_coeffs) > runif(n_eval)) %>% as.numeric
    
    eval_preds <- (predict(
      learn_cv_models, newx = eval_feats, s = preselected_lambdas,
      type = "response") > 0.5) * 1.0
    eval_similar_mat <- (eval_preds == eval_labs) * 1.0
    eval_accs <- colMeans(eval_similar_mat)
    eval_select_index <- which.max(eval_accs)
    eval_select_lambda <- preselected_lambdas[eval_select_index]
    
    boot <- boot::boot(eval_similar_mat, BootColMeans, n_boot_reps)
    mabt_bound <- MabtCi(
      boot, alpha = alpha, measure = "class", select_id = eval_select_index,
      preds = eval_preds, cls_thresh = 0.5, labs = eval_labs)
    mabt_select_gt_acc <- gt_learn_accs[eval_select_index]
    mabt_covers <- (mabt_bound < gt_learn_accs[eval_select_index])
    
    mabt_df <- data.frame(
      method = "MABT", lwr_bound = mabt_bound, 
      true_perform = mabt_select_gt_acc, lambda = eval_select_lambda,
      alpha = alpha, covers = mabt_covers)
    
    feats <- rbind(learn_feats, eval_feats)
    labs <- c(learn_labs, eval_labs)
    
    cv_models <- GlmnetCV(feats, labs, n_folds, n_models, lambdas)
    best_lambda <- cv_models$lambda.min
    beta_vect <- glmnet::coef.glmnet(
      cv_models$glmnet.fit, s = best_lambda) %>% as.numeric()
    
    bbccv <- length(labs) %>% BBCCV(cv_models$similar_mat, n_boot_reps)
    bbccv_bound <- bbccv$out_samples_performances %>% quantile(alpha)
    
    funcs_params = list("lambdas" = lambdas, "best_lambda" = best_lambda)
    
    ncv <- nestedcv::nested_cv(
      X = feats, Y = labs, funcs = lasso_funcs, funcs_params = funcs_params,
      n_folds = n_folds, alpha = 2*alpha)
    ncv_bound <- 1-ncv$ci_hi
    
    gt_preds <- (predict(
      cv_models$glmnet.fit, newx = gt_feats, s = best_lambda, 
      type = "response") > 0.5) * 1.0
    gt_acc <- mean(gt_preds == gt_labs)
    
    ncv_covers <- (ncv_bound < gt_acc) * 1.0
    bbccv_covers <- (bbccv_bound < gt_acc) * 1.0
    
    ncv_df <- data.frame(
      method = "NCV", lwr_bound = ncv_bound, true_perform = gt_acc, 
      lambda = best_lambda, alpha = alpha, covers = ncv_covers)
    bbccv_df <- data.frame(
      method = "BBC-CV", lwr_bound = bbccv_bound, true_perform = gt_acc, 
      lambda = best_lambda, alpha = alpha, covers = bbccv_covers)
    
    df <- rbind(mabt_df, ncv_df, bbccv_df)
    rownames(df) <- NULL
    
    out_list <- list(df = df, 
                     beta_vect = beta_vect)
    # paste0(
    #   "6-additions/conditional-coverage/ccs-interm/ccs-", run, ".RDS") %>%
    #   saveRDS(out_list, .)
    
    return(out_list)
  }

saveRDS(sim_out, "6-additions/conditional-coverage/ccs-results.RDS")
time_1 <- Sys.time()

parallel::stopCluster(par_clust)
time_1 - time_0
####
