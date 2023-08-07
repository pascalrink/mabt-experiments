TuneRangerSim <- function(seed, n_obs) {
  
  set.seed(seed)
  
  n_learn <- 0.75*n_obs
  n_test <- 0.25*n_obs
  
  n_obs <- c(learn = n_learn, test = n_test, groundtruth = 20000)
  intcpt <- -5  ## default, leads to balanced classes
  n_noise <- 500
  n_corr <- 485
  corr_type <- "exch"
  corr_value <- 0.8
  mislab_frac <- 0.01
  
  dat <- lapply(n_obs, function(.n_obs) {
    .dat <- caret::twoClassSim(
      .n_obs, intercept = intcpt, noiseVars = n_noise, corrVars = n_corr,
      corrType = corr_type, corrValue = corr_value, mislabel = mislab_frac)
    .dat$Class <- factor((.dat$Class == "Class1") * 1.0)
    return(.dat)})
  
  classif_task <- mlr::makeClassifTask(data = dat$learn, target = "Class")
  rf_tune <- tuneRanger::tuneRanger(
    classif_task, measure = list(acc), build.final.model = FALSE)
  tune_results <- rf_tune$results
  tune_results <- tune_results[
    order(tune_results$exec.time, decreasing = FALSE), ]
  rownames(tune_results) <- NULL
  
  learn_perf <- tune_results$acc
  preselect_ids <- which(learn_perf >= quantile(learn_perf, 0.9))
  preselect_tune <- tune_results[preselect_ids, ]
  
  rf_models <- apply(preselect_tune, 1, function(.tune) {
    rf_model <- ranger::ranger(
      Class ~ ., dat$learn, num.trees = 1000, mtry = .tune["mtry"],
      min.node.size = .tune["min.node.size"],
      sample.fraction = .tune["sample.fraction"])
    return(rf_model)
  })
  
  rf_preds <- lapply(rf_models, function(.rf_model) {
    return(predict(.rf_model, dat$test)$predictions)
  }) %>% simplify2array
  
  test_similar <- (rf_preds == dat$test$Class) * 1.0
  test_perf <- colMeans(test_similar)
  
  select_res <- list(
    default = list(id = which.max(preselect_tune$acc),
                   est = as.numeric(test_perf[which.max(preselect_tune$acc)])),
    proposed = list(id = as.numeric(which.max(test_perf)),
                    est = max(test_perf)))
  
  test_boot <- boot::boot(
    test_similar, function(x, i) colMeans(x[i, ]), R = 10000)
  mabt_ci_est <- MabtCi(test_boot, 0.05, "class", 
                        select_res$proposed$id, rf_preds, 0.5, dat$test$Class)
  mabt_ci <- data.frame(method = "mabt", lwr = mabt_ci_est)
  
  select_res$proposed$ci <- mabt_ci
  
  n_test <- nrow(dat$test)
  n_succ <- select_res$default$est * n_test
  default_ci_methods <- c("wilson", "wald", "clopper-pearson")
  default_ci_est <- DescTools::BinomCI(
    n_succ, n_test, conf.level = 0.95, sides = "left",
    method = default_ci_methods)[, 2]
  default_ci <- data.frame(method = default_ci_methods, 
                           lwr = unname(default_ci_est))
  
  select_res$default$ci <- default_ci
  
  n_succ_sidak <- select_res$proposed$est * n_test
  sidak_ci_methods <- default_ci_methods
  sidak_level <- (1-0.05)^(1/length(preselect_ids))
  sidak_ci_est <- DescTools::BinomCI(
    n_succ_sidak, n_test, conf.level = sidak_level, sides = "left",
    method = sidak_ci_methods)[, 2]
  sidak_ci <- data.frame(method = paste0(sidak_ci_methods, "_sidak"), 
                         lwr = unname(sidak_ci_est))
  
  gt_preds <- lapply(select_res, function(.lelem) {
    return(predict(rf_models[[.lelem$id]], dat$groundtruth)$predictions)
  }) %>% simplify2array
  gt_perf <- colMeans(gt_preds == dat$groundtruth$Class)
  
  select_res$default$gt <- gt_perf[names(gt_perf) == "default"] %>% as.numeric
  select_res$proposed$gt <- gt_perf[names(gt_perf) == "proposed"] %>% as.numeric
  
  select_res$sidak <- list(id = select_res$proposed$id,
                           est = select_res$proposed$est,
                           ci = sidak_ci,
                           gt = select_res$proposed$gt)
  
  return(select_res)
}