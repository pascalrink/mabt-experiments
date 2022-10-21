
StandardBounds <- function(inst, rule, alpha) {
  select <- inst$select
  params <- inst$params
  
  n_obs <- params$n_eval
  n_preselected <- length(select$valid[[rule]]$idx)
  alpha <- 1 - (1-alpha)^(1/n_preselected)
  
  select_idx <- select$eval[[rule]]$idx
  select_groundtruth <- inst$performs$groundtruth[select_idx]
  select_perform <- select$eval[[rule]]$performs
  
  if (params$measure == "class") {
    n_success <- select_perform * n_obs
    
    methods <- c("wilson", "clopper-pearson", "wald")
    bounds <- DescTools::BinomCI(
      n_success, n_obs, conf.level = 1-alpha, sides = "left", 
      method = methods)[, 2] %>% unname
  }
  if (params$measure == "auc") {
    methods <- c("delong", "hanley-mcneal")
    bounds <- rep(NA, length(methods))
    
    delong_bounds <- pROC::ci.auc(
      inst$labs, inst$preds[, select_idx], conf.level = 1 - 2*alpha)[1]
    
    auc <- select$eval$best$performs
    q1 <- auc / (2 - auc)
    q2 <- 2 * auc^2 / (1 + auc)
    n_success <- ((inst$preds[, select_idx] > 0.5) == inst$labs) %>% sum 
    n_fail <- n_obs - n_success
    numerator <- auc * (1-auc) + (n_success + 1)*(q1 - auc^2) + 
      (n_fail -1)*(q2 - auc^2)
    denominator <- n_success * n_fail
    hanley_bounds <- auc - qnorm(1-alpha) * sqrt(numerator / denominator)
    
    bounds <- c(delong_bounds, hanley_bounds)
  }
  
  feasible <- (bounds < 1) * 1.0
  
  return(data.frame(
    method = methods, 
    rule = rule, 
    bound = bounds, 
    groundtruth = select_groundtruth, 
    est = select_perform, 
    alpha = alpha, 
    n_preselected = n_preselected, 
    feasible = feasible, 
    tau = NA, 
    p = NA, 
    min_p = NA, 
    max_p = NA, 
    alt = NA))
}


