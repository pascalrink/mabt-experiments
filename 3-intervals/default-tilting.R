
DefaultTiltBound <- function(inst, boot, rule, alpha, mabt = FALSE) {
  
  select <- inst$select
  params <- inst$params
  preselect <- select$valid[[rule]]$idx
  n_preselected <- length(preselect)
  alpha <- 1 - (1-alpha)^(1/n_preselected)
  
  select_idx <- select$eval[[rule]]$idx
  select_groundtruth <- inst$performs$groundtruth[select_idx]
  measure <- params$measure
  
  labs <- inst$labs
  
  t0 <- inst$select$eval[[rule]]$performs
  t <- boot$t[, select_idx]
  
  select_preds <- inst$preds[, select_idx]
  if (measure == "class") {
    select_preds <- ((select_preds > params$cl_thresh) * 1.0 == labs) * 1.0
  }
  
  tau_range <- switch(measure, class = c(-10, 0), auc = c(-20, 0))
  
  .EstPval <- function(.tau) {
    if (is.na(.tau)) {
      p <- NA
      tilt_weights <- NA
    } else {
      eivs <- switch(measure, 
                     class = select_preds, 
                     auc = boot::empinf(boot, index = select_idx)) %>% as.matrix
      tilt_weights <- boot::exp.tilt(eivs, lambda = .tau * nrow(eivs))$p
      imp_weights <- rep(tilt_weights * params$n_eval, boot$R)^t(boot::boot.array(boot)) %>% apply(2, prod)
      p <- (sum(imp_weights * (t > t0)) + 
              sum(imp_weights * (t == t0)) / 2) / length(t)
    }
    return(list(
      tau = .tau, p = p, weights = tilt_weights, success = !is.na(p) * 1.0))
  }
  
  .CalibTau <- function(.tau) {
    p <- .EstPval(.tau)$p
    obj <- (p - alpha) + 1000 * (p > alpha)
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
        bound_boot <- boot$data[, select_idx] %>% 
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
  while (min_p > alpha / 2 & min_tau > tau_range[1]) {
    min_tau <- min_tau - 1
    min_p <- .EstPval(min_tau)$p
  }
  tau_range[1] <- min_tau
  
  max_p <- .EstPval(tau_range[2])$p
  feasible <- ifelse(min_p < alpha / 2 & max_p > 2*alpha, 1, 0)
  
  tau <- ifelse(feasible, stats::uniroot(.CalibTau, tau_range)$root, NA)
  tilt <- .EstPval(tau)
  bound <- .EstBound(tilt)
  alt_method <- NA
  
  if (sd(select_preds) == 0) {
    alt_bounds <- StandardBounds(inst, rule, alpha)
    alt_method <- switch(measure, class = "wilson", auc = "delong")
    alt_result <- alt_bounds[which(alt_bounds$method == alt_method), ]
    
    bound <- alt_result$bound
    alpha <- alt_result$alpha
  }
  
  return(data.frame(method = ifelse(mabt, "mabt", "bt"), 
                    rule = rule, 
                    bound = bound, 
                    groundtruth = select_groundtruth, 
                    est = t0, 
                    alpha = alpha, 
                    n_preselected = n_preselected, 
                    feasible = feasible, 
                    tau = tau, 
                    p = tilt$p, 
                    min_p = min_p, 
                    max_p = max_p, 
                    alt = alt_method))
}
